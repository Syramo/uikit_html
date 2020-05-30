-module (ztree).

-export ([root/1,add_child/2,add_children/2,append/2,prepend/2,delete/1]).
-export ([first_child/1,last_child/1,prev/1,next/1,parent/1,select/2]).
-export ([value/1,children/1,path/1,map/2,map/3]).

-export_type ([ztree/0,treepath/0]).


-type zlist(A) :: {Left::list(A), Right::list(A)}.

-type znode()  :: {term(), children() | nil}. 
-type children() :: zlist(znode()).

-opaque ztree() :: {ancestors(), siblings(), treepath()}.
-type ancestors() :: zlist(znode()).
-type siblings() :: zlist(znode()).
-opaque treepath() :: [integer()].



%%----------manipulation of the tree--------------------------------------------


-spec root(term()) -> ztree().

root(Val) -> {[], {[], [{Val, {[],[]}}]}, [1]}.



-spec add_child (ztree(),term()) -> ztree().									% a child is always appended to the end

add_child ({Ac, {Sl, [{Val, {L, R}}|Rt]}, Pt}, Child) ->
	N = {Val, {lists:reverse(R) ++ L, [{Child, {[],[]}}]}},
	{Ac, {Sl, [N|Rt]}, Pt}.
	

-spec add_children (ztree(), C::[term()]) -> ztree().							% add multiple children to the current node

add_children ([], T) -> T;

add_children ({Ac, {Ls, [{Val, {Lc, Rc}}|Rs]}, P}, Children) ->
	N = {Val, {lists:reverse(Rc) ++ Lc, [{Child, {[],[]}} || Child <- Children]}},
	{Ac, {Ls, [N|Rs]}, P}.
	


-spec append (ztree(),term()) -> ztree() | {error, Reason::string()}.			% append adds a node (Value) after the current node
																				% and makes it active
append ({[], _, _}, _) ->
	{error, "root can't have siblings"};

append ({Ac, {Sl, [C|Rt]}, [Pc|Pa]}, Val) ->
	{Ac, {[C|Sl], [{Val, {[],[]}}|Rt]}, [Pc+1|Pa]}.


-spec prepend (ztree(), term()) -> ztree() | {error, Reason::string()}.			% prepend inserts a node (Value) before the current node
																				% and makes it active
prepend ({[], _, _},_) ->
	{error, "root can't have siblings"};

prepend ({Ac, {Ls, Rs}, Pt}, Val) ->
	{Ac, {Ls, [{Val, {[],[]}}|Rs]}, Pt}.
	
	

-spec delete (ztree()) -> ztree() | {error, Reason::string()}.					% deletes the current node and all its children
																				% the sibling to the right (or if the last one then the sibling to the left) becomes active
delete ({[], _, _}) ->															% if no siblings left, the parent becomes active
	{error, "can't delete the root"};
	
delete ({Ac, {[], [_|[]]}, Pt}) ->
	parent({Ac, {[],[]}, Pt});
	
delete ({Ac, {[Cs|Ls], [_|[]]}, [Ph|Pt]}) ->
	{Ac, {Ls,[Cs]}, [Ph-1|Pt]};
	
delete ({Ac, {Ls, [_|Rs]}, Pt}) ->
	{Ac, {Ls,Rs}, Pt}.





%%---------navigating through the tree------------------------------------------


-spec first_child (ztree()) -> ztree() | {error, Reason::string()}.				% descends down to the first child of a node in a tree

first_child ({_, {_, [{_,{[],[]}}|_]}, _}) ->
	{error, "leaf node"};

first_child ({Ac, {Ls, [{Val,{Lc, Rc}}|Rs]}, Pt}) ->
	{[{Ls,[{Val,nil}|Rs]}|Ac],{[],lists:reverse(Lc) ++ Rc},[1|Pt]}.



-spec last_child (ztree()) -> ztree() | {error, Reason::string()}.				% descends into the tree and selects the last child

last_child ({_, {_, [{_,{[],[]}}|_]}, _}) ->
	{error, "leaf node"};
	
last_child ({Ac, {Ls, [{Val,{Lc, Rc}}|Rs]}, Pt}) ->
	[H | T] = lists:reverse(Rc) ++ Lc,
	{[{Ls,[{Val,nil}|Rs]}|Ac],{T, [H]},[length(T)+1|Pt]}.	



-spec next (ztree()) -> ztree() | {error,  Reason::string()}.					% selects the next sibling of the tree node

next ({_, {_,[_]}, _}) ->
	{error, "last sibling"};
	
next ({Ac, {Ls, [Cs|Rs]}, [Ph|Pt]}) ->
	{Ac, {[Cs|Ls], Rs}, [Ph+1|Pt]}.
	
	
	
-spec prev (ztree()) -> ztree() | {error, Reason::string()}.					% selects the previous sibling of the tree node

prev ({_, {[],_}, _}) ->
	{error, "first sibling"};
	
prev ({Ac, {[Cs|Ls], [Rs]}, [Ph|Pt]}) ->
	{Ac, {Ls, [Cs|Rs]}, [Ph-1|Pt]}.



-spec parent (ztree()) -> ztree() | {error, Reason::string()}.					% move the tree up one level

parent ({[],_,_}) ->
	{error, "no parent"};
	
parent ({[{Ls,[{Val,nil}|Rst]}|Ac],Cd,[_|Pt]}) ->
	{Ac,{Ls,[{Val,Cd}|Rst]},Pt}.



-spec select (ztree(),treepath()) -> ztree() | {error, Reason::string()}.		% selects or sets a path within a tree

select (T = {_,_,TP}, P) ->
	{C,D} = prefix_split(P,lists:reverse(TP)),
	try go_down (go_up(T, length(TP)-length(C)),D) of
		{error, _} -> {error, "Invalid path"};
		T2 -> T2
	catch
		_:_ -> {error, "Invalid path"}
	end.
		
		




%%----------getting info from tree--------------------------------------------

-spec value(ztree()) -> term().													% the value is the value of the current element in the tree

value ({_, {_, [{Val, _}|_]}, _}) -> Val.



-spec children(ztree()) -> integer().											% number of children from the current element

children ({_, {_, [{_, {L, R}}|_]}, _}) -> length(L) + length(R).



-spec path(ztree()) -> treepath().												% get the path to the curent element in form of a list

path ({_, _, Pt}) -> lists:reverse(Pt).

	
	
-spec map (ztree(), fun((Val::term())->term())) -> [{treepath(),term()}]. 		% walks the tree down from current node 
																				% returns treepath and the evaluated result of fun as a tuple
map ({_, {_, [N|_]}, _}, F) ->													% NOTE: treepath is relative to the current znode!						
	lists:reverse([{lists:reverse(P),V} || {_,P,V} <- walk_tree(N,F,nop,[],[1])]). 
	
	

-spec map (ztree(), fun((X)->term())|nop, fun((X)->term())|nop) -> list().		% walks the tree same as map/2 but executes F1
																				% on entry of every node and F2 on exit of every node
map ({_, {_, [N|_]}, _}, F1, F2) ->												% either of the functions can be nop = no function will be executed 
	lists:reverse([{D,lists:reverse(P),V} || {D,P,V} <- walk_tree(N,F1,F2,[],[1])]).





%%---------helper functions-----------------------------------------------------

-spec prefix_split (L::list(A),R::list(A)) -> {Prefix::list(A),Rest::list(A)}.	% splits a list L into the longest common prefix with R and the non common tail of L

prefix_split (L, R) -> prefix_split (L,R,[]).

prefix_split (L, R, D) ->
	case lists:prefix(L,R) of
		true -> {L, D};
		false -> 
			[H|T] = lists:reverse(L),
			prefix_split(lists:reverse(T),R,[H|D])
	end.



-spec  go_up(ztree(), N::integer()) -> ztree().									% selects the parent N times up in the tree

go_up (T, N) when N > 0 -> go_up(parent(T),N-1);

go_up (T, _) -> T.



-spec go_down (P::treepath(), ztree()) -> ztree().								% ascend the tree following the treepath P

go_down (T,[]) -> T;

go_down (T,[C|R]) -> go_down(go_right(first_child(T),C-1),R).


-spec go_right (N::integer(),ztree()) -> ztree().								% move right in the siblings N times

go_right (T, N) when N > 0 -> go_right(next(T),N-1);

go_right (T, _) -> T.



-spec walk_tree (znode(), fun(), fun(), list(), treepath()) -> [{treepath(),term()}].	% walk through the tree down and right executes fun(Val) on every node

walk_tree ([],_,_,Acc,_) -> Acc;

walk_tree ({Val, {Lc,Rc}},F1,F2,Acc,P) ->
	case F1 of
		nop -> A = Acc;
		_ -> A = [{in,P,F1(Val)} | Acc]
	end,
	C = lists:reverse(Lc) ++ Rc,
	A2 = walk_siblings(C,F1,F2,A,[1|P]),
	case F2 of
		nop -> A2;
		_ -> [{out,P,F2(Val)} | A2]
	end.
	

walk_siblings ([],_,_,Acc,_) -> Acc;

walk_siblings ([N|S],F1,F2,Acc,P=[Ph|Pt]) ->
	A = walk_tree(N,F1,F2,Acc,P),
	walk_siblings(S,F1,F2,A,[Ph+1|Pt]). 

