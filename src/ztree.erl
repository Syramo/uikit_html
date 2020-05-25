-module (ztree).

-export ([root/1,add_child/2,append/2,prepend/2,delete/1]).
-export ([first_child/1,last_child/1,prev/1,next/1,parent/1,select/2]).
-export ([value/1,children/1,path/1]).

-export_type ([ztree/0]).


-type zlist(A) :: {Left::list(A), Right::list(A)}.

-type znode()  :: zlist({term(), children() | nil}). 
-type children() :: zlist(znode()).

-opaque ztree() :: {ancestors(), siblings(), treepath()}.
-type ancestors() :: zlist(znode()).
-type siblings() :: zlist(znode()).
-type treepath() :: [integer()].



%%----------manipulation of the tree--------------------------------------------


-spec root(term()) -> ztree().

root(Val) -> {[], {[], [{Val, {[],[]}}]}, [1]}.



-spec add_child (term(),ztree()) -> ztree().									% a child is always appended to the end

add_child (Child, {Ac, {Sl, [{Val, {L, R}}|Rt]}, Pt}) ->
	N = {Val, {lists:reverse(R) ++ L, [{Child, {[],[]}}]}},
	{Ac, {Sl, [N|Rt]}, Pt}.
	


-spec append (term(),ztree()) -> ztree() | {error, Reason::string()}.			% append adds a node (Value) after the current node
																				% and makes it active
append (_, {[], _, _}) ->
	{error, "root can't have siblings"};

append (Val, {Ac, {Sl, [C|Rt]}, [Pc|Pa]}) ->
	{Ac, {[C|Sl], [{Val, {[],[]}}|Rt]}, [Pc+1|Pa]}.


-spec prepend (term(),ztree()) -> ztree() | {error, Reason::string()}.			% prepend inserts a node (Value) before the current node
																				% and makes it active
prepend (_, {[], _, _}) ->
	{error, "root can't have siblings"};

prepend (Val, {Ac, {Ls, Rs}, Pt}) ->
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



-spec select (treepath(),ztree()) -> ztree() | {error, Reason::string()}.		% selects or sets a path within a tree

select (P, T = {_,_,TP}) ->
	{C,D} = prefix_split(P,lists:reverse(TP)),
	try go_down (D, go_up(length(TP)-length(C),T)) of
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



-spec go_up (N::integer(),ztree()) -> ztree().									% selects the parent N times up in the tree

go_up (N, T) when N > 0 -> go_up(N-1,parent(T));

go_up (_, T) -> T.



-spec go_down (P::treepath(), ztree()) -> ztree().								% ascend the tree following the treepath P

go_down ([],T) -> T;

go_down ([C|R],T) -> go_down(R,go_right(C-1,first_child(T))).


-spec go_right (N::integer(),ztree()) -> ztree().								% move right in the siblings N times

go_right (N, T) when N > 0 -> go_right(N-1,next(T));

go_right (_, T) -> T.

