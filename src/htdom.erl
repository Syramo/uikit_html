-module (htdom).
-behaviour (gen_server).

-export ([create/1,discard/1]).
-export ([init/1,handle_call/3,handle_cast/2,terminate/2,handle_info/2]).
-export ([add_node/3,delete_node/1,graph/1,html/1,print/1]).

-import (htelm,[t_html/1,t_head/0,t_body/0,t_meta/1,t_meta/2,t_script/1,t_link_css/1]).


-record (dom,{ids = #{} :: pathmap(), tree :: ztree:ztree(), etag :: integer()}).
-type pathmap() :: #{string() => ztree:treepath()}.

-type dom_node() :: htelm:ht_elm() | htelm:vd_elm() | htelm:tx_elm().
-type dom() :: pid().
-type add_loc() :: append | prepend | child.
-type nav_loc() :: next | prev | parent | first_child | last_child.
-type err_reason() :: {error, Reason :: atom() | string()}.




%%----------starting and stopping the dom server--------------------------------

-spec create(Args::list()) -> {ok, dom()} | err_reason().

create (Args) ->
	case gen_server:start_link(?MODULE,Args,[]) of
		{ok, Pid} -> {ok, Pid};
		{error, Reason} -> {error, Reason};
		_	-> {error, "startup failed"}
	end.


-spec discard(dom()) -> ok.

discard (Dom) -> gen_server:cast(Dom,stop).
	
	
%%----------initialization/termination------------------------------------------
	
init (Options) ->
	T1 = ztree:first_child(ztree:add_children(ztree:root(t_html(#{lang=>"en"})),[t_head(),t_body()])),
	T2 = ztree:add_children(T1,
			[t_meta(N,V) ||  B <- Options, size(B) == 3, {K,N,V} <- [B], K =:= meta, N =/= charset]),
	T3 = ztree:add_children(T2,
			[t_link_css(L) || B <- Options, size(B) == 2, {K,L} <-[B], K =:= style]),
	T4 = ztree:add_children(ztree:next(T3),
			[t_script(L) || B <- Options, size(B) == 2, {K,L} <-[B], K =:= script]),
	{ok, #dom{tree=T4, etag=erlang:system_time(millisecond)}}.
	
	
terminate (_,_) -> ok.



	
%----------API------------------------------------------------------------------

-spec add_node(dom(), dom_node(), add_loc()) -> {ok, ztree:treepath()} |  err_reason(). 	% adds an htelm node to the dom tree
			
add_node (Dom, Node, child) ->
	case gen_server:call(Dom, {allow_child, nil}) of
		true -> gen_server:call(Dom,{add_child, Node});
		_ -> {error, "no soup for you"}
	end;
	
add_node (Dom, Node, append) -> gen_server:call(Dom, {append, Node});

add_node (Dom, Node, prepend) -> gen_server:call(Dom, {prepend, Node}).



-spec delete_node(dom()) -> {ok, ztree:treepath()} |  err_reason().				% deletes the current node from the tree

delete_node (Dom) -> gen_server:call(Dom, {delete, nil}).



-spec graph (dom()) -> {ok, list()} | err_reason().								% prints the whole dom tree

graph (Dom) -> gen_server:call(Dom, {graph, nil}).



-spec html (dom()) -> string().													% constructs html of the entire tree

html (Dom) -> 
	{ok, Html} = gen_server:call(Dom, {html, none}),
	lists:flatten(["<!DOCTYPE html>\n",Html]).


-spec print (dom()) -> ok.														% prints the pretty html to the console

print (Dom) ->
	{ok, Html} = gen_server:call(Dom, {html, pretty}),
	io:format("-----HTML---------->~n~s<-----------------",[Html]).


%----------Callbacks------------------------------------------------------------


handle_call ({allow_child, _}, _, State) -> allow_child(State);
	
handle_call ({add_child, Node}, _, State) -> add_child(State, Node);

handle_call ({append, Node}, _, State) -> append_node(State, Node);
	
handle_call ({prepend, Node}, _, State) -> prepend_node(State, Node);

handle_call ({delete, nil}, _, State) -> del_node(State);

handle_call ({graph, nil}, _, State) -> graph_tree(State);

handle_call ({html, Format}, _, State) -> html_out(Format,State);
	
handle_call (Msg, _, State) -> {reply, {error, io_lib:format("MSG: ~p => no soup for you",[Msg])}, State}.
	
	

handle_cast (stop, State) -> {stop, normal, State}.



handle_info ({'EXIT',_Pid, normal}, State) -> {noreply, State};

handle_info ({'EXIT', Pid, R}, State) -> 
	io:format ("Process: ~p terminated becaue of ~p~n",[Pid,R]),
	{noreply, State};
	
handle_info (_,State) -> {noreply, State}.


%%----------ztree manipulation--------------------------------------------------

allow_child (D = #dom{tree = T}) -> 
	{reply, htelm:allow_child(ztree:value(T)), D}.
	
	
	
add_child (D = #dom{tree = T, ids = I}, N) ->
	NT = ztree:add_child(T,N),
	NI = record_ID(I, N, [ztree:children(NT) | P=ztree:path(NT)]),
	{reply, {ok, P}, D#dom{ids=NI, tree=NT, etag=erlang:system_time(millisecond)}}.
	


append_node (D = #dom{tree = T, ids = I}, N) ->
	case NT = ztree:append(T,N) of
		{error, Reason} -> {reply, {error, Reason}, D};
		_	->	P = ztree:path(NT),
				{reply, {ok, P}, D#dom{ids=record_ID(I,N,P), tree=NT, etag=erlang:system_time(millisecond)}}
	end.	
	


prepend_node (D = #dom{tree = T, ids = I}, N) ->
	case NT = ztree:prepend(T,N) of
		{error, Reason} -> {reply, {error, Reason}, D};
		_	->	P = ztree:path(NT),
				{reply, {ok, P}, D#dom{ids=record_ID(I,N,P), tree=NT, etag=erlang:system_time(millisecond)}}
	end.	
	

	
del_node (D = #dom{tree = T, ids = I}) ->										% Note: we also want to remove all stored index keys of any sub elements
	P = ztree:path(T),
	SN = [P ++ PT || {[_|PT],N} <- ztree:map(T,fun(X) -> X end), htelm:get_id(N) /= []],
	T1 = ztree:delete(T),
	{reply, {ok, ztree:path(T1)}, D#dom{tree=T1, ids=maps:without(SN,I), etag=erlang:system_time(millisecond)}}.
	


graph_tree (D = #dom{tree = T}) ->
	T1 = ztree:select(T,[1]), 													% select the root node
	G = ztree:map(T1,fun(N) -> atom_to_list(htelm:get_name(N)) ++ "[" ++ htelm:get_id(N) ++ "]" end),
	{reply, {ok, G}, D}.
	
	
	
html_out (F, #dom{tree = T} = D) ->
	T1 = ztree:select(T,[1]),
	R = [html_indent(V,F) || V <- ztree:map(T1,fun(N) -> html_format(open,N,F) end,fun(N) -> html_format(close,N,F) end)],
	{reply, {ok, lists:flatten(R)}, D}.
	
	
html_indent ({_,P,Txt},pretty) ->
	[lists:duplicate(length(P)-1,"   "),Txt];
	
html_indent ({_,_,Txt},_) -> Txt.


html_format (Fn,N,pretty) ->
	case htelm:inline(N) of
		true -> htelm:Fn(N);
		_	 -> [htelm:Fn(N),"\n"]
	end;
	
html_format (Fn,N,_) -> htelm:Fn(N).
	
	
	
	
record_ID(Ids,{_,_,Id,_},P) when is_list(Id) andalso length(Id) > 0 ->
	Ids#{Id => P};
	
record_ID(Ids,_,_) ->
	Ids.
	
	