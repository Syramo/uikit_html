-module (htdom).
-behaviour (gen_server).

-export ([start/1]).
-export ([init/1]).

-import (htelm,[t_html/1,t_head/0,t_body/0,t_meta/1,t_meta/2,t_script/1,t_link_css/1]).


-record (dom,{ids = #{} :: pathmap(), tree :: ztree:ztree(), etag :: integer()}).
-type pathmap() :: #{string() => ztree:treepath()}.




%%----------starting and stopping the dom server--------------------------------

-spec start(Args::list()) -> {ok, pid()} | {error, Reason::term()}.

start (Args) ->
	case gen_server:start_link(?MODULE,Args,[]) of
		{ok, Pid} -> {ok, Pid};
		{error, Reason} -> {error, Reason};
		_	-> {error, "startup failed"}
	end.

	
	
	
%%----------initialization------------------------------------------------------
	
init (Options) ->
	T1 = ztree:first_child(ztree:add_children(ztree:root(t_html(#{lang=>"en"})),[t_head(),t_body()])),
	T2 = ztree:add_children(T1,
			[t_meta(N,V) ||  B <- Options, size(B) == 3, {K,N,V} <- [B], K =:= meta, N =/= charset]),
	T3 = ztree:add_children(T2,
			[t_link_css(L) || B <- Options, size(B) == 2, {K,L} <-[B], K =:= style]),
	T4 = ztree:add_children(ztree:next(T3),
			[t_script(L) || B <- Options, size(B) == 2, {K,L} <-[B], K =:= script]),
	{ok, #dom{tree=T4, etag=erlang:system_time(millisecond)}}.
	
	
	
%----------API------------------------------------------------------------------