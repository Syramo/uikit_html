-module (ukdom).

-export ([create/0, create/1]).

-import (htelm,[t_html/1,t_head/0,t_body/0,t_meta/1,t_meta/2,t_script/1,t_link_css/1]).

-record (dom,{ids = #{} :: pathmap(), tree :: ztree:ztree()}).

-type pathmap() :: #{string() => ztree:treepath()}.
-type dom() :: #dom{}.

-type options() :: [{meta | style | script, atom(), string()}].

	

%%----------create an initial dom with one html element in it-------------------

-spec create() -> Dom :: dom().

create () -> create([]).



-spec create (options()) -> dom().												% creates the initial dom with meta and script
																				% tags in place
create (Options) ->
	T = ztree:first_child(ztree:add_children([t_head(),t_body()],ztree:root(t_html(#{lang=>"en"})))),
	T2 = ztree:add_children([t_meta(#{charset => "utf-8"}) | 
								[t_meta(N,V) ||  B <- Options, size(B) == 3, {K,N,V} <- [B], K =:= meta, N =/= charset]],T),
	T3 = ztree:add_children([t_link_css("css/uikit.css") | 
								[t_link_css(L) || B <- Options, size(B) ==2, {K,L} <-[B], K =:= style]],T2),
	T4 = ztree:add_children([t_script("js/uikit.js") | 
								[t_script(L) || B <- Options, size(B) ==2, {K,L} <-[B], K =:= script]],ztree:next(T3)),
	#dom{tree = T4}.
	