-module(ukhtml).

-export([uk_doc/1,uk_doc/2]).

-import(htelm,[document/3]).

-include("ukhtml.hrl").


%===============================================================================
% document creation

uk_doc (Body) ->
	uk_doc(Body, #meta{}).

uk_doc (Body, P) ->
	document([{lang,"en"}], t_head(P), Body).


t_head(P) when is_record(P,meta) ->
	iolist_to_binary(["\n<head>\n"
	,"<meta charset=\"utf-8\">\n"
	,t_meta(title,P#meta.title)
	,t_meta(viewport,"width=device-width, initial-scale=1")
	,"</head>\n"]).
	
t_meta(K,Val) ->
	lists:flatten(["<meta name=\"",atom_to_list(K),"\" content=\"",Val,"\">\n"]).



%===============================================================================
% components
