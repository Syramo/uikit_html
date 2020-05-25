-module(ukhtml).

-export([uk_doc/1,uk_doc/2]).

-import(htelm,[document/3]).

-include("ukhtml.hrl").


%===============================================================================
% document creation

uk_doc (Body) ->
	uk_doc(Body, #meta{}).

uk_doc (Body, P) ->
	document([{lang,"en"}], t_head(P), t_body(Body,P)).


t_head (P) when is_record(P,meta) ->
	["\n<head>\n"
	,"<meta charset=\"utf-8\">\n"
	,t_meta(title,P#meta.title)
	,t_meta(viewport,"width=device-width, initial-scale=1")
	,t_css(P#meta.uikit_css)
	,[t_css(Path) || Path <- P#meta.other_css]
	,"</head>"].
	
t_meta (K,Val) when is_atom(K) ->
	["<meta name=\"",atom_to_list(K),"\" content=\"",Val,"\">\n"];
	
t_meta (K,Val) ->
	["<meta name=\"",K,"\" content=\"",Val,"\">\n"].


t_body (B,P) when is_record(P,meta) ->
	["\n<body>\n"
	,t_js(P#meta.uikit_js)
	,[t_js(Path) || {top,Path} <- P#meta.other_js]
	,B
	,[t_js(Path) || {bottom,Path} <- P#meta.other_js]
	,"</body>\n"].
	
	
t_css (Path) ->
	["<link rel=\"stylesheet\" type=\"text/css\" href=\"",Path,"\">\n"].
	
t_js (Path) ->
	["<script src=\"",Path,"\"></script>\n"].

%===============================================================================
% components
