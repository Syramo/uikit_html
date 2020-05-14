-module(ukhtml).

-export([uk_doc/2]).

-import(htmlrnd,[document/3]).


uk_doc (Body, _Props) ->
	document([], <<"<head></head>">>, Body).



