-module(htmlrnd).

-export([document/2, document/3]).


% creating a document providing attributes the head element and the body element

document (Head, Body) ->
	document ([],Head, Body).
	

document (Attr, Head, Body) ->
	Doc = [<<"<!DOCTYPE html>\n">>, element("HTML",Attr,[Head, Body])],
	iolist_to_binary(Doc).





%% internal helpers

element (Name,Attr,Content) ->
	Tag = tag_name(Name),
	As = attribute_string(Attr),
	[<<"<">>,Tag,As,<<">\n">>,Content,<<"\n</">>,Tag,<<">\n">>].
	
	
inline (Name,Attr,Content) ->
	Tag = tag_name(Name),
	[<<"<">>,Tag,attribute_string(Attr),<<">">>,Content,<<"</">>,Tag,<<">">>].
	
	
tag_name (Name) ->
	case is_atom(Name) of
		true -> atom_to_binary(Name);
		_	 -> string:lowercase(Name)
	end.
	

attribute_string ([]) ->
	[];
	
attribute_string (Al) ->
	attribute_string (Al, []).
	
	
attribute_string ([],As) ->
	As;
	
attribute_string ([At|Al], As) ->
	attribute_string(Al,[As, <<" ">>, mk_as(At)]).
	

mk_as ({Name, Val}) ->
	iolist_to_binary([tag_name(Name),<<"=\"">>,Val,<<"\"">>]);
	
mk_as (Name) ->
	tag_name(Name).
	
