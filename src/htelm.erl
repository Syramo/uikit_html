-module(htelm).

-export([document/2, document/3]).

-export([t_div/2,t_p/2,t_span/2]).
-export([t_h1/1,t_h2/1,t_h3/1,t_h4/1,t_h5/1,t_h6/1,t_h1/2,t_h2/2,t_h3/2,t_h4/2,t_h5/2,t_h6/2]).



-import(htutil,[escape_text/1]).



% creating a document providing attributes the head element and the body element

document (Head, Body) ->
	document ([],Head, Body).
	

document (Attr, Head, Body) ->
	iolist_to_binary([<<"<!DOCTYPE html>\n">>, element("HTML",Attr,[Head, Body]),"\n"]).





% structural html elements

t_div (A,C) -> lists:flatten(["\n",element("div",A,C),"\n"]).

t_p (A, C) -> lists:flatten(["\n",element("p",A,C),"\n"]).

t_span (A, C) -> element("span",A,C).



%headings 

t_h1 (C)   -> t_h1([],C).
t_h1 (A,C) -> lists:flatten(["\n",element("h1",A,C),"\n"]).

t_h2 (C)   -> t_h2([],C).
t_h2 (A,C) -> lists:flatten(["\n",element("h2",A,C),"\n"]).

t_h3 (C)   -> t_h3([],C).
t_h3 (A,C) -> lists:flatten(["\n",element("h3",A,C),"\n"]).

t_h4 (C)   -> t_h4([],C).
t_h4 (A,C) -> lists:flatten(["\n",element("h4",A,C),"\n"]).

t_h5 (C)   -> t_h5([],C).
t_h5 (A,C) -> lists:flatten(["\n",element("h5",A,C),"\n"]).

t_h6 (C)   -> t_h6([],C).
t_h6 (A,C) -> lists:flatten(["\n",element("h6",A,C),"\n"]).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% internal functions
%%
%%

	
element (Name,Attr,Content) ->
	Tag = tag_name(Name),
	["<",Tag,attribute_string(Attr),">",Content,"</",Tag,">"].
	
	
tag_name (Name) ->
	case is_atom(Name) of
		true -> atom_to_list(Name);
		_	 -> string:lowercase(Name)
	end.
	


%---------------------------
% handling html element attributes

attribute_string ([]) ->
	[];
	
attribute_string (Al) ->
	attribute_string (Al, []).
	
	
	
attribute_string ([],As) ->
	As;
	
attribute_string ([At|Al], As) ->
	attribute_string(Al,[As, <<" ">>, mk_as(At)]).
	

mk_as ({Name, Val}) ->
	[tag_name(Name),<<"=\"">>,escape_text(Val),<<"\"">>];
	
mk_as (Name) ->
	tag_name(Name).
	
	

