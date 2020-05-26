-module(htelm).


-export ([open/1, close/1, allow_child/1]).

-export_type([ht_elm/0,vd_elm/0,tx_elm/0]).

% standard html elements that are supported
-export ([t_html/0,t_html/1,t_head/0,t_head/1,t_body/0,t_body/1]).
-export ([t_meta/1, t_meta/2, t_link/1, t_link_css/1, t_script/1]).


-import (htutil,[escape_text/1]).


%%----------type definition-----------------------------------------------------

%	ht_elm - standard element with contents (other elements) with open and close tag
-record(ht_elm, {name :: atom(), id= ""	:: string(), attr = #{}	:: attributes()}).
	
%	vd_elm - empty html element that has no close tag and no contents
-record(vd_elm, {name :: atom(), id = "" :: string(), attr = #{} :: attributes()}).
	
%	tx_elm - dummy element that holds notmal text that will be escaped, id is for internal node reference only
-record(tx_elm,{id = ""	:: string(), content :: string()}).
	
	
-opaque ht_elm() :: #ht_elm{}.
-opaque vd_elm() :: #vd_elm{}.
-opaque tx_elm() :: #tx_elm{}.
-type attributes() :: #{atom() := string()}.




%%----------standard html tags definition---------------------------------------

-spec t_html () -> ht_elm().
-spec t_html (A :: attributes()) -> ht_elm().

-spec t_head () -> ht_elm().
-spec t_head (A :: attributes()) -> ht_elm().

-spec t_body () -> ht_elm().
-spec t_body (A :: attributes()) -> ht_elm().

-spec t_meta (A :: attributes()) -> vd_elm().
-spec t_meta (Name :: string(), Description :: string()) -> vd_elm().

-spec t_link (A :: attributes()) -> vd_elm().
-spec t_link_css (Loc :: string()) -> vd_elm().

-spec t_script (Loc :: string()) -> ht_elm().



%%----------standard html tags implementation-----------------------------------

t_html () -> #ht_elm{name = html}.
t_html (A) -> #ht_elm{name = html, attr = A}.

t_head () -> #ht_elm{name = head}.
t_head (A) -> #ht_elm{name = head, attr = A}.

t_body () -> #ht_elm{name = body}.
t_body (A) -> #ht_elm{name = body, attr = A}.

t_meta (A) -> #vd_elm{name = meta, attr = A}.
t_meta (N,D) -> #vd_elm{name = meta, attr = #{name => N, description => D}}.

t_link (A) -> #vd_elm{name = link, attr = A}.
t_link_css (Loc) -> #vd_elm{name = link, attr = #{rel => "stylesheet", type => "text/css", href => Loc}}.

t_script (Loc) -> #ht_elm{name = script, attr = #{src => Loc}}.


%%---------converting tags / elements to string representation------------------

-spec open (Elm :: ht_elm() | vd_elm() | tx_elm()) -> string().

open ({tx_elm,_,C}) -> lists:flatten(escape_text(C));

open ({_,Name,Id,Attr}) when is_atom(Name) ->
	A = append_id(Id, Attr),
	case map_size(A) of
		0 -> lists:flatten(["<",atom_to_list(Name),">"]);
		_ -> lists:flatten(["<",atom_to_list(Name),attrib_to_list(A),">"])
	end.



-spec close (Elm :: ht_elm() | vd_elm() | tx_elm()) -> string().

close ({tx_elm,_,_}) -> [];		
close ({vd_elm,_,_,_}) -> [];
close ({ht_elm,Name,_,_}) when is_atom(Name)-> lists:flatten(["</",atom_to_list(Name),">"]).	




-spec allow_child (Elm :: ht_elm() | vd_elm() | tx_elm()) -> boolean().

allow_child ({ht_elm,_,_,_}) -> true;
allow_child (_) -> false.



%%----------internals-----------------------------------------------------------

-spec append_id(Id :: string(), A :: attributes()) -> attributes().

append_id ([],A) -> A;
append_id (Id,A) -> A#{id => Id}.



-spec attrib_to_list (A :: attributes()) -> list().

attrib_to_list (A) ->
	maps:fold(fun (K,V,Acc) when is_atom(K) -> [[" ",atom_to_list(K),"=\"",escape_text(V),"\""] | Acc] end, [], A).
