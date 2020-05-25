-module (ukdom).

-export ([create/0, next/1]).

-record (dom,{ids = #{} :: pathmap(), tree_node :: treenode(), path = [] :: nodepath()}).

-type pathmap() :: #{string() => nodepath()}.
-type nodepath() :: [integer()].

-type zlist(A) :: {Left::list(A), Right::list(A)}.
-type znode() :: {Element :: element(), Children :: [element()]}.
-type element() :: htelm:ht_elm() | htelm:vd_elm() | htelm:tx_elm().
-type treenode() :: {Ancestors :: [zlist(znode)], Siblings :: zlist(znode())}.

-type dom() :: #dom{}.

	

%%----------create an initial dom with one html element in it-------------------

-spec create() -> Dom :: dom().
create () ->
	TN = {[], {[], [{htelm:t_head(), []},{htelm:t_body(), []}]}},
	#dom{tree_node = TN, path = [1]}.
	


%%----------manipulate the dom--------------------------------------------------

-spec next(Dom :: dom()) -> {ok, dom()} | {error, Reason :: string()}.
next (#dom{tree_node={_,{_,[_]}}}) -> 
	{error,"last sibling"};

next (Dom = #dom{tree_node={A,{P,[C|T]}}, path = [I|R]}) -> 
	{ok, Dom#dom{tree_node={A,{[C|P],T}}, path=[I+1|R]}}.




%%	add a single node to the DOM as a child to ParentID (0 = root)
%	the parent node must exist or bad things happen

% -spec add_node (Dom, PaID, Name, Attr) -> {Dom, NodeID}	when	Dom :: dom(),
% 																PaID :: integer(),
% 																Name :: nonempty_string(), 
% 																Attr :: attr_map(),
% 																NodeID :: integer().
												
% add_node (Dom, PaID, Name, Attr) ->
% 	Nid = Dom#dom.dom_id,
% 	#{PaID := Pnode} = Dom#dom.tree,
% 	Pnode2 = Pnode#node{cids = [Nid | Pnode#node.cids]},
% 	Node = #node{name = Name, attr = Attr, paid = PaID},
% 	Dom2 = Dom#dom{dom_id = Nid +1, tree = (Dom#dom.tree)#{PaID := Pnode2, Nid => Node}},
% 	{Dom2, Nid}.
% 	
% 	
% 
% 
% -spec mk_node (Name) -> Node		when	Name :: atom() | string(),
% 											Node :: ht_node() | ht_text().
% 										
% mk_node (Name) when is_atom(Name) ->
% 	mk_node(Name, #{});
% 	
% mk_node (Text) ->
% 	#text{text = Text}.
% 	
% 
% 
% 
% -spec mk_node (Name, Attr) -> Node	when	Name :: atom(),
% 											Attr :: attr_map(),
% 											Node :: ht_node().
% 											
% -spec mk_node (Name, Id, Attr) -> Node		when	Name :: atom(),
% 													Id :: string(),
% 													Attr :: attr_map(),
% 													Node :: ht_node().
% 											
% 											
% mk_node (Name, Attr) when is_atom(Name) ->
% 	#node{name = Name, attr = Attr}.
% 	
% 
% mk_node (Name, Id, Attr) when is_atom(Name) ->
% 	#node{name = Name, attr = Attr#{id => Id}}.