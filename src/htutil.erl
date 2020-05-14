-module (htutil).

-export ([escape_text/1]).

-import(unicode,[characters_to_list/1]).
-import(httpd_util,[integer_to_hexlist/1]).
-import(lists,[flatten/1]).



escape_text (Txt) ->
	flatten([esc_char(X) || X <- characters_to_list(Txt)]).


%-----------------------------
% escaping text for html representation
	
esc_char (Ch) when Ch > 255 ->
	["&#x",integer_to_hexlist(Ch),";"];
	
esc_char (Ch) ->
	case Ch of
		$" -> "&quot;";
		$& -> "&amp;";
		$> -> "&gt;";
		$< -> "&lt;";
		$' -> "&#x27;";
		_  -> Ch
	end.
