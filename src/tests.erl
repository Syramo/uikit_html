-module (tests).

-import (ukhtml,[uk_doc/1,uk_doc/2]).

-export ([run/0]).

-include ("ukhtml.hrl").




run () ->
	Body = "<div>hello</div>\n",
	P = #meta{other_css=["css/my.css","css/other.css"],other_js=[{top,"js/my.js"},{bottom,"js/other.js"}]},
	Doc = uk_doc(Body,P),
	file:write_file("output.txt",Doc).
	
