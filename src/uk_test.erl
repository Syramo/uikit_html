-module (uk_test).

-import (ukhtml,[uk_doc/1,uk_doc/2]).

-export ([run/0]).




run () ->
	Body = [],
	Doc = uk_doc(Body),
	file:write_file("output.txt",Doc).
	
