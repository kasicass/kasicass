-module(a).

-type rich_text() :: [{font(), char()}].
-type font()      :: integer().

-export_type([rich_text/0, font/0]).
