-module(b).
-export([rich_text_length/1]).

-spec rich_text_length(RichText) -> integer() when
  RichText :: a:rich_text().
rich_text_length(RichText) ->
  erlang:length(RichText).
