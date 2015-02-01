-module(hello).
-export([sayHello/0]).


sayHello() ->
  io:format("Hello World!~n").
