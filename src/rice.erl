-module(rice).
-export([file/1]).

file(Filename)->
    AST = rice_peg:file(Filename),
    io:format("AST: ~w~n", [AST]),
    erl_prettypr:format(erl_syntax:form_list(AST)).
