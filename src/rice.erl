-module(rice).
-export([ast/1,compile/1]).

ast(Filename)->
    AST = rice_peg:file(Filename),
    io:format("AST: ~w~n", [AST]),
    erl_prettypr:format(erl_syntax:form_list(AST)).

compile(Filename) ->
    AST = rice_peg:file(Filename),
    case compile:forms(AST) of
        {ok, Module, Binary} ->
            file:write_file(atom_to_list(Module) ++ ".beam", Binary)
    end.
            
