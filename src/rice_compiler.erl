-module(rice_compiler).
-export([
    compile/2,
    erl/2,
    erl/1,
    version/0,
    print_version/0
]).

-define(VERSION, 0.1).
-define(VERSION_PRINT, "0.1 (2010)").

version() ->
    ?VERSION.

print_version() ->
    io:format("Rice Programming Language ~s~n", [?VERSION_PRINT]).

compile(File, Output) ->
    AST = rice_peg:file(File),
    case compile:forms(AST) of
        {ok, Module, Binary} ->
            Path = string:strip(Output, right, $/) ++ "/" ++ atom_to_list(Module) ++ ".beam",
            file:write_file(Path, Binary),
            {ok, Path};
        Error ->
            Error
    end.

erl(File, Output) ->
    Code = ?MODULE:erl(File),
    Path = string:strip(Output, right, $/) ++ "/" ++ filename:basename(File, ".ri") ++ ".erl",
    file:write_file(Path, list_to_binary(Code)),
    Path.

erl(File) ->
    AST = rice_peg:file(File),
    erl_prettypr:format(erl_syntax:form_list(AST)).
