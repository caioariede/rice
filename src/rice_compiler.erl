-module(rice_compiler).
-export([
    compile/2,
    erl/2,
    erl/1,
    version/0,
    print_version/0
]).

-define(VERSION, 0.01).
-define(VERSION_PRINT, "0.01 (2010)").

version() ->
    ?VERSION.

print_version() ->
    io:format("Rice Programming Language ~s~n", [?VERSION_PRINT]).

compile(File, Opts) ->
    CompileOptions = parse_from_opts(Opts),
    Output = proplists:get_value(output, Opts),
    AST = rice_peg:file(File),
    case compile:forms(AST, CompileOptions) of
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

parse_from_opts([]) ->
    [];

parse_from_opts([{output, Dir} | T]) ->
    [{outdir, Dir} | parse_from_opts(T)];

parse_from_opts([{include, Dir} | T]) ->
    [{i, Dir} | parse_from_opts(T)];

parse_from_opts([{macro, Macros} | T]) ->
    [{d, Macro, Value} || {Macro, Value} <- Macros] ++ parse_from_opts(T);

parse_from_opts([{warn, 0} | T]) ->
    parse_from_opts(T);

parse_from_opts([{warn, Verbosity} | T]) ->
    [report_warnings, {warn_format, Verbosity} | parse_from_opts(T)];

parse_from_opts([{verbose, 1} | T]) ->
    [verbose | parse_from_opts(T)];

parse_from_opts([{term, [Terms]} | T]) ->
    [list_to_atom(Term) || Term <- string:tokens(Terms, [$,])] ++ parse_from_opts(T);

parse_from_opts([_ | T]) ->
    parse_from_opts(T).
