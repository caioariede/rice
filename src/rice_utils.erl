-module(rice_utils).
-export([scan/1,scan_file/1]).

scan(String)->
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, AST} = erl_parse:parse_exprs(Tokens),
    AST.

scan_file(Filename)->
    {ok, Binary} = file:read_file(Filename),
    io:format("~w~n", [rice_utils:scan(binary_to_list(Binary))]).
