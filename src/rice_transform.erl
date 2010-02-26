-module(rice_transform).
-author("Caio Ariede <caio.ariede@gmail.com>").
-export(['transform'/3]).

transform('root', [Module, Tree, _], _) ->
    [Functions] = rice_tree(Tree, [], []),
    [Module] ++ Functions;

transform('module', [_, _, ModuleName], {{'line', Line}, _}) ->
    {'attribute', Line, 'module', list_to_atom(ModuleName)};

transform('clause', [_, _, _, Identifier, [], Block], {{'line', Line}, _}) ->
    {'clause', [{Identifier, 0}, Line, [], [], Block]};

transform('clause', [_, _, _, Identifier, [_, Args, _, Kwargs], Block], {{'line', Line}, _}) ->
    ArgList = Args ++ Kwargs,
    {'clause', [{Identifier, length(ArgList)}, Line, ArgList, [], Block]};

transform('clause', [_, _, _, Identifier, [_, Args], Block], {{'line', Line}, _}) ->
    {'clause', [{Identifier, length(Args)}, Line, Args, [], Block]};

transform('clause_args', [Arg, Args], _) ->
    [Arg | rice_trim_left(Args, [])];

transform('clause_kwargs', [_, Identifier, _, _, Keyword, Keywords, _, _], {{'line', Line}, _}) ->
    [{'match', Line, {'var', Line, rice_var(Identifier)}, {'tuple', Line, [{'atom', Line, 'kwargs'}, rice_cons([Keyword | Keywords], Line)]}}];

transform('clause_kwargs_key', [Key, _, _, _, Value], {{'line', Line}, _}) ->
    {'tuple', Line, [Key, Value]};

transform('clause_kwargs_key', Key, {{'line', Line}, _}) ->
    {'tuple', Line, [Key, []]};

transform('block', [_, Block], _) ->
    Block;

transform('block_inline', [_, _, Block], _) ->
    Block;

transform('do', [_, Args, Block], {{'line', Line}, _}) ->
    {'fun', Line, Args, [], Block};

transform('statements', [Statement, Statements], _) ->
    [Statement | Statements];

transform('statements_inline', [Statement, Statements], _) ->
    [Statement | rice_trim_left(Statements, [])];

transform('statements_samedent', [_, [Statements]], _) ->
    Statements;

transform('call', [Call, [_, Args]], {{'line', Line}, _}) ->
    rice_call(Call, Line, Args);

transform('call', [Call, _], {{'line', Line}, _}) ->
    rice_call(Call, Line, []);

%transform('call', [[Node, _, Identifier], []], {{'line', Line}, _}) ->
%    rice_call(Node, Line, list_to_atom(Identifier), []);
%
%transform('call', [[Node, _, Identifier], [_, Args]], {{'line', Line}, _}) ->
%    rice_call(Node, Line, list_to_atom(Identifier), Args);
%
%transform('call', [[Node, _, Identifier], [_, Args, _, Kwargs]], {{'line', Line}, _}) ->
%    rice_call(Node, Line, list_to_atom(Identifier), Args ++ Kwargs);
%
%transform('call', [Identifier, []], {{'line', Line}, _}) ->
%    {'call', Line, {'atom', Line, list_to_atom(Identifier)}, []};
%
%transform('call', [Identifier, [_, Args]], {{'line', Line}, _}) ->
%    {'call', Line, {'atom', Line, list_to_atom(Identifier)}, Args};
%
%transform('call', [Identifier, [_, Args, _, Kwargs]], {{'line', Line}, _}) ->
%    {'call', Line, {'atom', Line, list_to_atom(Identifier)}, Args ++ Kwargs};

transform('call_value', [Node, Identifier], _) ->
    [Node] ++ rice_trim_left(Identifier, []);

transform('call_value', Identifier, _) ->
    Identifier;

transform('call_args', [Arg, Args], {{'line', Line}, _}) ->
    [Arg | rice_trim_left(Args, [])];

transform('call_args_arg', [Arg, _], _) ->
    Arg;

transform('call_kwargs', [Arg, Args], {{'line', Line}, _}) ->
    [{'tuple', Line, [{'atom', Line, 'kwargs'}, rice_cons([Arg | rice_trim_left(Args, [])], Line)]}];

transform('call_kwargs_arg', Node = [Identifier, _, _, _, Value], {{'line', Line}, _}) ->
    {'tuple', Line, [Identifier, Value]};

%transform('slice', [Node, _, Index, _], {{'line', Line}, _}) ->
%    rice_call(Node, Line, 'slice', [Index]);

transform('variable', Identifier, {{'line', Line}, _}) ->
    {'var', Line, rice_var(Identifier)};

transform('atom', Node, {{'line', Line}, _}) ->
    {'atom', Line, list_to_atom(lists:flatten(proplists:get_value(atom, Node)))};

transform('integer', ["-", Number], {{'line', Line}, _}) ->
    {'op', Line, '-', {'integer', Line, list_to_integer(Number)}};

transform('integer', [_, Number], {{'line', Line}, _}) ->
    {'integer', Line, list_to_integer(Number)};

transform('identifier', Identifier, _) ->
    lists:flatten(Identifier);

transform(_, Node, _) ->
    Node.




rice_tree([], ClauseAcc, Acc) ->
    lists:reverse(rice_function(ClauseAcc, [], []) ++ Acc);

rice_tree([Clause = {'clause', [Head | _]} | Tail], ClauseAcc = [{'clause', [Head | _]} | _], Acc) ->
    rice_tree(Tail, [Clause | ClauseACc], Aux);

rice_tree([Clause = {'clause', _} | Tail], [], Acc) ->
    rice_tree(Tail, [Clause], Acc);

rice_tree([Clause = {'clause', _} | Tail], ClauseAcc, Acc) ->
    rice_tree(Tail, [Clause], rice_function(ClauseAcc, [], []) ++ Acc).

%rice_tree([], ClauseAcc, FunctionAcc) ->
%    [
%        lists:reverse(rice_function(ClauseAcc, [], []) ++ FunctionAcc)
%    ];
%
%rice_tree([Clause = {'clause', [Head | _]} | Tail], ClauseAcc = [{'clause', [Head | _]} | _], FunctionAcc) ->
%    rice_tree(Tail, [Clause | ClauseAcc], FunctionAcc);
%
%rice_tree([Clause = {'clause', _} | Tail], [], FunctionAcc) ->
%    rice_tree(Tail, [Clause], FunctionAcc);
%
%rice_tree([Clause = {'clause', _} | Tail], ClauseAcc, FunctionAcc) ->
%    rice_tree(Tail, [Clause], rice_function(ClauseAcc, [], []) ++ FunctionAcc).
%
%
%rice_reparse(Block) ->
%    rice_reparse(Block, [], []).
%
%% agrupar funcao reparse a rice_tree
%
%
%rice_reparse([], [], Acc) ->
%    lists:reverse(Acc);
%
%rice_reparse([], FunAcc, Acc) ->
%    [rice_fun(FunAcc, []) | Acc];
%
%rice_reparse([Head = {'fun', _, _, _, _} | Tail], FunAcc, Acc) ->
%    rice_reparse(Tail, [Head | FunAcc], Acc);
%
%rice_reparse([Head | Tail], [], Acc) ->
%    rice_reparse(Tail, [], [Head | Acc]);
%
%rice_reparse([Head | Tail], FunAcc, Acc) ->
%    rice_reparse(Tail, [], [Head | [rice_fun(FunAcc, []) | Acc]]).




rice_call([Node, Call | []], Line, []) ->
    {'call', Line, {'remote', Line, {'atom', Line, 'rice'}, {'atom', Line, 'call'}}, [Node, {'atom', Line, list_to_atom(Call)}]};

rice_call([Node, Call | []], Line, Args) ->
    {'call', Line, {'remote', Line, {'atom', Line, 'rice'}, {'atom', Line, 'call'}}, [Node, {'atom', Line, list_to_atom(Call)}, rice_cons(Args, [])]};

rice_call([Node, Call | Tail], Line, Args) ->
    rice_call([{'call', Line, {'remote', Line, {'atom', Line, 'rice'}, {'atom', Line, 'call'}}, [Node, {'atom', Line, list_to_atom(Call)}]} | Tail], Line, Args).




rice_cons([Head], Line) ->
    {'cons', Line, Head, {'nil', Line}};

rice_cons([Head | Tail], Line) ->
    {'cons', Line, Head, rice_cons(Tail, Line)}.




rice_function([], [], []) ->
    [];

rice_function(Clauses = [{'clause', [Head | _]} | _], [], []) ->
    rice_function(Clauses, Head, []);

rice_function([], {Identifier, Arity}, Acc) ->
    Clauses = lists:reverse(Acc),
    [{'clause', Line, _, _, _} | _] = Clauses,
    [{'function', Line, list_to_atom(Identifier), Arity, Acc}];

rice_function([{'clause', [_, Line, Args, _, Block]} | Clauses], Head, Acc) ->
    rice_function(Clauses, Head, [{'clause', Line, Args, [], rice_reparse(Block)} | Acc]).




rice_fun([], Acc) ->
    AccReversed = lists:reverse(Acc),
    [{_, Line, _, _, _} | _] = AccReversed,
    {'fun', Line, {'clauses', AccReversed}};

rice_fun([{'fun', Line, Args, Guards, Block} | Tail], Acc) ->
    rice_fun(Tail, [{'clause', Line, Args, Guards, Block}]).




rice_trim_left([], Acc) ->
    lists:reverse(Acc);

rice_trim_left([[_, Value] | Tail], Acc) ->
    rice_trim_left(Tail, [Value | Acc]).



rice_var([S|Tring]) ->
    list_to_atom([string:to_upper(S)|Tring]).
