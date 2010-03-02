-module(rice_transform).
-author("Caio Ariede <caio.ariede@gmail.com>").
-export(['transform'/3]).

transform('root', [Module, Functions, _], _) ->
    [Module | Functions];

transform('module', [_, _, ModuleName], {{'line', Line}, _}) ->
    {'attribute', Line, 'module', list_to_atom(ModuleName)};

transform('functions',  Functions, _) ->
    Functions;

transform('function', [Clause = {_, Line, Identifier, Arity, _, _, _}, Clauses, _, _], _) ->
    {'function', Line, Identifier, Arity, rice_clauses(Identifier, Arity, [Clause | Clauses], [])};

transform('clause', [_, _, _, Identifier, [], Block], {{'line', Line}, _}) ->
    scope_erase(),
    {'clause', Line, list_to_atom(Identifier), 0, [], [], Block};

transform('clause', [_, _, _, Identifier, [_, Args, _, Kwargs], Block], {{'line', Line}, _}) ->
    scope_erase(),
    ArgList = Args ++ Kwargs,
    {'clause', Line, list_to_atom(Identifier), length(ArgList), ArgList, [], Block};

transform('clause', [_, _, _, Identifier, [_, Args], Block], {{'line', Line}, _}) ->
    scope_erase(),
    {'clause', Line, list_to_atom(Identifier), length(Args), Args, [], Block};

transform('clause_args_arg', Var = {'var', _, Term}, _) ->
    scope_add(Term),
    Var;

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

transform('call', [Term, []], {{'line', Line}, _}) ->
    Var = rice_var(Term),
    case scope_exists(Var) of
        true ->  {'var', Line, Var};
        _ -> rice_func(list_to_atom(Term), Line, [])
    end;

transform('call', [[Term, Function], [_, Args]], {{'line', Line}, _}) ->
    rice_call(list_to_atom(Function), Line, Term, Args);

transform('call', [[Term, Function], _], {{'line', Line}, _}) ->
    rice_call(list_to_atom(Function), Line, Term, []);

transform('call', [Function, [_, Args]], {{'line', Line}, _}) ->
    rice_func(list_to_atom(Function), Line, Args);

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



scope_add(Key) ->
    erlang:put('current_scope', case erlang:get('current_scope') of
        undefined -> [Key];
        Scope -> Scope ++ [Key]
    end).

scope_exists(Key) ->
    case erlang:get('current_scope') of
        undefined -> false;
        Scope -> case lists:member(Key, Scope) of
            false -> false;
            _ -> true
        end
    end.

scope_erase() ->
    erlang:erase('current_scope').




rice_call(Function, Line, Term, []) ->
    {'call', Line, {'remote', Line, {'atom', Line, 'rice'}, {'atom', Line, 'call'}}, [Term, {'atom', Line, Function}]};

rice_call(Function, Line, Term, Args) ->
    {'call', Line, {'remote', Line, {'atom', Line, 'rice'}, {'atom', Line, 'call'}}, [Term, {'atom', Line, Function}, rice_cons(Args, Line)]}.





rice_func({Module, Function}, Line, Args) ->
    {'call', Line, {'remote', Line, {'atom', Line, Module}, {'atom', Line, Function}}, Args};

rice_func(Function, Line, Args) ->
    {'call', Line, {'remote', Line, {'atom', Line, 'rice'}, {'atom', Line, Function}}, Args}.





rice_cons([Head], Line) ->
    {'cons', Line, Head, {'nil', Line}};

rice_cons([Head | Tail], Line) ->
    {'cons', Line, Head, rice_cons(Tail, Line)}.




rice_clauses(_, _, [], Acc) ->
    lists:reverse(Acc);

rice_clauses(Identifier, _, [{'clause', Line, ClauseIdentifier, _, _, _, _} | _], _) when Identifier /= ClauseIdentifier ->
    exit({'error', Line, 'invalid clause identifier'});

rice_clauses(_, Arity, [{'clause', Line, _, ClauseArity, _, _, _} | _], _) when Arity /= ClauseArity ->
    exit({'error', Line, 'invalid clause arity'});

rice_clauses(Identifier, Arity, [{'clause', Line, Identifier, Arity, Args, Guards, Block} | Tail], Acc) ->
    rice_clauses(Identifier, Arity, Tail, [{'clause', Line, Args, Guards, Block} | Acc]).




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
