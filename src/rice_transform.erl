-module(rice_transform).
-author("Caio Ariede <caio.ariede [do not spam] gmail com>").
-export(['transform'/3]).



transform('root', [Module, Functions, _], _) ->
    [Module | Functions];



transform('module', [_, _, {'identifier', ModuleName}], {{'line', Line}, _}) ->
    {'attribute', Line, 'module', list_to_atom(ModuleName)};



transform('functions',  Functions, _) ->
    Functions;



transform('function', [Clause = {_, Line, Identifier, Arity, _, _, _}, Clauses, _, _], _) ->
    {'function', Line, Identifier, Arity, rice_clauses(Identifier, Arity, [Clause | Clauses], [])};



transform('clause', [_, _, _, {'identifier', Identifier}, [], Block], {{'line', Line}, _}) ->
    scope_erase(),
    {'clause', Line, list_to_atom(Identifier), 0, [], [], Block};

transform('clause', [_, _, _, {'identifier', Identifier}, [_, Args, _, Kwargs], Block], {{'line', Line}, _}) ->
    scope_erase(),
    ArgList = Args ++ Kwargs,
    {'clause', Line, list_to_atom(Identifier), length(ArgList), ArgList, [], Block};

transform('clause', [_, _, _, {'identifier', Identifier}, [_, Args], Block], {{'line', Line}, _}) ->
    scope_erase(),
    {'clause', Line, list_to_atom(Identifier), length(Args), Args, [], Block};



transform('clause_args_arg', {'identifier', Identifier}, {{'line', Line}, _}) ->
    Var = rice_var(Identifier),
    scope_add(Var),
    {'var', Line, Var};



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



transform('call', [[Term | Functions], []], {{'line', Line}, _}) ->
    rice_call({Term, Functions}, Line, []);

transform('call', [[Term | Functions], [_, Args]], {{'line', Line}, _}) ->
    rice_call({Term, Functions}, Line, Args);

transform('call', [Function, []], {{'line', Line}, _}) ->
    rice_call(Function, Line, []);

transform('call', [Function, [_, Args]], {{'line', Line}, _}) ->
    rice_call(Function, Line, Args);



transform('call_value', [Node, Identifier], _) ->
    [Node] ++ rice_trim_left(Identifier, []);

transform('call_value', Identifier, _) ->
    Identifier;



transform('call_args', [Arg, Args], _) ->
    [Arg | rice_trim_left(Args, [])];



transform('call_args_arg', [{'identifier', Arg}, _], _) ->
    rice_var(Arg);

transform('call_args_arg', [Arg, _], _) ->
    Arg;



transform('call_kwargs', [Arg, Args], {{'line', Line}, _}) ->
    [{'tuple', Line, [{'atom', Line, 'kwargs'}, rice_cons([Arg | rice_trim_left(Args, [])], Line)]}];



transform('call_kwargs_arg', [Identifier, _, _, _, Value], {{'line', Line}, _}) ->
    {'tuple', Line, [Identifier, Value]};



transform('string', Node, {{'line', Line}, _}) ->
    {'string', Line, lists:flatten(proplists:get_value('string', Node))};



transform('variable', Identifier, {{'line', Line}, _}) ->
    {'var', Line, rice_var(Identifier)};



transform('atom', Node, {{'line', Line}, _}) ->
    {'atom', Line, list_to_atom(lists:flatten(proplists:get_value('atom', Node)))};



transform('integer', ["-", Number], {{'line', Line}, _}) ->
    {'op', Line, '-', {'integer', Line, list_to_integer(Number)}};



transform('integer', [_, Number], {{'line', Line}, _}) ->
    {'integer', Line, list_to_integer(Number)};



transform('identifier', Identifier, _) ->
    {'identifier', lists:flatten(Identifier)};



transform('list', [_, _, Head, Tail, _, _], {{'line', Line}, _}) ->
    rice_cons([Head | rice_trim_left(Tail, [])], Line);



transform('slice', [Value, _, [Start, _, []], _], {{'line', Line}, _}) ->
    rice_func({'rice', 'slice'}, Line, [Start, {'integer', Line, 1}, Value]);

transform('slice', [Value, _, [[], _, Count], _], {{'line', Line}, _}) ->
    rice_func({'rice', 'slice'}, Line, [{'integer', Line, 0}, Count, Value]);

transform('slice', [Value, _, [Start, _, Count], _], {{'line', Line}, _}) ->
    rice_func({'rice', 'slice'}, Line, [Start, Count, Value]);

transform('slice', [Value, _, Start, _], {{'line', Line}, _}) ->
    rice_func({'rice', 'slice'}, Line, [Start, {'integer', Line, 1}, Value]);



% remove this later, defensive programming / catch-all is bad, really bad!
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






rice_call({{'identifier', Term}, [{'identifier', Function} | Tail]}, Line, Args) ->
    Var = rice_var(Term),
    case scope_exists(Var) of
        true ->
            rice_call({rice_call({{'var', Line, Var}, [{'identifier', Function}]}, Line, Args), Tail}, Line, Args);
        _ ->
            rice_call({rice_func({list_to_atom(Term), list_to_atom(Function)}, Line, Args), Tail}, Line, Args)
    end;

rice_call({Term, [{'identifier', Function} | []]}, Line, []) ->
    rice_func({'rice', 'call'}, Line, [Term, {'atom', Line, list_to_atom(Function)}]);

rice_call({Term, [{'identifier', Function} | []]}, Line, Args) ->
    rice_func({'rice', 'call'}, Line, [Term, {'atom', Line, list_to_atom(Function)}, rice_cons(Args, Line)]);

rice_call({Term, [{'identifier', Function} | Tail]}, Line, Args) ->
    rice_call({rice_func({'rice', 'call'}, Line, [Term, {'atom', Line, list_to_atom(Function)}]), Tail}, Line, Args);

rice_call({'identifier', Function}, Line, []) ->
    Var = rice_var(Function),
    case scope_exists(Var) of
        true ->  {'var', Line, Var};
        _ -> rice_func(list_to_atom(Function), Line, [])
    end;

rice_call({'identifier', Function}, Line, Args) ->
    rice_func(list_to_atom(Function), Line, Args).






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






rice_trim_left([], Acc) ->
    lists:reverse(Acc);

rice_trim_left([[_, Value] | Tail], Acc) ->
    rice_trim_left(Tail, [Value | Acc]).






rice_var([S|Tring]) ->
    list_to_atom([string:to_upper(S)|Tring]).
