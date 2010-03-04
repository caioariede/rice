-module(rice_transform).
-author("Caio Ariede <caio.ariede [do not spam] gmail com>").
-export(['transform'/3]).


-define('FATAL'(Line, Message), exit({'fatal', Line, Message})).

-define('ERR_CLAUSE_INCORRECT_NAME', "Clause out of the group, incompatible name").
-define('ERR_CLAUSE_INCORRECT_ARITY', "Clause out of the group, incompatible arity").
-define('ERR_INDICE_NOT_INTEGER', "Indices must be integers, not float").



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
    [{'match', Line, {'var', Line, rice_var(Identifier)}, {'tuple', Line, [rice_atom('kwargs', Line), rice_cons([Keyword | Keywords], Line)]}}];



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


% Term "." Function
% Eg. "hello".length
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
    [{'tuple', Line, [rice_atom('kwargs', Line), rice_cons([Arg | rice_trim_left(Args, [])], Line)]}];



transform('call_kwargs_arg', [Identifier, _, _, _, Value], {{'line', Line}, _}) ->
    {'tuple', Line, [Identifier, Value]};



transform('string', Node, {{'line', Line}, _}) ->
    {'string', Line, lists:flatten(proplists:get_value('string', Node))};



transform('variable', Identifier, {{'line', Line}, _}) ->
    {'var', Line, rice_var(Identifier)};



transform('atom', Node, {{'line', Line}, _}) ->
    rice_atom(list_to_atom(lists:flatten(proplists:get_value('atom', Node))), Line);



transform('integer', ["-", Number], {{'line', Line}, _}) ->
    {'op', Line, '-', {'integer', Line, list_to_integer(Number)}};



transform('integer', [_, Number], {{'line', Line}, _}) ->
    {'integer', Line, list_to_integer(Number)};



% Exponent "." Mantissa 
% Eg. 123.45
transform('float', [[[], Exp], _, Mantissa], {{'line', Line}, _}) ->
    {'float', Line, list_to_float(Exp ++ "." ++ Mantissa)};

% "." Mantissa
% Eg. .45
transform('float', [[], _, Mantissa], {{'line', Line}, _}) ->
    {'float', Line, list_to_float("0." ++  Mantissa)};

% Signed Exponent "." Mantissa
% Eg. -123.45
transform('float', [["-", Exp], _, Mantissa], {{'line', Line}, _}) ->
    {'op', Line, '-', {'float', Line, list_to_float(Exp ++ "." ++  Mantissa)}};



% A "===" B
% Eg. 1.5 === 1.5
transform('op_identical', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '=:=', A, B};



% A "!==" B
% Eg. 1 !== 1.0
transform('op_not_identical', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '=/=', A, B};



% A "==" B
% Eg. 1 == 1.0
transform('op_equal', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '==', A, B};



% A "!=" B
% Eg. 1 != 1.0
transform('op_not_equal', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '/=', A, B};



% A "<" B
% Eg. 1 < 2
transform('op_less', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '<', A, B};



% A ">" B
% Eg. 2 > 1
transform('op_greater', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '>', A, B};



% A "<=" B
% Eg. 2 <= 2
transform('op_less_equal', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '=<', A, B};



% A ">=" B
% Eg. 2 >= 3
transform('op_greater_equal', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '>=', A, B};



% A "and" B
% Eg. true and true 
transform('op_and', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, 'and', A, B};



% A "or" B
% Eg. false or true 
transform('op_or', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, 'or', A, B};



% A "xor" B
% Eg. true xor true 
transform('op_xor', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, 'xor', A, B};



% "not" A
% Eg. not true 
transform('op_not', [_, _, A], {{'line', Line}, _}) ->
    {'op', Line, 'not', A};



% A "+" B
% Eg. 2 + 2
transform('op_add', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '+', A, B};

% A "-" B
% Eg. 2 - 2
transform('op_sub', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '-', A, B};

% A "*" B
% Eg. 2 * 2
transform('op_mul', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '*', A, B};

% A "/" B
% Eg. 4 / 2
transform('op_div', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, '/', A, B};

% A "%" B
% Eg. 4 % 2 
transform('op_mod', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, 'rem', A, B};



% A & B
% Eg. 1 & 1 (and)
transform('op_band', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, 'band', A, B};



% A | B
% Eg. 1 | 0 (inclusive or)
transform('op_bor', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, 'bor', A, B};



% A ^ B
% Eg. 1 ^ 1 (exclusive or) 
transform('op_bxor', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, 'bxor', A, B};



% A << B
% Eg. 1 << 1 (shift left)
transform('op_bsl', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, 'bsl', A, B};



% A >> B
% Eg. 1 >> 1 (shift right)
transform('op_bsr', [A, _, _, _, B], {{'line', Line}, _}) ->
    {'op', Line, 'bsr', A, B};



transform('identifier', Identifier, _) ->
    {'identifier', lists:flatten(Identifier)};



% "[" Term ("," Term)* "]"
% Eg. [1, 2, :foo, :bar]
transform('list', [_, _, Head, Tail, _, _], {{'line', Line}, _}) ->
    rice_cons([Head | rice_trim_left(Tail, [])], Line);



% Value "[" Pos ":" "]"
% Eg. [1, 2][2:]
transform('slice', [_, _, [Pos, _, []], _], {{'line', Line}, _}) when element(1, Pos) == 'float' ->
    ?FATAL(Line, ?ERR_INDICE_NOT_INTEGER);

transform('slice', [Value, _, [Pos, _, []], _], {{'line', Line}, _}) ->
    rice_func({'rice_core', 'call'}, Line, [Value, rice_atom('slice', Line), rice_cons([Pos, {'integer', Line, 1}], Line)]);

% Value "[" ":" Count "]"
% Eg. [1, 2][:2]
transform('slice', [_, _, [[], _, Count], _], {{'line', Line}, _}) when element(1, Count) == 'float' ->
    ?FATAL(Line, ?ERR_INDICE_NOT_INTEGER);

transform('slice', [Value, _, [[], _, Count], _], {{'line', Line}, _}) ->
    rice_func({'rice_core', 'call'}, Line, [Value, rice_atom('slice', Line), rice_cons([{'integer', Line, 0}, Count], Line)]);

% Value "[" Pos ":" Count "]"
% Eg. [1, 2][0:2]
transform('slice', [_, _, [Pos, _, Count], _], {{'line', Line}, _}) when (element(1, Pos) == 'float') or (element(1, Count) == 'float') ->
    ?FATAL(Line, ?ERR_INDICE_NOT_INTEGER);

transform('slice', [Value, _, [Pos, _, Count], _], {{'line', Line}, _}) ->
    rice_func({'rice_core', 'call'}, Line, [Value, rice_atom('slice', Line), rice_cons([Pos, Count], Line)]);

% Value "[" Pos "]"
% Eg. [1, 2][1]
transform('slice', [_, _, Pos, _], {{'line', Line}, _}) when element(1, Pos) == 'float' ->
    ?FATAL(Line, ?ERR_INDICE_NOT_INTEGER);

transform('slice', [Value, _, Pos, _], {{'line', Line}, _}) ->
    rice_func({'rice_core', 'call'}, Line, [Value, rice_atom('slice', Line), rice_cons([Pos], Line)]);



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






rice_call({Term, []}, Line, Args) ->
    Term;

rice_call({{'identifier', Term}, [{'identifier', Function} | Tail]}, Line, Args) ->
    Var = rice_var(Term),
    case scope_exists(Var) of
        true ->
            rice_call({rice_call({{'var', Line, Var}, [{'identifier', Function}]}, Line, Args), Tail}, Line, Args);
        _ ->
            rice_call({rice_func({list_to_atom(Term), list_to_atom(Function)}, Line, Args), Tail}, Line, Args)
    end;

rice_call({Term, [{'identifier', Function} | []]}, Line, []) ->
    rice_func({'rice_core', 'call'}, Line, [Term, rice_atom(list_to_atom(Function), Line)]);

rice_call({Term, [{'identifier', Function} | []]}, Line, Args) ->
    rice_func({'rice_core', 'call'}, Line, [Term, rice_atom(list_to_atom(Function), Line), rice_cons(Args, Line)]);

rice_call({Term, [{'identifier', Function} | Tail]}, Line, Args) ->
    rice_call({rice_func({'rice_core', 'call'}, Line, [Term, rice_atom(list_to_atom(Function), Line)]), Tail}, Line, Args);

rice_call({'identifier', Function}, Line, []) ->
    Var = rice_var(Function),
    case scope_exists(Var) of
        true ->  {'var', Line, Var};
        _ -> rice_func(list_to_atom(Function), Line, [])
    end;

rice_call({'identifier', Function}, Line, Args) ->
    rice_func(list_to_atom(Function), Line, Args).






rice_func({Module, Function}, Line, Args) ->
    {'call', Line, {'remote', Line, rice_atom(Module, Line), rice_atom(Function, Line)}, Args};

rice_func(Function, Line, Args) ->
    {'call', Line, {'remote', Line, rice_atom('rice_core', Line), rice_atom(Function, Line)}, Args}.






rice_cons([Head], Line) ->
    {'cons', Line, Head, {'nil', Line}};

rice_cons([Head | Tail], Line) ->
    {'cons', Line, Head, rice_cons(Tail, Line)}.






rice_clauses(_, _, [], Acc) ->
    lists:reverse(Acc);

rice_clauses(Identifier, _, [{'clause', Line, ClauseIdentifier, _, _, _, _} | _], _) when Identifier /= ClauseIdentifier ->
    ?FATAL(Line, ?ERR_CLAUSE_INCORRECT_NAME);

rice_clauses(_, Arity, [{'clause', Line, _, ClauseArity, _, _, _} | _], _) when Arity /= ClauseArity ->
    ?FATAL(Line, ?ERR_CLAUSE_INCORRECT_ARITY);

rice_clauses(Identifier, Arity, [{'clause', Line, Identifier, Arity, Args, Guards, Block} | Tail], Acc) ->
    rice_clauses(Identifier, Arity, Tail, [{'clause', Line, Args, Guards, Block} | Acc]).






rice_trim_left([], Acc) ->
    lists:reverse(Acc);

rice_trim_left([[_, Value] | Tail], Acc) ->
    rice_trim_left(Tail, [Value | Acc]).






rice_var([S | Tring]) ->
    list_to_atom([string:to_upper(S) | Tring]).






rice_atom(Atom, Line) ->
    {'atom', Line, Atom}.
