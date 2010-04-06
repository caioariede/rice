-module(peg2rice).
-compile(nowarn_unused_function).
-export([parse/1]).

-extends(peg2).

-define(p, ?BASE_MODULE).
-define(t(F), fun F/2).

% Rules

'root'(Input, Index) ->
    ?p:t_seq('root', Input, Index, [ ?t('module'), ?p:p_zero_or_more(?t('function')), ?p:p_eof() ],
    fun([Module, [], _], _) ->
        Module;
    ([Module, Functions, _], _) ->
        [Module | Functions]
    end).
 
'module'(Input, Index) ->
    ?p:t_seq('module', Input, Index, [ ?p:p_string("module"), ?t('spaces'), ?t('identifier') ],
    fun([_, _, Name], {{line, Line}, _}) ->
        {'attribute', Line, 'module', list_to_atom(Name)}
    end).

'function'(Input, Index) ->
    ?p:t_seq('functions', Input, Index, [ ?p:p_zero_or_more(?t('clause')), ?t('end') ],
    fun([Clauses = [{'clause', Line, Identifier} | _], _], _) ->
        {'function', Line, Identifier, 123, Clauses}
    end).

'clause'(Input, Index) ->
    ?p:t_seq('clause', Input, Index, [ ?t('samedent'), ?p:p_string("def"), ?t('spaces'), ?t('identifier'), ?t('statements'), ?t('dedent') ],
    fun([_, _, _, Identifier, Statements, _], {{line, Line}, _}) ->
        {'clause', Line, list_to_atom(Identifier)}
    end).

'statements'(Input, Index) ->
    ?p:t_seq('statements', Input, Index, [ ?t('indent'), ?t('statement'), ?p:p_zero_or_more(?t('statements_sm')) ],
    fun(Node, _) ->
        Node
    end).

'statements_sm'(Input, Index) ->
    ?p:t_seq('statements_sm', Input, Index, [ ?t('samedent'), ?t('statement') ],
    fun(Node, _) ->
        Node
    end).

'statement'(Input, Index) ->
    ?p:t_string('statement', Input, Index, "foobar",
    fun(Node, _) ->
        Node
    end).

'indent'(Input, Index) ->
    R = ?p:t_seq('indent', Input, Index, [ ?t('nl'), ?p:p_zero_or_more(?t('spaces')) ],
    fun(_, _) ->
        'indent'
    end),

    Stack = get('__stack'),

    case element(1, R) of
        match ->
            [_, Spaces] = ?p:lookahead(R),
            NewStack = length(Spaces),
            if NewStack > hd(Stack) ->
                put('__stack', [NewStack | Stack]),
                R;
            true ->
                SupposeStack = case tl(Stack) of
                    [] -> 4;
                    _ -> hd(tl(Stack))
                end,
                {fail, 0, Index, {expected, indent, [SupposeStack]}}
            end;
        _ ->
            R
    end.

'dedent'(Input, Index) ->
    R = ?p:t_seq('dedent', Input, Index, [ ?t('nl'), ?p:p_zero_or_more(?t('spaces')) ],
    fun(_, _) ->
        'dedent'
    end),

    [_ | [PrevStack | NewStack]] = get('__stack'),

    case element(1, R) of
        match ->
            [_, Spaces] = ?p:lookahead(R),
            if length(Spaces) == PrevStack ->
                put('__stack', NewStack),
                R;
            true ->
                {fail, 0, Index, {expected, dedent, [PrevStack]}}
            end;
        _ ->
            R
    end.

'samedent'(Input, Index) ->
    R = ?p:t_seq('samedent', Input, Index, [ ?t('nl'), ?p:p_zero_or_more(?t('spaces')) ],
    fun(_, _) ->
        'samedent'
    end),

    Stack = get('__stack'),

    case element(1, R) of
        match ->
            CurrentStack = hd(Stack),

            case ?p:lookahead(R) of
                ["\n", Spaces] when length(Spaces) == CurrentStack ->
                    R;
                _ ->
                    {fail, 0, Index, {expected, samedent, [CurrentStack]}}
            end;
        _ ->
            R
    end.

'identifier'(Input, Index) ->
    ?p:t_regex('identifier', Input, Index, "[a-zA-Z]+",
    fun(Node, _) ->
        Node
    end).

'spaces'(Input, Index) ->
    ?p:t_regex('spaces', Input, Index, "\s+",
    fun(_, _) ->
        'spaces'
    end).

'nl'(Input, Index) ->
    ?p:t_regex('nl', Input, Index, "\n",
    fun(_, _) ->
        'nl'
    end).

'end'(Input, Index) ->
    ?p:t_string('end', Input, Index, "end",
    fun(_, _) ->
        'end'
    end).

% Parsing functions

parse(Input) ->

    put('__stack', [0]),

    case 'root'(Input, {{line, 1}, {column, 1}}) of
        {match, AST, _, _, Transform} ->
            hd(?p:transform([AST]));
        {fail, _, Index, Expected} ->
            {fail, Index, Expected}
    end.