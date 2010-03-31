-module(peg2rice).
-compile(nowarn_unused_function).
-export([parse/1]).

-extends(peg2).

-define(p, ?BASE_MODULE).
-define(t(F), fun F/2).

% Rules

'root'(Input, Index) ->
    ?p:t_seq('root', Input, Index, [ ?t('module'), ?p:p_or([ ?p:p_seq([ ?t('nl'), ?t('end') ]), ?p:p_zero_or_more(?t('functions')) ]) ]).

'module'(Input, Index) ->
    ?p:t_seq('module', Input, Index, [ ?p:p_string("module"), ?t('spaces'), ?t('identifier') ]).

'functions'(Input, Index) ->
    ?p:t_seq('functions', Input, Index, [ ?t('clause'), ?t('end') ]).

'clause'(Input, Index) ->
    ?p:t_seq('clause', Input, Index, [ ?t('samedent'), ?p:p_string("def"), ?t('spaces'), ?t('identifier'), ?t('statements'), ?t('dedent') ]).

'statements'(Input, Index) ->
    ?p:t_seq('statements', Input, Index, [ ?t('indent'), ?t('statement'), ?p:p_zero_or_more(?t('statements_sm')) ]).

'statements_sm'(Input, Index) ->
    ?p:t_seq('statements_sm', Input, Index, [ ?t('samedent'), ?t('statement') ]).

'statement'(Input, Index) ->
    ?p:t_string('statement', Input, Index, "caio").

'indent'(Input, Index) ->
    R = ?p:t_seq('indent', Input, Index, [ ?t('nl'), ?p:p_zero_or_more(?t('spaces')) ]),

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
    R = ?p:t_seq('dedent', Input, Index, [ ?t('nl'), ?p:p_zero_or_more(?t('spaces')) ]),

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
    R = ?p:t_seq('samedent', Input, Index, [ ?t('nl'), ?p:p_zero_or_more(?t('spaces')) ]),

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
    ?p:t_regex('identifier', Input, Index, "[a-zA-Z]+").

'spaces'(Input, Index) ->
    ?p:t_regex('spaces', Input, Index, "[\\s\\t]+").

'nl'(Input, Index) ->
    ?p:t_regex('nl', Input, Index, "\\n").

'eof'(Input, Index) ->
    ?p:t_not('eof', Input, Index, ?p:p_string("end")).

'end'(Input, Index) ->
    ?p:t_string('end', Input, Index, "end").

% Transformation

transform('root', Node, _) ->
    Node;

transform('module', [_, _, Module], _) ->
    {attribute, 1, list_to_atom(Module)};

transform(_, Node, _) ->
    Node.

% Parsing functions

parse(Input) ->

    put('__stack', [0]),

    case 'root'(Input, {{line, 1}, {column, 1}}) of
        {match, AST, _, _} ->
            [Transformed] = ?p:transform([AST], fun transform/3),
            Transformed;
        {fail, _, Index, Expected} ->
            {fail, Index, Expected}
    end.
