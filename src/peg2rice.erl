-module(peg2rice).
-compile(nowarn_unused_function).
-export([file/1,parse/1]).

-extends(peg2).

-define(p, ?BASE_MODULE).
-define(t(F), fun F/2).

% Rules

'root'(Input, Index) ->
    ?p:t_seq('root', Input, Index, [
        ?t('module'), ?p:p_zero_or_more(?t('function')), ?t('eof')
    ],

    fun ([Module, [], _], _) ->
        Module;

    ([Module, Functions, _], _) ->
        [Module | Functions]

    end).
 
'module'(Input, Index) ->
    ?p:t_seq('module', Input, Index, [
        ?p:p_string("module"), ?t('spaces'), ?t('identifier')
    ],

    fun ([_, _, Name], {{line, Line}, _}) ->
        {'attribute', Line, 'module', list_to_atom(Name)}

    end).

'function'(Input, Index) ->
    ?p:t_seq('function', Input, Index, [
        ?p:p_one_or_more(?t('clause')), ?t('end')
    ],

    fun ([Clauses = [{'clause', Line, Identifier, Args, _, _} | _], _], _) ->
        {'function', Line, Identifier, length(Args), Clauses}

    end).

'clause'(Input, Index) ->
    ?p:t_seq('clause', Input, Index, [

        ?p:p_unimportant(?t('samedent')),
        
        ?p:p_string("def"), ?t('spaces'), ?t('identifier'),
        
        ?p:p_optional(?p:p_or('clause_args_or_guards', [

            ?t('clause_guards'),

            ?p:p_seq([ ?t('clause_args'), ?t('clause_guards') ]),

            ?t('clause_args')

        ])),

        ?t('statements'),
        
        ?t('dedent')

    ],

    fun ([_, _, _, Identifier, {args, Args}, Statements, _], {{line, Line}, _}) ->
        {'clause', Line, list_to_atom(Identifier), Args, [], Statements};

    ([_, _, _, Identifier, [{args, Args}, {guards, Guards}], Statements, _], {{line, Line}, _}) ->
        {'clause', Line, list_to_atom(Identifier), Args, Guards, Statements};

    ([_, _, _, Identifier, {guards, Guards}, Statements, _], {{line, Line}, _}) ->
        {'clause', Line, list_to_atom(Identifier), [], Guards, Statements}

    end).

'clause_args'(Input, Index) ->
    ?p:t_seq('clause_args', Input, Index, [
    
        ?p:p_optional(?t('spaces')),
        
        ?p:p_string("("),
        
        ?p:p_optional(?p:p_seq([
        
            ?t('clause_args_arg'),
            
            ?p:p_zero_or_more(?p:p_seq([ ?t('comma'), ?t('clause_args_arg') ]))
            
        ])),
        
        ?p:p_string(")")
        
    ],

    fun ([_, _, [Arg, Args], _], _) ->
        {args, [Arg | util_trim_left(Args)]};

    (_, _) ->
        []

    end).

'clause_args_arg'(Input, Index) ->
    ?p:t_or('clause_args_arg', Input, Index, [ ?t('atom'), ?t('number'), ?t('string') ],
    fun(Node, _) ->
        Node
    end).

'clause_guards'(Input, Index) ->
    ?p:t_seq('clause_guards', Input, Index, [

        ?t('spaces'), ?p:p_string("when"), ?t('spaces'),

        ?p:p_optional(?p:p_seq([

            ?t('clause_guards_guard'),

            ?p:p_zero_or_more(?p:p_seq([ ?t('comma'), ?t('clause_guards_guard') ]))

        ]))

    ],

    fun ([_, _, _, [Guard, Guards]], _) ->
        {guards, [Guard | util_trim_left(Guards)]};

    (_, _) ->
        []

    end).

'clause_guards_guard'(Input, Index) ->
    ?p:t_or('clause_guards_guard', Input, Index, [ ?t('atom'), ?t('number'), ?t('string') ],
    fun(Node, _) ->
        Node
    end).

'statements'(Input, Index) ->
    ?p:t_seq('statements', Input, Index, [ ?t('indent'), ?t('statement'), ?p:p_zero_or_more(?t('statements_sm')) ],
    fun([_, Statement, Statements], _) ->
        [Statement | Statements]
    end).

'statements_sm'(Input, Index) ->
    ?p:t_seq('statements_sm', Input, Index, [ ?t('samedent'), ?t('statement') ],
    fun([_, Statement], _) ->
        Statement
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
                {fail, 0, Index, {expected, indent, SupposeStack, NewStack}}
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
            NumberOfSpaces = length(Spaces),
            if NumberOfSpaces == PrevStack ->
                put('__stack', NewStack),
                R;
            true ->
                {fail, 0, Index, {expected, dedent, PrevStack, NumberOfSpaces}}
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
            CurrentStack = case Stack of
                [] -> 0;
                 _ -> hd(Stack)
            end,

            case ?p:lookahead(R) of
                ["\n", Spaces] when length(Spaces) == CurrentStack ->
                    R;
                [_, Spaces] ->
                    {fail, length(Spaces), Index, {expected, samedent, CurrentStack, length(Spaces)}}
            end;
        _ ->
            R
    end.

'string'(Input, Index) ->
    ?p:t_seq('string', Input, Index, [
        
        ?p:p_string("\""),
        
        ?p:p_regex("[^\"]+"),
        
        ?p:p_string("\"")
            
    ],
    fun([_, String, _], {{line, Line}, _}) ->
        {string, Line, String}
    end).

'atom'(Input, Index) ->
    ?p:t_or('atom', Input, Index, [

        ?p:p_seq([ ?p:p_string(":"), ?t('identifier') ]),

        ?p:p_seq([ ?p:p_string("'"), ?p:p_one_or_more(?p:p_not(?p:p_string("'"))), ?p:p_string("'") ])

    ],

    fun([":", Name], {{line, Line}, _}) ->
        {'atom', Line, list_to_atom(Name)};

    ([_, Name, _], {{line, Line}, _}) ->
        {'atom', Line, list_to_atom(Name)}

    end).

'number'(Input, Index) ->
    ?p:t_or('number', Input, Index, [ ?t('float'), ?t('integer') ],
    fun(Node, _) ->
        Node
    end).

'float'(Input, Index) ->
    ?p:t_regex('float', Input, Index, "[0-9]*\.[0-9]+",

    fun([$.|_] = Node, {{line, Line}, _}) ->
        {float, Line, list_to_float("0" ++ Node)};

    (Node, {{line, Line}, _}) ->
        {float, Line, list_to_float(Node)}

    end).

'integer'(Input, Index) ->
    ?p:t_regex('integer', Input, Index, "[0-9]+",

    fun(Node, {{line, Line}, _}) ->
        {integer, Line, list_to_integer(Node)}

    end).

'identifier'(Input, Index) ->
    ?p:t_regex('identifier', Input, Index, "[a-zA-Z]+",
    fun(Node, _) ->
        Node
    end).

'spaces'(Input, Index) ->
    ?p:t_regex('spaces', Input, Index, "[\s\t]+",
    fun(_, _) ->
        'spaces'
    end).

'comma'(Input, Index) ->
    ?p:t_regex('comma', Input, Index, "[\s\t]*,[\s\t]*",
    fun(_, _) ->
        'comma'
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

'eof'(Input, Index) ->
    ?p:t_seq('eof', Input, Index, [

        ?p:p_optional(?p:p_regex("[\r\n\t\s]+")),

        ?p:p_eof()

    ],
    fun(_, _) ->
        'eof'
    end).

% Utils

util_trim_left([]) ->
    [];

util_trim_left([[_ | H] | Tail]) ->
    [H | util_trim_left(Tail)].

% Parsing functions

file(Filepath) ->
    {ok, Data} = file:read_file(Filepath),
    parse(binary_to_list(Data)).

parse(Input) ->

    put('__stack', [0]),

    case 'root'(Input, {{line, 1}, {column, 1}}) of
        {match, AST, _, _, _} ->
            hd(?p:transform([AST]));
        {fail, _, Index, Expected} ->
            {fail, Index, Expected}
    end.
