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

    fun ([_, _, {identifier, Name}], {{line, Line}, _}) ->
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

        ?p:p_unimportant(?p:p_one_or_more(?t('samedent'))),
        
        ?p:p_string("def"), ?t('spaces'), ?t('identifier'),
        
        ?p:p_optional(?p:p_or('clause_args_or_guards', [

            ?t('clause_guards'),

            ?p:p_seq([ ?t('clause_args'), ?t('clause_guards') ]),

            ?t('clause_args')

        ])),

        ?t('statements'),
        
        ?t('dedent')

    ],

    fun ([_, _, _, {identifier, Identifier}, {args, Args}, Statements, _], {{line, Line}, _}) ->
        {'clause', Line, list_to_atom(Identifier), Args, [], Statements};

    ([_, _, _, {identifier, Identifier}, [{args, Args}, {guards, Guards}], Statements, _], {{line, Line}, _}) ->
        {'clause', Line, list_to_atom(Identifier), Args, Guards, Statements};

    ([_, _, _, {identifier, Identifier}, {guards, Guards}, Statements, _], {{line, Line}, _}) ->
        {'clause', Line, list_to_atom(Identifier), [], Guards, Statements};

    ([_, _, _, {identifier, Identifier}, [], Statements, _], {{line, Line}, _}) ->
        {'clause', Line, list_to_atom(Identifier), [], [], Statements}

    end).

'clause_args'(Input, Index) ->
    ?p:t_seq('clause_args', Input, Index, [
    
        ?p:p_optional(?t('spaces')),
        
        ?p:p_string("("),
        
        ?p:p_optional(?t('spaces')),

        ?p:p_optional(?p:p_seq([
        
            ?t('clause_args_arg'),
            
            ?p:p_zero_or_more(?p:p_seq([ ?t('comma'), ?t('clause_args_arg') ]))
            
        ])),
        
        ?p:p_optional(?t('spaces')),

        ?p:p_string(")")
        
    ],

    fun ([_, _, _, [Arg, Args], _, _], _) ->
        {args, [Arg | util_trim_left(Args)]};

    (_, _) ->
        []

    end).

'clause_args_arg'(Input, Index) ->
    Result = ?p:t_or('clause_args_arg', Input, Index, [ ?t('op'), ?t('atom'), ?t('number'), ?t('string') ],
    fun(Node, _) ->
        Node
    end),

    case element(1, Result) of
        match ->
            Arg = ?p:lookahead(Result, transform),
            case check_for_argument(Arg) of
                ok ->
                    Result;
                NotAllowed ->
                    {fail, 1, Index, {expected, valid_argument, [], NotAllowed}}
            end;
        _ ->
            Result
    end.

'clause_guards'(Input, Index) ->
    ?p:t_seq('clause_guards', Input, Index, [

        ?p:p_unimportant(?t('spaces')),
        
        ?p:p_string("when"),
        
        ?t('spaces'),

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
    Result = ?p:t_or('clause_guards_guard', Input, Index, [ ?t('op'), ?t('atom'), ?t('number'), ?t('string') ],
    fun(Node, _) ->
        Node
    end),

    case element(1, Result) of
        match ->
            Arg = ?p:lookahead(Result, transform),
            case check_for_bif(Arg) of
                ok ->
                    Result;
                NotAllowed ->
                    {fail, 1, Index, {expected, valid_argument_or_bif, [], NotAllowed}}
            end;
        _ ->
            Result
    end.

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
    ?p:t_or('statement', Input, Index, [
        ?t('atom'), ?t('number'), ?t('string'), ?t('identifier')
    ],

    fun({identifier, Name}, {{line, Line}, _}) ->
        {var, Line, list_to_atom(Name)};

    (Node, _) ->
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
        
        ?p:p_regex("[^\"]*"),
        
        ?p:p_string("\"")
            
    ],
    fun([_, String, _], {{line, Line}, _}) ->
        {string, Line, String}
    end).

'atom'(Input, Index) ->
    ?p:t_or('atom', Input, Index, [

        ?p:p_seq([ ?p:p_string("'"), ?p:p_regex("[^'\n]*"), ?p:p_string("'") ]),

        ?p:p_seq([ ?p:p_string("'"), ?t('identifier') ])

    ],

    fun(["'", {identifier, Name}], {{line, Line}, _}) ->
        {'atom', Line, list_to_atom(Name)};

    ([_, Name, _], {{line, Line}, _}) ->
        {'atom', Line, list_to_atom(Name)}

    end).

'op'(Input, Index) ->
    (?t('op_mult'))(Input, Index).

'op_mult'(Input, Index) ->
    ('operator'('op_multi', ?t('op_div'), ?p:p_string("*"), '*'))(Input, Index).

'op_div'(Input, Index) ->
    ('operator'('op_div', ?t('op_mod'), ?p:p_string("/"), '/'))(Input, Index).

'op_mod'(Input, Index) ->
    ('operator'('op_mod', ?t('op_sum'), ?p:p_string("%"), 'rem'))(Input, Index).

'op_sum'(Input, Index) ->
    ('operator'('op_sum', ?t('op_sub'), ?p:p_string("+"), '+'))(Input, Index).

'op_sub'(Input, Index) ->
    ('operator'('op_sub', ?t('number'), ?p:p_string("-"), '-'))(Input, Index).

'number'(Input, Index) ->
    ?p:t_or('number', Input, Index, [ ?t('float'), ?t('integer') ],
    fun(Node, _) ->
        Node
    end).

'float'(Input, Index) ->
    ?p:t_regex('float', Input, Index, "[0-9]*\\.[0-9]+",

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
        {identifier, Node}
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

% Parsing shortcuts

'operator'(Name, Left, Op, Transform) when is_function(Transform) ->
    fun(Input, Index) ->
        ?p:t_or(Name, Input, Index, [
            ?p:p_seq([ Left, ?p:p_optional(?t('spaces')), Op, ?p:p_optional(?t('spaces')), ?t('number') ]),
            Left
        ], Transform)
    end;

'operator'(Name, Left, Op, NewOp) ->
    'operator'(Name, Left, Op, fun([Operand1, _, _, _, Operand2], {{line, Line}, _}) ->
        {'op', Line, NewOp, Operand1, Operand2};
    (Operand, _) ->
        Operand
    end).

'func_call'({Module, Function}, Line, Args) ->
    {'call', Line, {'remote', Line, {'atom', Line, Module}, {'atom', Line, Function}, Args}};

'func_call'(Function, Line, Args) ->
    {'call', Line, {'atom', Line, Function}, Args}.

% Utils

util_trim_left([]) ->
    [];

util_trim_left([[_ | H] | Tail]) ->
    [H | util_trim_left(Tail)].

check_for_bif(Arg) ->
    case check_for_argument(Arg) of
        ok ->
            ok;
        NotAllowed ->
            NotAllowed
    end.

check_for_argument(Arg) when (element(1, Arg) =:= 'integer')
                          or (element(1, Arg) =:= 'float')
                          or (element(1, Arg) =:= 'op')
                          or (element(1, Arg) =:= 'atom')
                          or (element(1, Arg) =:= 'string') ->
    ok;

check_for_argument(Arg) ->
    Arg.

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
