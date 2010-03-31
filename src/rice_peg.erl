-module(rice_peg).
-export([parse/1,file/1]).
-compile(nowarn_unused_vars).
-compile({nowarn_unused_function,[p/4, p/5, p_eof/0, p_optional/1, p_not/1, p_assert/1, p_seq/1, p_and/1, p_choose/1, p_zero_or_more/1, p_one_or_more/1, p_label/2, p_string/1, p_anything/0, p_charclass/1, line/1, column/1]}).



file(Filename) -> {ok, Bin} = file:read_file(Filename), parse(binary_to_list(Bin)).

parse(Input) ->

    setup_memo(),

    Result = case 'root'(Input,{{line,1},{column,1}}) of
        {AST, [], _Index} -> AST;
        Any -> Any
    end,

    release_memo(),

    case Result of
        {fail, _} = Failure ->
            exit(Failure);
        _ ->
            Result
    end.

'root'(Input, Index) ->
    p(Input, Index, 'root', fun(I,D) -> (p_seq([

        % module attribute
        fun 'module'/2,

        % export attribute
        p_optional(fun 'export'/2),

        % functions
        fun 'functions'/2,

        % garbage end of file
        p_optional(p_choose([fun 'spaces'/2, fun 'newline'/2]))

    ]))(I,D) end, fun(Node, Idx) -> transform('root', Node, Idx) end).

'module'(Input, Index) ->
    p(Input, Index, 'module', fun(I,D) -> (p_seq([

        % "module" spaces identifier
        p_string("module"), fun 'spaces'/2, fun 'identifier'/2

    ]))(I,D) end, fun(Node, Idx) -> transform('module', Node, Idx) end).

'export'(Input, Index) ->
    p(Input, Index, 'export', fun(I,D) -> (p_seq(
    [
        % export attribute
        fun 'newline'/2,
        % "export" spaces export_function (comma export_function)*
        p_string("export"), fun 'spaces'/2, fun 'export_function'/2, p_zero_or_more(p_seq([fun 'comma'/2, fun 'export_function'/2]))

    ]))(I,D) end, fun(Node, Idx) -> transform('export', Node, Idx) end).

'export_function'(Input, Index) ->
    p(Input, Index, 'export_function', fun(I,D) -> (p_seq(
    [

        % identifier "/" integer
        fun 'identifier'/2, p_string("/"), fun 'integer'/2

    ]))(I,D) end, fun(Node, Idx) -> transform('export_function', Node, Idx) end).

'functions'(Input, Index) ->
    p(Input, Index, 'functions', fun(I,D) ->
   
        % function
        (p_zero_or_more(match, fun 'function'/2))(I,D)

    end, fun(Node, Idx) -> transform('functions', Node, Idx) end).

'function'(Input, Index) ->
    p(Input, Index, 'function', fun(I,D) -> (p_seq([

        % function: first clause
        fun 'clause'/2,
        
        % function: other clauses
        p_zero_or_more(match, fun 'clause'/2),
        
        % dedent
        fun 'dedent'/2,
        
        % function: end
        p_string("end")

    ]))(I,D) end, fun(Node, Idx) -> transform('function', Node, Idx) end).

'clause'(Input, Index) ->
    p(Input, Index, 'clause', fun(I,D) -> (p_seq_match([

        fun 'indent'/2,
        
        % "def" spaces identifier
        p_string("def"), fun 'spaces'/2, fun 'identifier'/2,
        
        % ((clause_args clause_guards) / clause_guards / clause_args)?
        p_optional(p_choose([p_seq([fun 'clause_args'/2, fun 'clause_guards'/2]), fun 'clause_guards'/2, fun 'clause_args'/2])),

        % block / block_inline
        p_choose([fun 'block'/2, fun 'block_inline'/2])
        
    ]))(I,D) end, fun(Node, Idx) -> transform('clause', Node, Idx) end).

'clause_args'(Input, Index) ->
    p(Input, Index, 'clause_args', fun(I,D) -> (p_seq([fun 'spaces'/2, p_choose([p_seq([fun 'p_open'/2, p_seq([fun 'clause_args_arg'/2, p_zero_or_more(p_seq([fun 'comma'/2, fun 'clause_args_arg'/2]))]), fun 'p_close'/2]), p_seq([fun 'clause_args_arg'/2, p_zero_or_more(p_seq([fun 'comma'/2, fun 'clause_args_arg'/2]))])])]))(I,D) end, fun(Node, Idx) -> transform('clause_args', Node, Idx) end).

'clause_args_arg'(Input, Index) ->
    p(Input, Index, 'clause_args_arg', fun(I,D) -> (p_choose([fun 'atom'/2, fun 'string'/2, fun 'integer'/2, fun 'identifier'/2]))(I,D) end, fun(Node, Idx) -> transform('clause_args_arg', Node, Idx) end).

'clause_guards'(Input, Index) ->
    p(Input, Index, 'clause_guards', fun(I,D) -> (p_seq([fun 'spaces'/2, p_string("when"), fun 'spaces'/2, fun 'clause_guards_guard'/2, p_zero_or_more(p_seq([fun 'comma'/2, fun 'clause_guards_guard'/2]))]))(I,D) end, fun(Node, Idx) -> transform('clause_guards', Node, Idx) end).

'clause_guards_guard'(Input, Index) ->
    p(Input, Index, 'clause_guards_guard', fun(I,D) -> (fun 'op'/2)(I,D) end, fun(Node, Idx) -> transform('clause_guards_guard', Node, Idx) end).

'p_open'(Input, Index) ->
    p(Input, Index, 'p_open', fun(I,D) -> (p_seq([p_string("("), p_optional(fun 'spaces'/2)]))(I,D) end, fun(Node, Idx) -> transform('p_open', Node, Idx) end).

'p_close'(Input, Index) ->
    p(Input, Index, 'p_close', fun(I,D) -> (p_seq([p_optional(fun 'spaces'/2), p_string(")")]))(I,D) end, fun(Node, Idx) -> transform('p_close', Node, Idx) end).

'block'(Input, Index) ->
    p(Input, Index, 'block', fun(I,D) -> (p_seq([fun 'indent'/2, fun 'statements'/2]))(I,D) end, fun(Node, Idx) -> transform('block', Node, Idx) end).

'block_inline'(Input, Index) ->
    p(Input, Index, 'block_inline', fun(I,D) -> (p_seq([p_string(":"), p_optional(fun 'spaces'/2), fun 'statements_inline'/2]))(I,D) end, fun(Node, Idx) -> transform('block_inline', Node, Idx) end).

'statements'(Input, Index) ->
    p(Input, Index, 'statements', fun(I,D) -> (p_seq([

        % statement statements_samedent*
        fun 'statement'/2, p_optional(fun 'statements_samedent'/2)

    ]))(I,D) end, fun(Node, Idx) -> transform('statements', Node, Idx) end).

'statements_samedent'(Input, Index) ->
     p(Input, Index, 'statements_samedent', fun(I,D) -> (p_seq([

        % samedent statements+
        fun 'samedent'/2, p_one_or_more(fun 'statements'/2)

    ]))(I,D) end, fun(Node, Idx) -> transform('statements_samedent', Node, Idx) end).

'statements_inline'(Input, Index) ->
    p(Input, Index, 'statements_inline', fun(I,D) -> (p_seq([fun 'statement'/2, p_zero_or_more(p_seq([fun 'comma'/2, fun 'statement'/2]))]))(I,D) end, fun(Node, Idx) -> transform('statements_inline', Node, Idx) end).

'statement'(Input, Index) ->
    p(Input, Index, 'statement', fun(I,D) -> (p_choose([
        fun 'assign_op'/2, fun 'case'/2, fun 'call'/2, fun 'op'/2
    ]))(I,D) end, fun(Node, Idx) -> transform('statement', Node, Idx) end).

'op'(Input, Index) ->
    p(Input, Index, 'op', fun(I,D) -> (p_choose([fun 'op_identical'/2, fun 'op_not'/2]))(I,D) end, fun(Node, Idx) -> transform('op', Node, Idx) end).

'op_identical'(Input, Index) ->
    p(Input, Index, 'op_identical', fun(I,D) -> (p_choose([p_seq([fun 'op_equal'/2, p_optional(fun 'spaces'/2), p_string("==="), p_optional(fun 'spaces'/2), fun 'op_identical'/2]), fun 'op_equal'/2]))(I,D) end, fun(Node, Idx) -> transform('op_identical', Node, Idx) end).

'op_equal'(Input, Index) ->
    p(Input, Index, 'op_equal', fun(I,D) -> (p_choose([p_seq([fun 'op_not_identical'/2, p_optional(fun 'spaces'/2), p_string("=="), p_optional(fun 'spaces'/2), fun 'op_equal'/2]), fun 'op_not_identical'/2]))(I,D) end, fun(Node, Idx) -> transform('op_equal', Node, Idx) end).

'op_not_identical'(Input, Index) ->
    p(Input, Index, 'op_not_identical', fun(I,D) -> (p_choose([p_seq([fun 'op_not_equal'/2, p_optional(fun 'spaces'/2), p_string("!=="), p_optional(fun 'spaces'/2), fun 'op_not_identical'/2]), fun 'op_not_equal'/2]))(I,D) end, fun(Node, Idx) -> transform('op_not_identical', Node, Idx) end).

'op_not_equal'(Input, Index) ->
    p(Input, Index, 'op_not_equal', fun(I,D) -> (p_choose([p_seq([fun 'op_less'/2, p_optional(fun 'spaces'/2), p_string("!="), p_optional(fun 'spaces'/2), fun 'op_not_equal'/2]), fun 'op_less'/2]))(I,D) end, fun(Node, Idx) -> transform('op_not_equal', Node, Idx) end).

'op_less'(Input, Index) ->
    p(Input, Index, 'op_less', fun(I,D) -> (p_choose([p_seq([fun 'op_greater'/2, p_optional(fun 'spaces'/2), p_string("<"), p_optional(fun 'spaces'/2), fun 'op_less'/2]), fun 'op_greater'/2]))(I,D) end, fun(Node, Idx) -> transform('op_less', Node, Idx) end).

'op_greater'(Input, Index) ->
    p(Input, Index, 'op_greater', fun(I,D) -> (p_choose([p_seq([fun 'op_less_equal'/2, p_optional(fun 'spaces'/2), p_string(">"), p_optional(fun 'spaces'/2), fun 'op_greater'/2]), fun 'op_less_equal'/2]))(I,D) end, fun(Node, Idx) -> transform('op_greater', Node, Idx) end).

'op_less_equal'(Input, Index) ->
    p(Input, Index, 'op_less_equal', fun(I,D) -> (p_choose([p_seq([fun 'op_greater_equal'/2, p_optional(fun 'spaces'/2), p_string("<="), p_optional(fun 'spaces'/2), fun 'op_less_equal'/2]), fun 'op_greater_equal'/2]))(I,D) end, fun(Node, Idx) -> transform('op_less_equal', Node, Idx) end).

'op_greater_equal'(Input, Index) ->
    p(Input, Index, 'op_greater_equal', fun(I,D) -> (p_choose([p_seq([fun 'op_and'/2, p_optional(fun 'spaces'/2), p_string(">="), p_optional(fun 'spaces'/2), fun 'op_greater_equal'/2]), fun 'op_and'/2]))(I,D) end, fun(Node, Idx) -> transform('op_greater_equal', Node, Idx) end).

'op_not'(Input, Index) ->
    p(Input, Index, 'op_not', fun(I,D) -> (p_choose([p_seq([p_string("not"), p_optional(fun 'spaces'/2), fun 'op_not'/2]), fun 'op_and'/2]))(I,D) end, fun(Node, Idx) -> transform('op_not', Node, Idx) end).

'op_and'(Input, Index) ->
    p(Input, Index, 'op_and', fun(I,D) -> (p_choose([p_seq([fun 'op_or'/2, p_optional(fun 'spaces'/2), p_string("and"), p_optional(fun 'spaces'/2), fun 'op_and'/2]), fun 'op_or'/2]))(I,D) end, fun(Node, Idx) -> transform('op_and', Node, Idx) end).

'op_or'(Input, Index) ->
    p(Input, Index, 'op_or', fun(I,D) -> (p_choose([p_seq([fun 'op_xor'/2, p_optional(fun 'spaces'/2), p_string("or"), p_optional(fun 'spaces'/2), fun 'op_or'/2]), fun 'op_xor'/2]))(I,D) end, fun(Node, Idx) -> transform('op_or', Node, Idx) end).

'op_xor'(Input, Index) ->
    p(Input, Index, 'op_xor', fun(I,D) -> (p_choose([p_seq([fun 'op_add'/2, p_optional(fun 'spaces'/2), p_string("+"), p_optional(fun 'spaces'/2), fun 'op_xor'/2]), fun 'op_add'/2]))(I,D) end, fun(Node, Idx) -> transform('op_xor', Node, Idx) end).

'op_add'(Input, Index) ->
    p(Input, Index, 'op_add', fun(I,D) -> (p_choose([p_seq([fun 'op_sub'/2, p_optional(fun 'spaces'/2), p_string("+"), p_optional(fun 'spaces'/2), fun 'op_add'/2]), fun 'op_sub'/2]))(I,D) end, fun(Node, Idx) -> transform('op_add', Node, Idx) end).

'op_sub'(Input, Index) ->
    p(Input, Index, 'op_sub', fun(I,D) -> (p_choose([p_seq([fun 'op_pow'/2, p_optional(fun 'spaces'/2), p_string("-"), p_optional(fun 'spaces'/2), fun 'op_sub'/2]), fun 'op_pow'/2]))(I,D) end, fun(Node, Idx) -> transform('op_sub', Node, Idx) end).

'op_pow'(Input, Index) ->
    p(Input, Index, 'op_pow', fun(I,D) -> (p_choose([p_seq([fun 'op_mul'/2, p_optional(fun 'spaces'/2), p_string("**"), p_optional(fun 'spaces'/2), fun 'op_pow'/2]), fun 'op_mul'/2]))(I,D) end, fun(Node, Idx) -> transform('op_pow', Node, Idx) end).

'op_mul'(Input, Index) ->
    p(Input, Index, 'op_mul', fun(I,D) -> (p_choose([p_seq([fun 'op_div'/2, p_optional(fun 'spaces'/2), p_string("*"), p_optional(fun 'spaces'/2), fun 'op_mul'/2]), fun 'op_div'/2]))(I,D) end, fun(Node, Idx) -> transform('op_mul', Node, Idx) end).

'op_div'(Input, Index) ->
    p(Input, Index, 'op_div', fun(I,D) -> (p_choose([p_seq([fun 'op_mod'/2, p_optional(fun 'spaces'/2), p_string("/"), p_optional(fun 'spaces'/2), fun 'op_div'/2]), fun 'op_mod'/2]))(I,D) end, fun(Node, Idx) -> transform('op_div', Node, Idx) end).

'op_mod'(Input, Index) ->
    p(Input, Index, 'op_mod', fun(I,D) -> (p_choose([p_seq([fun 'op_band'/2, p_optional(fun 'spaces'/2), p_string("%"), p_optional(fun 'spaces'/2), fun 'op_mod'/2]), fun 'op_band'/2]))(I,D) end, fun(Node, Idx) -> transform('op_mod', Node, Idx) end).

'op_band'(Input, Index) ->
    p(Input, Index, 'op_band', fun(I,D) -> (p_choose([p_seq([fun 'op_bor'/2, p_optional(fun 'spaces'/2), p_string("&"), p_optional(fun 'spaces'/2), fun 'op_band'/2]), fun 'op_bor'/2]))(I,D) end, fun(Node, Idx) -> transform('op_band', Node, Idx) end).

'op_bor'(Input, Index) ->
    p(Input, Index, 'op_bor', fun(I,D) -> (p_choose([p_seq([fun 'op_bxor'/2, p_optional(fun 'spaces'/2), p_string("|"), p_optional(fun 'spaces'/2), fun 'op_bor'/2]), fun 'op_bxor'/2]))(I,D) end, fun(Node, Idx) -> transform('op_bor', Node, Idx) end).

'op_bxor'(Input, Index) ->
    p(Input, Index, 'op_bxor', fun(I,D) -> (p_choose([p_seq([fun 'op_bsl'/2, p_optional(fun 'spaces'/2), p_string("^"), p_optional(fun 'spaces'/2), fun 'op_bxor'/2]), fun 'op_bsl'/2]))(I,D) end, fun(Node, Idx) -> transform('op_bxor', Node, Idx) end).

'op_bsl'(Input, Index) ->
    p(Input, Index, 'op_bsl', fun(I,D) -> (p_choose([p_seq([fun 'op_bsr'/2, p_optional(fun 'spaces'/2), p_string("<<"), p_optional(fun 'spaces'/2), fun 'op_bsl'/2]), fun 'op_bsr'/2]))(I,D) end, fun(Node, Idx) -> transform('op_bsl', Node, Idx) end).

'op_bsr'(Input, Index) ->
    p(Input, Index, 'op_bsr', fun(I,D) -> (p_choose([p_seq([fun 'expr'/2, p_optional(fun 'spaces'/2), p_string(">>"), p_optional(fun 'spaces'/2), fun 'op_band'/2]), fun 'expr'/2]))(I,D) end, fun(Node, Idx) -> transform('op_bsr', Node, Idx) end).

'call'(Input, Index) ->
    p(Input, Index, 'call', fun(I,D) -> (p_choose([p_seq([p_choose([fun 'call_value'/2, fun 'identifier'/2]), p_seq([fun 'spaces'/2, fun 'call_args'/2])]), fun 'unary_call'/2]))(I,D) end, fun(Node, Idx) -> transform('call', Node, Idx) end).

'call_value'(Input, Index) ->
    p(Input, Index, 'call_value', fun(I,D) -> (p_seq([fun 'call_value_value'/2, p_one_or_more(p_seq([p_string("."), fun 'identifier'/2]))]))(I,D) end, fun(Node, Idx) -> transform('call_value', Node, Idx) end).

'call_value_value'(Input, Index) ->
    p(Input, Index, 'call_value_value', fun(I,D) -> (p_choose([p_seq([p_string("("), p_optional(fun 'spaces'/2), p_choose([fun 'call'/2, fun 'op'/2]), p_optional(fun 'spaces'/2), p_string(")")]), fun 'primary'/2]))(I,D) end, fun(Node, Idx) -> transform('call_value_value', Node, Idx) end).

'call_args'(Input, Index) ->
    p(Input, Index, 'call_args', fun(I,D) -> (p_seq([fun 'call_args_arg'/2, p_zero_or_more(p_seq([fun 'comma'/2, fun 'call_args_arg'/2]))]))(I,D) end, fun(Node, Idx) -> transform('call_args', Node, Idx) end).

'call_args_arg'(Input, Index) ->
    p(Input, Index, 'call_args_arg', fun(I,D) -> (p_choose([fun 'call'/2, fun 'op'/2]))(I,D) end, fun(Node, Idx) -> transform('call_args_arg', Node, Idx) end).

'case'(Input, Index) ->
    p(Input, Index, 'case', fun(I,D) -> (p_seq([p_string("case"), fun 'spaces'/2, fun 'primary'/2, fun 'case_clause'/2, fun 'indent'/2, p_string("end")]))(I,D) end, fun(Node, Idx) -> transform('case', Node, Idx) end).

'case_clause'(Input, Index) ->
    p(Input, Index, 'case_clause', fun(I,D) -> (p_seq([fun 'indent'/2, fun 'match'/2, fun 'indent'/2, fun 'statements'/2]))(I,D) end, fun(Node, Idx) -> transform('case_clause', Node, Idx) end).

'match'(Input, Index) ->
    p(Input, Index, 'match', fun(I,D) -> (fun 'primary'/2)(I,D) end, fun(Node, Idx) -> transform('match', Node, Idx) end).

'unary_call'(Input, Index) ->
    p(Input, Index, 'unary_call', fun(I,D) -> (fun 'call_value'/2)(I,D) end, fun(Node, Idx) -> transform('unary_call', Node, Idx) end).

'expr'(Input, Index) ->
    p(Input, Index, 'expr', fun(I,D) -> (p_choose([p_seq([p_string("("), p_optional(fun 'spaces'/2), p_choose([fun 'call'/2, fun 'op'/2]), p_optional(fun 'spaces'/2), p_string(")")]), fun 'call'/2, fun 'primary'/2]))(I,D) end, fun(Node, Idx) -> transform('expr', Node, Idx) end).

'assign_op'(Input, Index) ->
    p(Input, Index, 'assign_op', fun(I,D) -> (p_seq([fun 'identifier'/2, p_optional(fun 'spaces'/2), p_string("="), p_optional(fun 'spaces'/2), fun 'op'/2]))(I,D) end, fun(Node, Idx) -> transform('assign_op', Node, Idx) end).

'newline'(Input, Index) ->
    p(Input, Index, 'newline', fun(I,D) -> (p_one_or_more(p_charclass("[\r\n]")))(I,D) end, fun(Node, Idx) -> transform('newline', Node, Idx) end).

'indent'(Input, Index) ->
    Stack = case get(indent_stack) of
        undefined -> [0];
        Else -> Else
    end,
    MatchIndent = fun(_, _) ->
        {ok, RE} = re:compile("^\n+([\t\s]*)"),
        case re:run(Input, RE) of
            {match, [_, {S, Indent}]} ->
                Matched = string:substr(Input, S, Indent + 1),
                Tail = string:substr(Input, S + Indent + 1),
                NewIndex = p_advance_index(Matched, Index),
                if Indent < hd(Stack) ->
                    {fail, {expected, indent, NewIndex}};
                true ->
                    put(indent_stack, [Indent | Stack]),
                    {Matched, Tail, p_advance_index(Matched, NewIndex)}
                end;
            _ ->
                {fail, {expected, indent, Index}}
        end
    end,
    p(Input, Index, 'indent', fun(I,D) -> (p_seq([MatchIndent]))(I,D) end, fun(Node, Idx) -> transform('indent', Node, Idx) end).

'dedent'(Input, Index) ->
    Stack = case get(indent_stack) of
        undefined -> [0];
        Else -> tl(Else)
    end,
    MatchIndent = fun(_, _) ->
        Dedent = hd(Stack),
        {ok, RE} = re:compile("^\n+([\t\s]*)"),
        case re:run(Input, RE) of
            {match, [_, {S, Indent}]} ->
                Matched = string:substr(Input, S, Indent + 1),
                Tail = string:substr(Input, S + Indent + 1),
                NewIndex = p_advance_index(Matched, Index),
                if Indent =/= Dedent ->
                    {fail, {expected, {dedent, Dedent}, NewIndex}};
                true ->
                    put(indent_stack, Stack),
                    {Matched, Tail, p_advance_index(Matched, NewIndex)}
                end;
            _ ->
                {fail, {expected, {dedent, Dedent}, Index}}
        end
    end,
    p(Input, Index, 'dedent', fun(I,D) -> (p_seq([MatchIndent]))(I,D) end, fun(Node, Idx) -> transform('dedent', Node, Idx) end).

'samedent'(Input, Index) ->
    Stack = case get(indent_stack) of
        undefined -> [0];
        Else -> Else
    end,
    MatchIndent = fun(_, _) ->
        Samedent = hd(Stack),
        {ok, RE} = re:compile("^\n+([\t\s]*)"),
        case re:run(Input, RE) of
            {match, [_, {S, Indent}]} ->
                Matched = string:substr(Input, S, Indent + 1),
                Tail = string:substr(Input, S + Indent + 1),
                NewIndex = p_advance_index(Matched, Index),
                if Indent == 0 ->
                    {fail, {expected, {samedent, Samedent}, NewIndex}};
                Indent =/= Samedent ->
                    {{line, L}, _} = Index,
                    exit({fail, {expected, {samedent, Samedent}, {{line, L}, {column, Indent}}}});
                true ->
                    {Matched, Tail, NewIndex}
                end;
            _ ->
                {fail, {expected, {samedent, Samedent}, Index}}
        end
    end,
    p(Input, Index, 'samedent', fun(I,D) -> (p_seq([MatchIndent]))(I,D) end, fun(Node, Idx) -> transform('samedent', Node, Idx) end).

'primary'(Input, Index) ->
    p(Input, Index, 'primary', fun(I,D) -> (p_choose([fun 'atom'/2, fun 'string'/2, fun 'number'/2, fun 'slice'/2, fun 'list'/2, fun 'identifier'/2]))(I,D) end, fun(Node, Idx) -> transform('primary', Node, Idx) end).

'comma'(Input, Index) ->
    p(Input, Index, 'comma', fun(I,D) -> (p_seq([p_optional(fun 'spaces'/2), p_string(","), p_optional(fun 'spaces'/2)]))(I,D) end, fun(Node, Idx) -> transform('comma', Node, Idx) end).

'list'(Input, Index) ->
    p(Input, Index, 'list', fun(I,D) -> (p_seq([p_string("["), p_optional(fun 'spaces'/2), fun 'number'/2, p_zero_or_more(p_seq([fun 'comma'/2, fun 'number'/2])), p_optional(fun 'spaces'/2), p_string("]")]))(I,D) end, fun(Node, Idx) -> transform('list', Node, Idx) end).

'slice'(Input, Index) ->
    p(Input, Index, 'slice', fun(I,D) -> (p_seq([p_choose([fun 'string'/2, fun 'list'/2, fun 'value'/2]), p_string("["), p_choose([p_seq([p_optional(fun 'number'/2), p_string(":"), p_optional(fun 'number'/2)]), fun 'number'/2]), p_string("]")]))(I,D) end, fun(Node, Idx) -> transform('slice', Node, Idx) end).

'value'(Input, Index) ->
    p(Input, Index, 'value', fun(I,D) -> (fun 'identifier'/2)(I,D) end, fun(Node, Idx) -> transform('value', Node, Idx) end).

'number'(Input, Index) ->
    p(Input, Index, 'number', fun(I,D) -> (p_choose([fun 'float'/2, fun 'integer'/2]))(I,D) end, fun(Node, Idx) -> transform('number', Node, Idx) end).

'string'(Input, Index) ->
    p(Input, Index, 'string', fun(I,D) -> (p_choose([p_seq([p_string("\""), p_label('string', p_zero_or_more(p_seq([p_not(p_string("\"")), p_choose([p_string("\\\""), p_anything()])]))), p_string("\"")]), p_seq([p_string("'"), p_label('string', p_zero_or_more(p_seq([p_not(p_string("'")), p_choose([p_string("\\'"), p_anything()])]))), p_string("'")])]))(I,D) end, fun(Node, Idx) -> transform('string', Node, Idx) end).

'atom'(Input, Index) ->
    p(Input, Index, 'atom', fun(I,D) -> (p_choose([p_seq([p_string(":"), p_label('atom', p_one_or_more(p_charclass("[a-zA-Z0-9@_]")))]), p_seq([p_string(":"), p_string("'"), p_label('atom', p_one_or_more(p_seq([p_not(p_string("'")), p_choose([p_string("\\'"), p_anything()])]))), p_string("'")])]))(I,D) end, fun(Node, Idx) -> transform('atom', Node, Idx) end).

'integer'(Input, Index) ->
    p(Input, Index, 'integer', fun(I,D) -> (p_seq([p_optional(p_string("-")), p_one_or_more(p_charclass("[0-9]"))]))(I,D) end, fun(Node, Idx) -> transform('integer', Node, Idx) end).

'float'(Input, Index) ->
    p(Input, Index, 'float', fun(I,D) -> (p_seq([p_optional(p_seq([p_optional(p_string("-")), p_one_or_more(p_charclass("[0-9]"))])), p_string("."), p_one_or_more(p_charclass("[0-9]"))]))(I,D) end, fun(Node, Idx) -> transform('float', Node, Idx) end).

'spaces'(Input, Index) ->
    p(Input, Index, 'spaces', fun(I,D) -> (p_one_or_more(p_charclass("[\s\t]")))(I,D) end, fun(Node, Idx) -> transform('spaces', Node, Idx) end).

'identifier'(Input, Index) ->
    p(Input, Index, 'identifier', fun(I,D) -> (p_seq([

        % [a-zA-Z_] [a-zA-Z0-9_-]* "?"?
        p_charclass("[a-zA-Z_]"), p_zero_or_more(p_charclass("[a-zA-Z0-9_-]")), p_optional(p_string("?"))
        
    ]))(I,D) end, fun(Node, Idx) -> transform('identifier', Node, Idx) end).

transform(Symbol,Node,Index) -> rice_transform:transform(Symbol, Node, Index).





p(Inp, Index, Name, ParseFun) ->
    p(Inp, Index, Name, ParseFun, fun(N, _Idx) -> N end).

p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
    % Grab the memo table from ets
    Memo = get_memo(StartIndex),
    % See if the current reduction is memoized
    case dict:find(Name, Memo) of
        % If it is, return the result
        {ok, Result} -> Result;
        % If not, attempt to parse
        _ ->
            case ParseFun(Inp, StartIndex) of
                % If it fails, memoize the failure
                {fail,_} = Failure ->
                    memoize(StartIndex, dict:store(Name, Failure, Memo)),
                    Failure;
                % If it passes, transform and memoize the result.
                {Result, InpRem, NewIndex} ->
                    Transformed = TransformFun(Result, StartIndex),
                    memoize(StartIndex, dict:store(Name, {Transformed, InpRem, NewIndex}, Memo)),
                    {Transformed, InpRem, NewIndex}
            end
    end.

setup_memo() ->
    put(parse_memo_table, ets:new(?MODULE, [set])).

release_memo() ->
    ets:delete(memo_table_name()).

memoize(Position, Struct) ->
    ets:insert(memo_table_name(), {Position, Struct}).

get_memo(Position) ->
    case ets:lookup(memo_table_name(), Position) of
        [] -> dict:new();
        [{Position, Dict}] -> Dict
    end.

memo_table_name() ->
        get(parse_memo_table).

p_eof() ->
    fun([], Index) -> {eof, [], Index};
         (_, Index) -> {fail, {expected, eof, Index}} end.

p_optional(P) ->
    fun(Input, Index) ->
            case P(Input, Index) of
                {fail,_} -> {[], Input, Index};
                {_, _, _} = Success -> Success
            end
    end.

p_not(P) ->
    fun(Input, Index)->
            case P(Input,Index) of
                {fail,_} ->
                    {[], Input, Index};
                {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
            end
    end.

p_assert(P) ->
    fun(Input,Index) ->
            case P(Input,Index) of
                {fail,_} = Failure-> Failure;
                _ -> {[], Input, Index}
            end
    end.

p_and(P) ->
    p_seq(P).

p_seq(P) ->
    fun(Input, Index) ->
            p_all(P, Input, Index, [])
    end.

p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
    case P(Inp, Index) of
        {fail, _} = Failure -> Failure;
        {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
    end.

p_choose(Parsers) ->
    fun(Input, Index) ->
            p_attempt(Parsers, Input, Index, none)
    end.

p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
    case P(Input, Index) of
        {fail, _} = Failure ->
            case FirstFailure of
                none -> p_attempt(Parsers, Input, Index, Failure);
                _ -> p_attempt(Parsers, Input, Index, FirstFailure)
            end;
        Result -> Result
    end.

p_zero_or_more(P) ->
    fun(Input, Index) ->
            p_scan(P, Input, Index, [])
    end.

p_zero_or_more(match, P) ->
    fun(Input, Index) ->
        case P(Input, Index) of
            {fail, {match, 0, _}} ->
                {[], Input, Index};
            {fail, {match, _, _}} = Failure ->
                p_match_error(Failure);
            Result ->
                Result
        end
    end.

p_match_error({fail, {match, _, Failure}}) ->
    p_match_error(Failure);

p_match_error(Failure) ->
    Failure.

p_seq_match(P) ->
    fun(Input, Index) ->
        p_seq_match_all(P, Input, Index, 0, [])
    end.

p_seq_match_all([], Inp, Index, _, Accum) -> {lists:reverse(Accum), Inp, Index};
p_seq_match_all([P|Parsers], Inp, Index, Count, Accum) ->
    case P(Inp, Index) of
        {fail, Failure} -> {fail, {match, Count, Failure}};
        {Result, InpRem, NewIndex} -> p_seq_match_all(Parsers, InpRem, NewIndex, Count + 1, [Result|Accum])
    end.

p_one_or_more(P) ->
    fun(Input, Index)->
            Result = p_scan(P, Input, Index, []),
            case Result of
                {[_|_], _, _} ->
                    Result;
                _ ->
                    {fail, {expected, Failure, _}} = P(Input,Index),
                    {fail, {expected, {at_least_one, Failure}, Index}}
            end
    end.

p_label(Tag, P) ->
    fun(Input, Index) ->
            case P(Input, Index) of
                {fail,_} = Failure ->
                     Failure;
                {Result, InpRem, NewIndex} ->
                    {{Tag, Result}, InpRem, NewIndex}
            end
    end.

p_scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
p_scan(P, Inp, Index, Accum) ->
    case P(Inp, Index) of
        {fail,_} -> {lists:reverse(Accum), Inp, Index};
        {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
    end.

p_string(S) ->
    fun(Input, Index) ->
            case lists:prefix(S, Input) of
                true -> {S, lists:sublist(Input, length(S)+1, length(Input)), p_advance_index(S,Index)};
                _ -> {fail, {expected, {string, S}, Index}}
            end
    end.

p_anything() ->
    fun([], Index) -> {fail, {expected, any_character, Index}};
         ([H|T], Index) -> {H, T, p_advance_index(H, Index)}
    end.

p_charclass(Class) ->
    fun(Inp, Index) ->
         {ok, RE} = re:compile("^"++Class),
            case re:run(Inp, RE) of
                {match, _} ->
                    {hd(Inp), tl(Inp), p_advance_index(hd(Inp), Index)};
                _ -> {fail,{expected, {character_class, Class}, Index}}
            end
    end.

line({{line,L},_}) -> L;
line(_) -> undefined.

column({_,{column,C}}) -> C;
column(_) -> undefined.

p_advance_index(MatchedInput, Index) when is_list(MatchedInput) -> % strings
    lists:foldl(fun p_advance_index/2, Index, MatchedInput);
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
    {{line, Line}, {column, Col}} = Index,
    case MatchedInput of
        $\n -> {{line, Line+1}, {column, 1}};
        _ -> {{line, Line}, {column, Col+1}}
    end.
