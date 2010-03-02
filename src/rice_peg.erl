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
  release_memo(), Result.

'root'(Input, Index) ->
  p(Input, Index, 'root', fun(I,D) -> (p_seq([fun 'module'/2, fun 'functions'/2, p_optional(p_choose([fun 'spaces'/2, fun 'newline'/2]))]))(I,D) end, fun(Node, Idx) -> transform('root', Node, Idx) end).

'module'(Input, Index) ->
  p(Input, Index, 'module', fun(I,D) -> (p_seq([p_string("module"), fun 'spaces'/2, fun 'identifier'/2]))(I,D) end, fun(Node, Idx) -> transform('module', Node, Idx) end).

'functions'(Input, Index) ->
  p(Input, Index, 'functions', fun(I,D) -> (p_one_or_more(fun 'function'/2))(I,D) end, fun(Node, Idx) -> transform('functions', Node, Idx) end).

'function'(Input, Index) ->
  p(Input, Index, 'function', fun(I,D) -> (p_seq([fun 'clause'/2, p_zero_or_more(fun 'clause'/2), fun 'indent'/2, p_string("end")]))(I,D) end, fun(Node, Idx) -> transform('function', Node, Idx) end).

'clause'(Input, Index) ->
  p(Input, Index, 'clause', fun(I,D) -> (p_seq([fun 'indent'/2, p_string("def"), fun 'spaces'/2, fun 'identifier'/2, p_optional(p_choose([p_seq([fun 'spaces'/2, fun 'clause_args'/2, fun 'comma'/2, fun 'clause_kwargs'/2]), p_seq([fun 'spaces'/2, p_choose([fun 'clause_args'/2, fun 'clause_kwargs'/2])])])), p_choose([fun 'block'/2, fun 'block_inline'/2])]))(I,D) end, fun(Node, Idx) -> transform('clause', Node, Idx) end).

'clause_args'(Input, Index) ->
  p(Input, Index, 'clause_args', fun(I,D) -> (p_seq([fun 'clause_args_arg'/2, p_zero_or_more(p_seq([fun 'comma'/2, fun 'clause_args_arg'/2]))]))(I,D) end, fun(Node, Idx) -> transform('clause_args', Node, Idx) end).

'clause_args_arg'(Input, Index) ->
  p(Input, Index, 'clause_args_arg', fun(I,D) -> (p_choose([fun 'variable'/2, fun 'atom'/2, fun 'string'/2, fun 'integer'/2]))(I,D) end, fun(Node, Idx) -> transform('clause_args_arg', Node, Idx) end).

'clause_kwargs'(Input, Index) ->
  p(Input, Index, 'clause_kwargs', fun(I,D) -> (p_seq([p_string("*"), fun 'identifier'/2, p_string("("), p_optional(fun 'spaces'/2), fun 'clause_kwargs_key'/2, p_zero_or_more(p_seq([fun 'comma'/2, fun 'clause_kwargs_key'/2])), p_optional(fun 'spaces'/2), p_string(")")]))(I,D) end, fun(Node, Idx) -> transform('clause_kwargs', Node, Idx) end).

'clause_kwargs_key'(Input, Index) ->
  p(Input, Index, 'clause_kwargs_key', fun(I,D) -> (p_choose([p_seq([fun 'atom'/2, p_optional(fun 'spaces'/2), p_string("=>"), p_optional(fun 'spaces'/2), fun 'value'/2]), fun 'atom'/2]))(I,D) end, fun(Node, Idx) -> transform('clause_kwargs_key', Node, Idx) end).

'block'(Input, Index) ->
  p(Input, Index, 'block', fun(I,D) -> (p_seq([fun 'indent'/2, fun 'statements'/2]))(I,D) end, fun(Node, Idx) -> transform('block', Node, Idx) end).

'block_inline'(Input, Index) ->
  p(Input, Index, 'block_inline', fun(I,D) -> (p_seq([p_string(":"), p_optional(fun 'spaces'/2), fun 'statements_inline'/2]))(I,D) end, fun(Node, Idx) -> transform('block_inline', Node, Idx) end).

'do'(Input, Index) ->
  p(Input, Index, 'do', fun(I,D) -> (p_seq([p_one_or_more(fun 'do_clause'/2), p_string("end")]))(I,D) end, fun(Node, Idx) -> transform('do', Node, Idx) end).

'do_clause'(Input, Index) ->
  p(Input, Index, 'do_clause', fun(I,D) -> (p_seq([p_string("do"), p_optional(p_seq([fun 'spaces'/2, fun 'clause_args'/2])), fun 'block'/2]))(I,D) end, fun(Node, Idx) -> transform('do_clause', Node, Idx) end).

'statements'(Input, Index) ->
  p(Input, Index, 'statements', fun(I,D) -> (p_seq([fun 'statement'/2, p_optional(fun 'statements_samedent'/2)]))(I,D) end, fun(Node, Idx) -> transform('statements', Node, Idx) end).

'statements_samedent'(Input, Index) ->
  p(Input, Index, 'statements_samedent', fun(I,D) -> (p_seq([fun 'samedent'/2, p_one_or_more(fun 'statements'/2)]))(I,D) end, fun(Node, Idx) -> transform('statements_samedent', Node, Idx) end).

'statements_inline'(Input, Index) ->
  p(Input, Index, 'statements_inline', fun(I,D) -> (p_seq([fun 'statement'/2, p_zero_or_more(p_seq([fun 'comma'/2, fun 'statement'/2]))]))(I,D) end, fun(Node, Idx) -> transform('statements_inline', Node, Idx) end).

'statement'(Input, Index) ->
  p(Input, Index, 'statement', fun(I,D) -> (p_choose([fun 'do'/2, fun 'call'/2, fun 'variable'/2, fun 'atom'/2, fun 'string'/2, fun 'integer'/2]))(I,D) end, fun(Node, Idx) -> transform('statement', Node, Idx) end).

'call'(Input, Index) ->
  p(Input, Index, 'call', fun(I,D) -> (p_seq([fun 'call_value'/2, p_optional(p_choose([p_seq([fun 'spaces'/2, fun 'call_args'/2, fun 'comma'/2, fun 'call_kwargs'/2]), p_seq([fun 'spaces'/2, p_choose([fun 'call_args'/2, fun 'call_kwargs'/2])])]))]))(I,D) end, fun(Node, Idx) -> transform('call', Node, Idx) end).

'call_value'(Input, Index) ->
  p(Input, Index, 'call_value', fun(I,D) -> (p_choose([p_seq([fun 'value'/2, p_one_or_more(p_seq([p_string("."), fun 'identifier'/2]))]), fun 'identifier'/2]))(I,D) end, fun(Node, Idx) -> transform('call_value', Node, Idx) end).

'call_args'(Input, Index) ->
  p(Input, Index, 'call_args', fun(I,D) -> (p_seq([p_choose([fun 'call'/2, fun 'call_args_arg'/2]), p_zero_or_more(p_seq([fun 'comma'/2, fun 'call_args_arg'/2]))]))(I,D) end, fun(Node, Idx) -> transform('call_args', Node, Idx) end).

'call_args_arg'(Input, Index) ->
  p(Input, Index, 'call_args_arg', fun(I,D) -> (p_seq([fun 'value'/2, p_seq([p_not(p_seq([fun 'spaces'/2, p_string("=>")])), p_string("")])]))(I,D) end, fun(Node, Idx) -> transform('call_args_arg', Node, Idx) end).

'call_kwargs'(Input, Index) ->
  p(Input, Index, 'call_kwargs', fun(I,D) -> (p_seq([fun 'call_kwargs_arg'/2, p_zero_or_more(p_seq([fun 'comma'/2, fun 'call_kwargs_arg'/2]))]))(I,D) end, fun(Node, Idx) -> transform('call_kwargs', Node, Idx) end).

'call_kwargs_arg'(Input, Index) ->
  p(Input, Index, 'call_kwargs_arg', fun(I,D) -> (p_seq([fun 'value'/2, p_optional(fun 'spaces'/2), p_string("=>"), p_optional(fun 'spaces'/2), fun 'value'/2]))(I,D) end, fun(Node, Idx) -> transform('call_kwargs_arg', Node, Idx) end).

'newline'(Input, Index) ->
  p(Input, Index, 'newline', fun(I,D) -> (p_one_or_more(p_charclass("[\r\n]")))(I,D) end, fun(Node, Idx) -> transform('newline', Node, Idx) end).

'indent'(Input, Index) ->
  p(Input, Index, 'indent', fun(I,D) -> (p_seq([fun 'newline'/2, p_optional(fun 'spaces'/2)]))(I,D) end, fun(Node, Idx) -> transform('indent', Node, Idx) end).

'samedent'(Input, Index) ->
  p(Input, Index, 'samedent', fun(I,D) -> (p_seq([fun 'newline'/2, fun 'spaces'/2]))(I,D) end, fun(Node, Idx) -> transform('samedent', Node, Idx) end).

'value'(Input, Index) ->
  p(Input, Index, 'value', fun(I,D) -> (p_choose([fun 'primitive'/2, fun 'do'/2, fun 'variable'/2]))(I,D) end, fun(Node, Idx) -> transform('value', Node, Idx) end).

'variable'(Input, Index) ->
  p(Input, Index, 'variable', fun(I,D) -> (fun 'identifier'/2)(I,D) end, fun(Node, Idx) -> transform('variable', Node, Idx) end).

'comma'(Input, Index) ->
  p(Input, Index, 'comma', fun(I,D) -> (p_seq([p_optional(fun 'spaces'/2), p_string(","), p_optional(fun 'spaces'/2)]))(I,D) end, fun(Node, Idx) -> transform('comma', Node, Idx) end).

'primitive'(Input, Index) ->
  p(Input, Index, 'primitive', fun(I,D) -> (p_choose([fun 'atom'/2, fun 'string'/2, fun 'integer'/2]))(I,D) end, fun(Node, Idx) -> transform('primitive', Node, Idx) end).

'string'(Input, Index) ->
  p(Input, Index, 'string', fun(I,D) -> (p_choose([p_seq([p_string("\""), p_label('string', p_zero_or_more(p_seq([p_not(p_string("\"")), p_choose([p_string("\\\""), p_anything()])]))), p_string("\"")]), p_seq([p_string("'"), p_label('string', p_zero_or_more(p_seq([p_not(p_string("'")), p_choose([p_string("\\'"), p_anything()])]))), p_string("'")])]))(I,D) end, fun(Node, Idx) -> transform('string', Node, Idx) end).

'atom'(Input, Index) ->
  p(Input, Index, 'atom', fun(I,D) -> (p_choose([p_seq([p_string(":"), p_label('atom', p_one_or_more(p_charclass("[a-zA-Z0-9@_]")))]), p_seq([p_string(":"), p_string("'"), p_label('atom', p_one_or_more(p_seq([p_not(p_string("'")), p_choose([p_string("\\'"), p_anything()])]))), p_string("'")])]))(I,D) end, fun(Node, Idx) -> transform('atom', Node, Idx) end).

'integer'(Input, Index) ->
  p(Input, Index, 'integer', fun(I,D) -> (p_seq([p_optional(p_string("-")), p_one_or_more(p_charclass("[0-9]"))]))(I,D) end, fun(Node, Idx) -> transform('integer', Node, Idx) end).

'spaces'(Input, Index) ->
  p(Input, Index, 'spaces', fun(I,D) -> (p_one_or_more(p_charclass("[\s\t]")))(I,D) end, fun(Node, Idx) -> transform('spaces', Node, Idx) end).

'identifier'(Input, Index) ->
  p(Input, Index, 'identifier', fun(I,D) -> (p_seq([p_charclass("[a-zA-Z_]"), p_zero_or_more(p_charclass("[a-zA-Z0-9_-]")), p_optional(p_string("?"))]))(I,D) end, fun(Node, Idx) -> transform('identifier', Node, Idx) end).
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
