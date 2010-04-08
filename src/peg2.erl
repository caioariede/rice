-module(peg2).
-compile(nowarn_unused_function).
-export([
    p/4,
    t_seq/5, p_seq/1,
    t_zero_or_more/5, p_zero_or_more/1,
    t_one_or_more/5, p_one_or_more/1,
    t_optional/5, p_optional/1,
    t_string/5, p_string/1,
    t_regex/5, p_regex/1,
    t_not/5, p_not/1,
    t_or/5, p_or/2,
    p_eof/0,
    lookahead/1,
    transform/1,
    advance_index/2,
    test/0
]).

% Parser functions

p(_, Index, {match, [[]], _, _, _}, _) ->
    {fail, 0, Index, nothing_match};

p(Name, Index, {match, Acc, Input, ParseIndex, _}, Transform) ->
    {match, {Name, Acc, Index, Transform}, Input, ParseIndex, undefined};

p(_, _, Failure = {fail, _, _, nothing_match}, _) ->
    Failure;

p(_, _, {fail, Count, Index, {expected, Type = {_, _}, Match, Given}}, _) ->
    {fail, Count, Index, {expected, Type, Match, Given}};

p(Name, _, {fail, Count, Index, {expected, Type, Match, Given}}, _) ->
    {fail, Count, Index, {expected, {Name, Type}, Match, Given}}.

lookahead({match, {_, Acc, _, _}, _, _, _}) ->
    lookahead_acc(Acc).

lookahead_acc([]) ->
    [[]];

lookahead_acc([{_, Match, _, _} | []]) ->
    [Match | []];

lookahead_acc([{_, Match, _, _} | [T]]) ->
    [Match | lookahead_acc(T)].

% sequence

t_seq(Name, Input, Index, Sequence, Transform) ->
    p(Name, Index, (p_seq(Sequence))(Input, Index), Transform).

p_seq(Sequence) ->
    fun(Input, Index) ->
        seq(Input, Index, Sequence, [], 0)
    end.

seq(Input, Index, [], Acc, _) ->
    {match, lists:reverse(Acc), Input, Index, undefined};

seq(Input, Index, [S | Sequence], Acc, Count) ->
    case S(Input, Index) of
        {fail, TestCount, FailureIndex, Failure} when TestCount == 0 ->
            {fail, Count, FailureIndex, Failure};
        {fail, _, _, _} = Failure ->
            Failure;
        {match, Match, NewInput, NewIndex, _} ->
            seq(NewInput, NewIndex, Sequence, [Match | Acc], Count + 1)
    end.

% one or more

t_one_or_more(Name, Input, Index, Match, Transform) ->
    p(Name, Index, (p_one_or_more(Match))(Input, Index), Transform).

p_one_or_more(Match) ->
    p_one_or_more_acc(Match, [], 0).

p_one_or_more_acc(Match, Acc, Count) ->
    fun(Input, Index) ->
        case Match(Input, Index) of
            {fail, TestCount, _, _} when (TestCount == 0) and (Count > 0) ->
                {match, Acc, Input, Index, undefined};
            {fail, _, _, _} = Failure ->
                Failure;
            {match, Result, [], NewIndex, _} ->
                {match, [Result | Acc], [], NewIndex, undefined};
            {match, Result, NewInput, NewIndex, _} ->
                (p_one_or_more_acc(Match, [Result | Acc], Count + 1))(NewInput, NewIndex)
        end
    end.

% zero or more

t_zero_or_more(Name, Input, Index, Match, Transform) ->
    p(Name, Index, (p_zero_or_more(Match))(Input, Index), Transform).

p_zero_or_more(Match) ->
    p_zero_or_more_acc(Match, [], 0).

p_zero_or_more_acc(Match, Acc, Count) ->
    fun(Input, Index) ->
        case Match(Input, Index) of
            {fail, TestCount, _, _} when TestCount == 0 ->
                {match, Acc, Input, Index, undefined};
            {fail, _, _, _} = Failure ->
                Failure;
            {match, Result, [], NewIndex, _} ->
                {match, [Result | Acc], [], NewIndex, undefined};
            {match, Result, NewInput, NewIndex, _} ->
                (p_zero_or_more_acc(Match, [Result | Acc], Count + 1))(NewInput, NewIndex)
        end
    end.

% optional

t_optional(Name, Input, Index, Match, Transform) ->
    p(Name, Index, (p_optional(Match))(Input, Index), Transform).

p_optional(Match) ->
    fun(Input, Index) ->
        case Match(Input, Index) of
            {fail, TestCount, _, _} when TestCount == 0 ->
                {match, [], Input, Index, undefined};
            Other ->
                Other
        end
    end.

% string

t_string(Name, Input, Index, Match, Transform) ->
    p(Name, Index, (p_string(Match))(Input, Index), Transform).

p_string(Match) ->
    fun(Input, Index) ->
        case lists:prefix(Match, Input) of
            true ->
                {match, {string, Match}, string:substr(Input, length(Match) + 1), advance_index(Match, Index), undefined};
            false ->
                Given = string:substr(Input, 1, length(Match)),
                {fail, 0, Index, {expected, string, Match, Given}}
        end
    end.

% regex

t_regex(Name, Input, Index, Match, Transform) ->
    p(Name, Index, (p_regex(Match))(Input, Index), Transform).

p_regex(Match) ->
    fun(Input, Index) ->
        {ok, Regex} = re:compile("^" ++ Match),
        case re:run(Input, Regex) of
            {match, [{_, Count}]} ->
                ResultMatch = string:substr(Input, 1, Count),
                NewInput = string:substr(Input, Count + 1),
                {match, ResultMatch, NewInput, advance_index(ResultMatch, Index), undefined};
            _ ->
                Given = string:substr(Input, 1, length(Match)),
                {fail, 0, Index, {expected, regex, Match, Given}}
        end
    end.

% not

t_not(Name, Input, Index, Match, Transform) ->
    p(Name, Index, (p_not(Match))(Input, Index), Transform).

p_not(Match) ->
    fun(Input, Index) ->
        case Match(Input, Index) of
            {fail, 0, Index, {_, _, ResultMatch}} ->
                NewInput = case Input of
                    [] -> [];
                    _ -> string:substr(Input, length(ResultMatch) + 1)
                end,
                {match, ResultMatch, NewInput, advance_index(ResultMatch, Index), undefined};
            {match, ResultMatch, _, _, _} ->
                {fail, 0, Index, {expected, 'not', Match, ResultMatch}}
        end
    end.

% or

t_or(Name, Input, Index, Sequence, Transform) ->
    p(Name, Index, (p_or(Name, Sequence))(Input, Index), Transform).

p_or(Name, Sequence) ->
    fun(Input, Index) ->
        p_or_acc(Name, Sequence, Input, Index, -1, {fail, 0, Index, nothing_match})
    end.

p_or_acc(Name, [], _, _, _, {fail, Count, Index, {expected, T, Expected, Given}}) ->
    {fail, Count, Index, {expected, {Name, T}, Expected, Given}};

p_or_acc(Name, [S | Sequence], Input, Index, Count, MoreProx) ->
    case S(Input, Index) of
        {match, _, _, _, _} = Match ->
            Match;
        {fail, FailCount, _, _} = Failure when FailCount > Count ->
            p_or_acc(Name, Sequence, Input, Index, FailCount, Failure);
        _ ->
            p_or_acc(Name, Sequence, Input, Index, Count, MoreProx)
    end.

% eof

p_eof() ->
    fun(Input, Index) ->
        case Input of
            [] ->
                {match, [], 'eof', Index, undefined};
            [Match | _] ->
                {fail, 0, Index, {expected, 'eof', [], [Match]}}
        end
    end.
    

% Internal functions

advance_index(Match, Index) ->
    lists:foldl(fun(Char, {Line = {line, L}, {column, C}}) ->
        case Char of
            $\n -> {{line, L + 1}, {column, 1}};
              _ -> {Line, {column, C + 1}}
        end end, Index, Match).

% Transformation

transform([]) ->
    [];

transform([Sequence | Tail]) when is_list(Sequence) ->
    [transform_sequence(Sequence) | transform(Tail)];

transform([Node | Tail]) ->
    [transform(Node) | transform(Tail)];

transform({string, String}) ->
    String;

transform({_, Children, Index, Transform}) ->
    Transform(transform(Children), Index);

transform(Node) ->
    Node.

transform_sequence([]) ->
    [];

transform_sequence([Node | Tail]) ->
    [transform(Node) | transform_sequence(Tail)].

% Test

-include_lib("eunit/include/eunit.hrl").

test() ->
    test_advance_index().

test_advance_index() ->
    ?assertEqual(advance_index("foo\n\nbar", {{line, 1}, {column, 1}}), {{line, 3}, {column, 4}}).
