-module(peg2).
-compile(nowarn_unused_function).
-export([
    t_seq/4, p_seq/1,
    t_zero_or_more/4, p_zero_or_more/1,
    t_string/4, p_string/1,
    t_regex/4,
    t_not/4,
    t_or/4, p_or/1,
    lookahead/1,
    transform/2,
    test/0
]).

% Parser functions

p(_, Index, {match, [[]], _, _}) ->
    {fail, 0, Index, nothing_match};

p(Name, Index, {match, Acc, Input, ParseIndex}) ->
    {match, {Name, Acc, Index}, Input, ParseIndex};

p(_, _, Failure = {fail, _, _, nothing_match}) ->
    Failure;

p(_, _, {fail, Count, Index, {expected, Type = {_, _}, Match}}) ->
    {fail, Count, Index, {expected, Type, Match}};

p(Name, _, {fail, Count, Index, {expected, Type, Match}}) ->
    {fail, Count, Index, {expected, {Name, Type}, Match}}.

lookahead({match, {_, Acc, _}, _, _}) ->
    lookahead_acc(Acc).

lookahead_acc([]) ->
    [[]];

lookahead_acc([{_, Match, _} | []]) ->
    [Match | []];

lookahead_acc([{_, Match, _} | [T]]) ->
    [Match | lookahead_acc(T)].

% sequence

t_seq(Name, Input, Index, Sequence) ->
    p(Name, Index, (p_seq(Sequence))(Input, Index)).

p_seq(Sequence) ->
    fun(Input, Index) ->
        seq(Input, Index, Sequence, [], 0)
    end.

seq(Input, Index, [], Acc, _) ->
    {match, lists:reverse(Acc), Input, Index};

seq(Input, Index, [S | Sequence], Acc, Count) ->
    case S(Input, Index) of
        {fail, TestCount, FailureIndex, Failure} when (Count == 0) and (TestCount > 0)->
            {fail, TestCount, FailureIndex, Failure};
        {fail, _, FailureIndex, Failure} ->
            {fail, Count, FailureIndex, Failure};
        {match, Match, NewInput, NewIndex} ->
            seq(NewInput, NewIndex, Sequence, [Match | Acc], Count + 1)
    end.

% zero or more

t_zero_or_more(Name, Input, Index, Match) ->
    p(Name, Index, (p_zero_or_more(Match))(Input, Index)).

p_zero_or_more(Match) ->
    p_zero_or_more_acc(Match, []).

p_zero_or_more_acc(Match, Acc) ->
    fun(Input, Index) ->
        case Match(Input, Index) of
            {fail, 0, _, _} ->
                {match, Acc, Input, Index};
            {fail, _, _, _} = Failure ->
                Failure;
            {match, Result, NewInput, NewIndex} ->
                (p_zero_or_more_acc(Match, [Result | Acc]))(NewInput, NewIndex)
        end
    end.

% string

t_string(Name, Input, Index, Match) ->
    p(Name, Index, (p_string(Match))(Input, Index)).

p_string(Match) ->
    fun(Input, Index) ->
        case lists:prefix(Match, Input) of
            true ->
                {match, Match, string:substr(Input, length(Match) + 1), advance_index(Match, Index)};
            false ->
                {fail, 0, Index, {expected, string, Match}}
        end
    end.

% regex

t_regex(Name, Input, Index, Match) ->
    p(Name, Index, (p_regex(Match))(Input, Index)).

p_regex(Match) ->
    fun(Input, Index) ->
        {ok, Regex} = re:compile("^" ++ Match),
        case re:run(Input, Regex) of
            {match, [{_, Count}]} ->
                ResultMatch = string:substr(Input, 1, Count),
                NewInput = string:substr(Input, Count + 1),
                {match, ResultMatch, NewInput, advance_index(ResultMatch, Index)};
            _ ->
                {fail, 0, Index, {expected, regex, Match}}
        end
    end.

% not

t_not(Name, Input, Index, Match) ->
    p(Name, Index, (p_not(Match))(Input, Index)).

p_not(Match) ->
    fun(Input, Index) ->
        case Match(Input, Index) of
            {fail, 0, Index, {_, _, ResultMatch}} ->
                NewInput = case Input of
                    [] -> [];
                    _ -> string:substr(Input, length(ResultMatch) + 1)
                end,
                {match, ResultMatch, NewInput, advance_index(ResultMatch, Index)};
            {match, ResultMatch, _, _} ->
                {fail, 0, Index, {expected, 'not', ResultMatch}}
        end
    end.

% or

t_or(Name, Input, Index, Sequence) ->
    p(Name, Index, (p_or(Sequence))(Input, Index)).

p_or(Sequence) ->
    fun(Input, Index) ->
        p_or_acc(Sequence, Input, Index, -1, {fail, 0, Index, nothing_match})
    end.

p_or_acc([], _, _, _, MoreProx) ->
    MoreProx;

p_or_acc([S | Sequence], Input, Index, Count, MoreProx) ->
    case S(Input, Index) of
        {match, _, _, _} = Match ->
            Match;
        {fail, FailCount, _, _} = Failure when FailCount > Count ->
            p_or_acc(Sequence, Input, Index, FailCount, Failure);
        _ ->
            p_or_acc(Sequence, Input, Index, Count, MoreProx)
    end.

% Internal functions

advance_index(Match, Index) ->
    lists:foldl(fun(Char, {Line = {line, L}, {column, C}}) ->
        case Char of
            $\n -> {{line, L + 1}, {column, 1}};
              _ -> {Line, {column, C + 1}}
        end end, Index, Match).

transform([], _) ->
    [];

transform([{Name, Children, Index} | Tail], Fn) ->
    [Fn(Name, transform(Children, Fn), Index) | transform(Tail, Fn)];

transform([Terminal | Tail], Fn) ->
    [Terminal | transform(Tail, Fn)].

-include_lib("eunit/include/eunit.hrl").

test() ->
    test_advance_index().

test_advance_index() ->
    ?assertEqual(advance_index("foo\n\nbar", {{line, 1}, {column, 1}}), {{line, 3}, {column, 4}}).
