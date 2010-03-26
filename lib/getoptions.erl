-module(getoptions).
-export([extract_options/2, extract_options/3, get_opt/2]).
-vsn({0, 5, 0}).

%% @copyright 2008 partdavid at gmail.com
%%
%% This program is free software: you can redistribute it and/or modify it
%% under the terms of the GNU Lesser General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or (at your
%% option) any later version.
%% 
%% This program is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
%% FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
%% License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program (see the COPYING file for details).  If not, see <http://www.gnu.org/licenses/>.
%%


-include_lib("eunit/include/eunit.hrl").

-record(opt,
        {long,
         longstr,
         short,
         actual,
         type}).

%% @doc Extract options from a list of arguments, according to the
%% specification given. This function returns a tuple containing a keylist
%% of the option settings and a list of remaining arguments (unmodified, in
%% the original order).
%%
%% The intent of the module is to pass in a list of strings, such as you
%% receive from invoking `erts' with the `run' option or in an escript,
%% and parse the list to extract Unix-style command-line options. The
%% remaining arguments and option settings are conveniently available
%% after extraction.
%%
%% Options necessarily have a "long" name, and optionally might have a
%% single-letter equivalent. Single-letter options are introduced with a
%% single hyphen (`-'). Multiple single-letter options can be run together.
%% The first single-letter option in the group that requires an argument
%% will consume the rest of the word (that is, options are processed like
%% "most" Unix commands, and <i>not</i> like `tar'.
%%
%% Single-letter options that require an argument can be separated from their
%% argument.
%%
%% Options specified by their "long" name are introduced by a double hyphen
%% (`--'). Options that accept an argument can be separated from their
%% argument with an equals sign (`=') or the option argument may follow
%% the option.
%%
%% This function knows how to parse the list of arguments from the option
%% specifier list passed. This consists of a list of option specifiers, which
%% are tuples describing the name and type of each option.
%%
%% The option specifier is of the form `{Name, Type}' or
%% `{Name, SingleLetter, Type}'. The Name is an atom denoting the "long" name
%% of the option. The optional SingleLetter attribute is an atom describing
%% the single-letter abbreviation of the option, and the Type describes
%% what sort of value the option takes.
%%
%% ==Types==
%% <ul>
%%    <li>Types that accept arguments:
%%    <ul>
%%
%%       <li>`boolean' - The value of the option, if set, will be `true' or
%%       `false'. A long, negative form of the option introduced with `--no'
%%       will be parsed as setting the option to `false' (e.g. for the option
%%       `verbose', `--noverbose' will set `verbose' to `false').</li>
%%
%%       <li>`count' - The value of the option, if set, will be a count of
%%       the number of occurrences of the option in the argument list.</li>
%%
%%   </ul>
%%   <li>Types that accept simple arguments:
%%   <ul>
%%
%%      <li>`string' - The option argument is left alone.</li>
%%      <li>`atom' - The option argument is converted to an atom.</li>
%%      <li>`integer' - The option argument is converted to an integer.</li>
%%      <li>`float' - The option argument is converted to a float.</li>
%%
%%   </ul>
%%   <li>Types that accept complex arguments:
%%   <ul>
%%
%%      <li>`{list, Type}' - Each time the option is encountered, its argument
%%      is added to the list. The value of the option, if set, will be a list
%%      of option arguments, each of one of the simple types listed above.</li>
%%
%%      <li>`{keylist, Type}' - Each time the option is encountered its
%%      argument is taken as an assignment of the form `Key=Value'. The Key
%%      portion is turned into an atom, and the Value portion set according
%%      to Type, one of the simple types, above. The value of the option,
%%      if set, is a keylist of `{Key, Value}' pairs.</li>
%%   </ul>
%% </ul>
%%
%%
%% @spec extract_options(ArgList::StringList, OptSpecs::OptSpecList) -> {OptList, StringList}
%%    StringList = [string()]
%%    OptSpecList = [OptSpec]
%%    OptSpec = {atom(), OptType}
%%    OptType = atom + boolean + count + float + integer + string + {list, OptType} + {keylist, OptType}
%%    OptList = [Option]
%%    Option = {atom(), term()}
extract_options(ArgList, OptSpecs) ->
   extract_options(ArgList, OptSpecs, []).

%% @doc As {@link extract_options/2}, but with an initial list of default option settings.
%% @spec extract_options(ArgList::StringList, OptSpecs::OptSpecList, Default::OptList) -> {OptList, StringList}
%%    StringList = [string()]
%%    OptSpecList = [OptSpec]
%%    OptSpec = {atom(), OptType}
%%    OptType = atom + boolean + count + float + integer + string + {list, OptType} + {keylist, OptType}
%%    OptList = [Option]
%%    Option = {atom(), term()}
extract_options(ArgList, OptSpecs, Defaults) ->
   extract_options(ArgList, canonicalize(OptSpecs), '_', Defaults, []).

canonicalize(OptSpecs) ->
   lists:flatten(lists:map(fun can_spec/1, OptSpecs)).

can_spec({Long, Type}) ->
   can_spec({Long, '_', Type});
can_spec({Long, Short, boolean}) ->
   LongStr = atom_to_list(Long),
   AntiStr = "no" ++ LongStr,
   [#opt{long = Long,
        longstr = atom_to_list(Long),
        short = Short,
        actual = Long,
        type = boolean},
    #opt{long = list_to_atom(AntiStr),
         longstr = AntiStr,
         short = '_',
         actual = Long,
         type = antiboolean}];
can_spec({Long, Short, Type}) when Type == count; Type == string;
                                   Type == atom;  Type == integer;
                                   Type == float ->
   #opt{long = Long,
        longstr = atom_to_list(Long),
        short = Short,
        actual = Long,
        type = Type};
can_spec({Long, Short, {Type, What}}) when Type == list;
                                           Type == keylist ->
   case What of
      SubType when SubType == string;
                   SubType == atom;
                   SubType == integer;
                   SubType == float ->
         #opt{long = Long, longstr = atom_to_list(Long), short = Short,
              actual = Long, type = {Type, What}};
      _ -> throw({invalid_subtype_for_type, Type, What})
   end.

extract_options([], _, '_', Opts, Args) ->
   {Opts, Args};
extract_options([], _, Getting, _, _) ->
   throw({option_requires_argument, Getting#opt.actual});
extract_options(["--"|Rest], _, '_', Opts, Args) ->
   {Opts, Args ++ Rest};
extract_options(["--" ++ Opt|Rest], OptSpecs, _, Opts, Args) ->
   %% TODO: what if long option that does not take argument has an
   %% argument by "="?
   case lists:splitwith(fun noteq/1, Opt) of
      {Optp, "=" ++ Value} -> 
         extract_options(["--" ++ Optp, Value|Rest], OptSpecs, '_', Opts, Args);
      {Opt, []} ->
         case lists:keysearch(Opt, 3, OptSpecs) of
            {value, OptSpec = #opt{type = boolean}} ->
               extract_options(Rest, OptSpecs, '_',
                               set_opt(OptSpec#opt.actual, true, Opts), Args);
            {value, OptSpec = #opt{type = antiboolean}} ->
               extract_options(Rest, OptSpecs, '_',
                               set_opt(OptSpec#opt.actual, false, Opts), Args);
            {value, OptSpec = #opt{type = count}} ->
               extract_options(Rest, OptSpecs, '_',
                               increment(OptSpec#opt.actual, Opts), Args);
            {value, OptSpec} ->
               extract_options(Rest, OptSpecs, OptSpec,
                               Opts, Args);
            false ->
               throw({unknown_option, "--" ++ Opt})
         end
   end;
extract_options(["-" ++ [O|Word]|Rest], OptSpecs, _, Opts, Args)
  when is_integer(O) ->
   ShortOpt = list_to_atom([O]),
   case lists:keysearch(ShortOpt, 4, OptSpecs) of
      {value, OptSpec = #opt{type = boolean}} ->
         extract_options(["-" ++ Word|Rest], OptSpecs, '_',
                         set_opt(OptSpec#opt.actual, true, Opts), Args);
      {value, OptSpec = #opt{type = antiboolean}} ->
         extract_options(["-" ++ Word|Rest], OptSpecs, '_',
                         set_opt(OptSpec#opt.actual, false, Opts), Args);
      {value, OptSpec = #opt{type = count}} ->
         extract_options(["-" ++ Word|Rest], OptSpecs, '_',
                         increment(OptSpec#opt.actual, Opts), Args);
      {value, OptSpec} ->
         case Word of
            "" ->
               extract_options(Rest, OptSpecs, OptSpec, Opts, Args);
            _ ->
               extract_options([Word|Rest], OptSpecs, OptSpec, Opts, Args)
         end;
      false ->
         throw({unknown_option, "-" ++ [O]})
   end;
extract_options([Arg|Rest], OptSpecs, '_', Opts, Args) ->
   extract_options(Rest, OptSpecs, '_', Opts, Args ++ [Arg]);
extract_options([Arg|Rest], OptSpecs, Getting, Opts, Args) ->
   extract_options(Rest, OptSpecs, '_', bytype(Getting, Arg, Opts), Args).

noteq($=) -> false;
noteq(_) -> true.

increment(Opt, OptList) ->
   set_opt(Opt,
           case get_opt(Opt, OptList) of
              '_' -> 1;
              V -> V + 1
           end,
           OptList).

list_to_type({keylist, Subtype}, Val) ->
   case lists:splitwith(fun noteq/1, Val) of
      {Key, "=" ++ Subval} -> {list_to_atom(Key),
                               list_to_type(Subtype, Subval)};
      {Val, []} ->
         throw({error_keylist_option_requires_assignment, Val})
   end;
list_to_type({list, Subtype}, Val) ->
   list_to_type(Subtype, Val);
list_to_type(string, Val) -> Val;
list_to_type(atom, Val) -> list_to_atom(Val);
list_to_type(integer, Val) -> list_to_integer(Val);
list_to_type(float, Val) -> list_to_float(Val).

bytype(OptSpec, Val, OptList) ->
   Type = OptSpec#opt.type,
   set_opt(OptSpec#opt.actual, Type, list_to_type(Type, Val), OptList).

set_opt(Opt, Val, OptList) ->
   lists:keystore(Opt, 1, OptList, {Opt, Val}).

set_opt(Opt, {list, _}, Val, OptList) ->
   case lists:keysearch(Opt, 1, OptList) of
      {value, {Opt, ListVal}} ->
         set_opt(Opt, ListVal ++ [Val], lists:keydelete(Opt, 1, OptList));
      false ->
         set_opt(Opt, [Val], OptList)
   end;
set_opt(Opt, {keylist, _}, {Key, Val}, OptList) ->
   case lists:keysearch(Opt, 1, OptList) of
      {value, {Opt, TupleListVal}} ->
         set_opt(Opt, lists:keystore(Key, 1, TupleListVal, {Key, Val}),
                 lists:keydelete(Opt, 1, OptList));
      false ->
         set_opt(Opt, [{Key, Val}], OptList)
   end;
set_opt(Opt, _, Val, OptList) ->
   set_opt(Opt, Val, OptList).

%% Should be smart about optspec? return false for unset boolean, 0 for
%% unset integer?
%%
%% @doc Retrieve option value. If the option has not been set, the special
%% atom '_' is returned. Note: this may be incompatible with desired behavior
%% if an option of type `atom' is passed an option argument of `_'. The
%% option list is the value returned from {@link extract_options/2}.
%%
%% @spec get_opt(Opt::atom(), OptList::optlist()) -> term()
get_opt(Opt, OptList) ->
   case lists:keysearch(Opt, 1, OptList) of
      {value, {Opt, Val}} ->
         Val;
      _ -> '_'
   end.

%% There must be a better/more efficient way of saying the disjunction of
%% these two lists is/is not the empty set
optequiv(Opts1, {Opts2, _}) ->
   optequiv(Opts1, Opts2);
optequiv(Opts1, Opts2) ->
   case {Opts1 -- Opts2, Opts2 -- Opts1} of
      {[], []} -> true;
      _ -> false
   end.
   

basic_test_() ->
   [
    ?_assertMatch({[{verbose, true}], ["one"]},
                  extract_options(["--verbose", "one"],
                                  [{verbose, boolean}])),
    ?_assertMatch({[{verbose, false}], ["one"]},
                  extract_options(["--noverbose", "one"],
                                  [{verbose, boolean}])),
    ?_assertMatch({[], ["one", "two", "three"]},
                  extract_options(["one", "two", "three"],
                                  [{verbose, boolean}])),
    ?_assertMatch({[{num, 1}], ["one", "two"]},
                  extract_options(["one", "--num", "1", "two"],
                                  [{num, n, integer}])),
    ?_assertMatch({[{num, 1}], ["one", "two"]},
                  extract_options(["one", "--num=1", "two"],
                                  [{num, n, integer}])),
    ?_assertMatch({[{num, 1}], ["one", "two"]},
                  extract_options(["one", "-n", "1", "two"],
                                  [{num, n, integer}])),
    ?_assertMatch({[{num, 1}], ["one", "two"]},
                  extract_options(["one", "-n1", "two"],
                                  [{num, n, integer}])),
    ?_assertMatch({[{num, 1}], ["one", "two", "--verbose", "three"]},
                  extract_options(["one", "-n1", "two", "--",
                                   "--verbose", "three"],
                                  [{num, n, integer}, {verbose, boolean}]))
    ].

type_test_() ->
   [
    ?_assertMatch({[{name, "bob"}], []},
                  extract_options(["--name=bob"],
                                  [{name, string}])),
    ?_assertMatch({[{name, bob}], []},
                  extract_options(["--name=bob"],
                                  [{name, atom}])),
    ?_assertMatch({[{level, 1.3}], []},
                  extract_options(["--level=1.3"],
                                  [{level, float}])),
    ?_assertMatch({[{debug, 3}], ["one"]},
                  extract_options(["--debug", "--debug", "one", "--debug"],
                                  [{debug, count}]))
    ].

optequiv_test_() ->
   [
    ?_assertMatch(true,
                  optequiv([{one, 1}, {two, 2}, {three, 3}],
                           [{two, 2}, {one, 1}, {three, 3}])),
    ?_assertMatch(false,
                  optequiv([{one, 1}, {two, 3}, {three, 3}],
                           [{two, 2}, {one, 1}, {three, 3}])),
    ?_assertMatch(false,
                  optequiv([{one, 1}, {two, 2}, {three, 3}],
                           [])),
    ?_assertMatch(true,
                  optequiv([{one, 1}, {two, 2}],
                           {
                             [{two, 2}, {one, 1}],
                             ["foo", "bar", "baz"]
                            }))
    ].
                  
fail_test_() ->
   [
    ?_assertMatch({unknown_option, "--verbose"},
                  catch extract_options(["--verbose"],
                                  [])),
    ?_assertMatch({unknown_option, "-v"},
                  catch extract_options(["-v"],
                                        [])),
    ?_assertMatch({invalid_subtype_for_type, list, list},
                  catch extract_options([],
                                        [{name, {list, list}}])),
    ?_assertMatch({option_requires_argument, num},
                  catch extract_options(["one", "--num"],
                                        [{num, integer}]))
   ].

intermix_test_() ->
   [
    ?_assert(
       optequiv([{verbose, true},
                 {debug, 3},
                 {num, 1},
                 {level, 2.1},
                 {name, "bob"},
                 {to, alice}],
                extract_options(
                  ["one", "--name=bob", "-n1", "two", "--level", "2.1",
                   "--verbose", "--debug", "--debug", "--to=alice",
                   "three", "--debug", "--", "--debug"],
                  [{verbose, boolean},
                   {debug, d, count},
                   {num, n, integer},
                   {level, float},
                   {name, string},
                   {to, atom}])))
    ].

default_test_() ->
   [
    ?_assert(
       optequiv([{verbose, false},
                 {debug, 4}],
                extract_options(["--debug"],
                                [{debug, count}, {verbose, boolean}],
                                [{verbose, false}, {debug, 3}])))
   ].

complex_test_() ->
   [
    ?_assertMatch({[{name, [bob, alice]}], []},
                  extract_options(["--name=bob", "--name", "alice"],
                                  [{name, {list, atom}}])),
    ?_assertMatch({[{rating, [{bob, 8}, {alice, 7}]}], []},
                  extract_options(["--rating", "bob=8", "--rating", "alice=7"],
                                  [{rating, {keylist, integer}}]))
   ].


boolean_test_() ->
   [
    ?_assertMatch({[{verbose, false}], []},
                  extract_options(["--verbose", "--verbose", "--noverbose",
                                   "--verbose", "--noverbose"],
                                  [{verbose, boolean}]))
    ].
