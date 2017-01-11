-module(alpaca_exhaustiveness).

-export([check_exhaustiveness/1]).
-export([print_warning/1]).

-compile([export_all]).

-include("alpaca_ast.hrl").

check_exhaustiveness(#alpaca_module{functions=Funs}) ->
  lists:flatmap(fun test_exhaustiveness/1, Funs).

print_warning({partial_function, F, Patterns}) ->
  {symbol, _, Name} = F#alpaca_fun_def.name,
  Arity = F#alpaca_fun_def.arity,
  io:format("Warning: Partial function ~s/~w. Missing patterns:~n",
            [Name, Arity]),
  lists:foreach(fun(P) ->  print_pattern(P, Name) end, Patterns).

print_pattern({missing_pattern, Args}, FName) ->
  Formatted = lists:map(fun format_pattern/1, Args),
  io:format("  let ~s ~s = ...~n", [FName, string:join(Formatted, " ")]).

format_pattern({t_adt_cons, C})   -> C;
format_pattern({t_bool, Bool})    -> atom_to_list(Bool);
format_pattern({t_list, empty})   -> "[]";
format_pattern({t_list, C})       ->
  "(" ++ format_pattern(C) ++ " :: _)";
format_pattern(t_map)             -> "#{}";
format_pattern({t_tuple, Size, Ix, C}) ->
  Parts = lists:duplicate(Ix-1, "_") ++
          [format_pattern(C)|lists:duplicate(Size-Ix, "_")],
  "(" ++ string:join(Parts, ",") ++ ")";
format_pattern(t_unit)            -> "()";
format_pattern({t_record, Assignments}) ->
  Fields = lists:map(fun({K, V}) ->
    atom_to_list(K) ++ " = " ++ format_pattern(V) end,
  maps:to_list(Assignments)),
  "{ " ++ string:join(Fields, ", ") ++ " }";
format_pattern('_')               -> "_".

test_exhaustiveness(#alpaca_fun_def{type=Type}=F) ->
  case Type of
    {t_arrow, FunArgTypes, _}                  ->
      test_exhaustiveness(F, FunArgTypes);
    {t_receiver, _, {t_arrow, FunArgTypes, _}} ->
      test_exhaustiveness(F, FunArgTypes);
    _                                          -> % Top level value
      []
  end.

test_exhaustiveness(#alpaca_fun_def{versions=FunArgPatterns}=F, FunArgTypes) ->
  case exhaustiveness(FunArgTypes, FunArgPatterns) of
    []              -> [];
    MissingPatterns -> [{partial_function, F, MissingPatterns}]
  end.
%% For each argument a with index i:
%%   For each constructor of typeof(a):
%%     * Extract matching patterns, assuming all previous argument patterns match.
%%     * For each constructor argument c:
%%       Assert that there is at least one pattern covering c.
exhaustiveness(FunArgTypes, FunArgPatterns) ->
  NumArgs = length(FunArgTypes),
  flatmap_with_index(FunArgTypes, fun(FunArgType, ArgIx) ->
    Patterns = extract_patterns(FunArgPatterns, ArgIx),
    ArgsAfter = NumArgs-ArgIx-1,
    map(constructors(FunArgType), fun(Constr) ->
      case covered(Constr, Patterns) of
        true -> [];
        false ->
          MissingPattern = lists:duplicate(ArgIx, '_') ++
                           [Constr|lists:duplicate(ArgsAfter, '_')],
          [{missing_pattern, MissingPattern}]
      end
    end)
  end).

flatmap_with_index(List, F) ->
  lists:flatten(map_with_index(List, 0, [], F)).

map_with_index([], _Ix, Acc, _F) -> lists:reverse(Acc);
map_with_index([X|Rest], Ix, Acc, F) ->
  map_with_index(Rest, Ix+1, [F(X, Ix)|Acc], F).

map(List, F) -> lists:map(F, List).

constructors(#adt{members=Constructors}=A) ->
  lists:flatmap(fun constructors/1, Constructors);
constructors({t_adt_cons, N}=C) when is_list(N) -> [C];
constructors({t_arrow, _, _})     -> ['_'];
constructors(t_atom)              -> ['_'];
constructors(t_binary)            -> ['_'];
constructors(t_bool)              -> [{t_bool, true}, {t_bool, false}];
constructors(t_chars)             -> ['_'];
constructors(t_float)             -> ['_'];
constructors(t_int)               -> ['_'];
constructors({t_list, Elem})      ->
  Base = lists:map(fun(E) -> {t_list, E} end, constructors(Elem)),
  [{t_list, empty}|Base];
%% We explicitly ignore maps.
%% Consider this example:
%%   let foo #{true => false,  false => true} = ...
%%
%% The most helpful patterns to report would be:
%%   let foo #{true => false, false => false} = ...
%%   let foo #{true => true, false => true } = ...
%%   let foo #{true => true, false => false } = ...
%%
%% However, to do this, we would need to know all the keys that are used
%% in the patterns, and we do not get that information from the type.
constructors({t_map, _KeyT, _ValT})  -> [t_map];
constructors(#t_record{members=Ms}) ->
  lists:map(fun(A) -> {t_record, A} end, assignments(Ms));
constructors(t_string)              -> ['_'];
constructors({t_tuple, Members})    ->
  TupleSize = length(Members),
  F = fun(Member, Ix) ->
    MemberConstructors = constructors(Member),
    lists:map(fun(C) ->
      {t_tuple, TupleSize, Ix+1, C}
    end, MemberConstructors)
  end,
  flatmap_with_index(Members, F);
constructors(t_unit)             -> [t_unit];
constructors({unbound, _, _})    -> ['_'].

assignments([]) -> [#{}];
assignments([#t_record_member{name=Key, type=T}|Rest]) ->
  RestAssignments = assignments(Rest),
  lists:flatmap(fun(C) ->
    lists:map(fun(A) -> maps:put(Key, C, A) end, RestAssignments)
  end, constructors(T)).

extract_patterns(FunArgPatterns, ArgIx) ->
  lists:map(fun(FV) -> extract_pattern(FV, ArgIx) end, FunArgPatterns).

extract_pattern(#alpaca_fun_version{args=Args}, Ix) ->
  lists:nth(Ix+1, Args).

covered('_', Patterns) ->
  lists:any(fun matches_wildcard/1, Patterns);
covered({t_adt_cons, CNeedle}, Patterns) ->
  lists:any(fun(P) -> matches_constructor(P, CNeedle) end, Patterns);
covered({t_bool, Boolean}, Patterns) ->
  lists:any(fun(P) -> matches_bool(P, Boolean) end, Patterns);
covered({t_list, empty}, Patterns) ->
    lists:any(fun matches_empty_list/1, Patterns);
covered({t_list, Elem}, Patterns) ->
  lists:any(fun(P) -> matches_list(P, Elem) end, Patterns);
covered(t_map, _Patterns) -> true;
covered({t_record, Assignments}, Patterns) ->
  lists:any(fun(P) -> matches_record(P, Assignments) end, Patterns);
covered({t_tuple, TSize, Ix, MC}, Patterns) ->
  lists:any(fun(P) -> matches_tuple(P, TSize, Ix, MC) end, Patterns);
covered(t_unit, Patterns) ->
  lists:any(fun matches_unit/1, Patterns).

to_val(#alpaca_type_apply{name={type_constructor, _, CName}}) -> {t_adt_cons, CName};
to_val({boolean, _Line, Val}) -> {t_bool, Val};
to_val({'_', _Line})          -> '_'.

matches_bool({boolean, _, Bool}, Bool) -> true;
matches_bool(Other, _Bool)             -> matches_wildcard(Other).

matches_constructor(#alpaca_type_apply{name={type_constructor, _, CName}},
                    CNeedle) ->
  CName =:= CNeedle;
matches_constructor(C, _CName) -> matches_wildcard(C).

matches_empty_list({nil, _}) -> true;
matches_empty_list(C)        -> matches_wildcard(C).

matches_list(#alpaca_cons{head=H, tail=T}, E) ->
  covered(E, [H]) andalso matches_wildcard(T);
matches_list(P, _E) ->
  matches_wildcard(P).

matches_record(#alpaca_record{members=Ms}, Assignments) ->
  lists:all(fun(#alpaca_record_member{name=N, val=P}) ->
    covered(maps:get(N, Assignments), [P])
  end, Ms);
matches_record(C, _Assignments) -> matches_wildcard(C).

matches_tuple(#alpaca_tuple{values=Patterns}, _TSize, ElemIx, TConstr) ->
  Pattern = lists:nth(ElemIx, Patterns),
  covered(TConstr, [Pattern]); 
matches_tuple(Other, _, _, _) -> matches_wildcard(Other).

matches_unit({unit, _}) -> true;
matches_unit(C)         -> matches_wildcard(C).

matches_wildcard({'_', _})       -> true;
matches_wildcard({symbol, _, _}) -> true;
matches_wildcard(_)              -> false.

%% 1. Find all match expressions and pattern matching function heads.
%% For each match expression/function head:
%% 1.1 Collect all clauses.
%% 1.2 If any of the clauses is a wildcard, we are done. Otherwise go to 1.3.
%% 1.3 If any of the clauses contains a guard, print a warning. Otherwise go to 1.4
%% 1.4 Use the clauses to reconstruct which type it must be.
%% 1.5. Generate the "principal" patterns for the type, i.e.
%%      the patterns that must be covered for the match to be total.
%% 1.6 Check that all the principal patterns are covered. 
%%
%% Note 1.2: We do not deal with guards, since they can be arbitrary expressions.
%% Here are two examples that both cover the full set of integers:
%%   match i with
%%     x, x < 50 -> :ok
%%     x, x = 50 -> :ok
%%     x, x > 50 -> :ok
%%
%%   match i with
%%     x, x % 2 = 0 -> :ok
%%     x, x % 2 = 1 -> :ok
%%
%% Note 1.4: We don't handle union types!
%%
%% Principal patterns, enums:
%% type color = Red | Green | Blue
%% -- principal patterns = Red + Green + Blue
%%
%% Principal patterns, complex functions:
%% type gimp = Surface color color
%%
%% match g with
%%   Surface Red Red -> :ok
%%   Surface Red Green -> :ok
%%   Surface Red Blue -> :ok
%%   Surface Green Red -> :ok
%%   Surface Green Green -> :ok
%%   Surface Green Blue -> :ok
%%   Surface Blue Red -> :ok
%%   Surface Blue Green -> :ok
%%   Surface Blue Blue -> :ok
%%
%% Principal patterns, polymorphic data structures:
%% type List 'a = Nil | Cons 'a 
%% -- principal patterns = enum patterns + \/ x:'a Cons x
%% Note: If we don't have any information about 'a, totality is;  Nil + Cons _
%%
%% Princial patterns, recursive data types:
%% type Tree = Leaf color | Node Tree Tree
%%
%% match t with
%%   Leaf _ -> :ok
%% | Tree Leaf Leaf ->kk
%% | Tree _ _ > :ok
%%
%% What about first checking all top level constructors, assuming that all
%% arguments are wildcards. If that passes the totality checker, then go through
%% one constructor at a time, put all arguments into a tuple, and check if that
%% totality checks. Or should we totality check the arguments one at a time?
%%
