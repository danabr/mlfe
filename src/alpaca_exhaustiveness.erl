%% Performs exhaustiveness checking of pattern matches.
%%
%% Only deals with top level functions, as the typer currently does not
%% expose type information on the expression level.
%%
%% Note: We do not deal with guards, since they can be arbitrary expressions.
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
%% Note: We don't handle union types!
-module(alpaca_exhaustiveness).

-export([check_exhaustiveness/1]).
-export([print_warning/1]).

-compile([export_all]).

-include("alpaca_ast.hrl").

check_exhaustiveness(#alpaca_module{functions=Funs, types=Ts}) ->
  lists:flatmap(fun(F) -> check_exhaustiveness(F, Ts) end, Funs).

print_warning({partial_function, F, Patterns}) ->
  {symbol, _, Name} = F#alpaca_fun_def.name,
  Arity = F#alpaca_fun_def.arity,
  io:format("Warning: Partial function ~s/~w. Missing patterns:~n",
            [Name, Arity]),
  lists:foreach(fun(P) ->  print_pattern(P, Name) end, Patterns).

print_pattern({missing_pattern, Args}, FName) ->
  Formatted = lists:map(fun format_pattern/1, Args),
  io:format("  let ~s ~s = ...~n", [FName, string:join(Formatted, " ")]).

format_pattern({t_adt_cons, C, none})   -> C;
format_pattern({t_adt_cons, C, Arg})    ->
  "(" ++ C ++ " " ++ format_pattern(Arg) ++ ")";
format_pattern({t_bool, Bool})    -> atom_to_list(Bool);
format_pattern({t_list, empty})   -> "[]";
format_pattern({t_list, C})       ->
  "(" ++ format_pattern(C) ++ " :: _)";
format_pattern(t_map)             -> "#{}";
format_pattern({t_tuple, Elems}) ->
  Parts = lists:map(fun(E) -> format_pattern(E) end, Elems),
  "(" ++ string:join(Parts, ", ") ++ ")";
format_pattern(t_unit)            -> "()";
format_pattern({t_record, Assignments}) ->
  Fields = lists:map(fun({K, V}) ->
    atom_to_list(K) ++ " = " ++ format_pattern(V) end,
  maps:to_list(Assignments)),
  "{ " ++ string:join(Fields, ", ") ++ " }";
format_pattern('_')               -> "_".

check_exhaustiveness(#alpaca_fun_def{type=Type}=F, ModTypes) ->
  case Type of
    {t_arrow, FunArgTypes, _}                  ->
      check_exhaustiveness(F, FunArgTypes, ModTypes);
    {t_receiver, _, {t_arrow, FunArgTypes, _}} ->
      check_exhaustiveness(F, FunArgTypes, ModTypes);
    _                                          -> % Top level value
      []
  end.

check_exhaustiveness(#alpaca_fun_def{versions=FunArgPatterns}=F, FunArgTypes,
                    ModTypes) ->
  case missing_patterns(FunArgTypes, FunArgPatterns, ModTypes) of
    []              -> [];
    MissingPatterns -> [{partial_function, F, MissingPatterns}]
  end.

missing_patterns(FunArgTypes, FunArgPatterns, ModTypes) ->
  Constructors = constructors({t_tuple, FunArgTypes}, ModTypes, sets:new(), []),
  Patterns = extract_patterns(FunArgPatterns),
  lists:flatmap(fun({t_tuple, FunArgs}=Constr) ->
    case lists:any(fun(P) -> covered(Constr, P) end, Patterns) of
      true  -> [];
      false -> [{missing_pattern, FunArgs}]
    end
  end, Constructors).

constructors(#adt{name=Name, vars=Vars}, ModTypes, SeenADTs, _Vars) ->
  wildcard_if_seen(Name, ModTypes, SeenADTs, Vars);
constructors(#alpaca_type{members=[], name={type_name, _, Name}, vars=Vars},
             ModTypes, SeenADTs, _Vars) ->
  wildcard_if_seen(Name, ModTypes, SeenADTs, Vars);
constructors(#alpaca_type{members=Members}, ModTypes, SeenADTs, Vars) ->
  lists:flatmap(fun(C) -> constructors(C, ModTypes, SeenADTs, Vars) end, Members);
constructors(#alpaca_type_tuple{members=Members}, ModTypes, SeenADTs, Vars) ->
  constructors({t_tuple, Members}, ModTypes, SeenADTs, Vars);
constructors(#alpaca_constructor{name={type_constructor, _, N}, arg=none},
             _ModTypes, _SeenADTs, _Vars) ->
  [{t_adt_cons, N, none}];
constructors(#alpaca_constructor{name={type_constructor, _, N}, arg=Arg},
             ModTypes, SeenADTs, Vars) ->
  lists:map(fun(A) -> {t_adt_cons, N, A} end, constructors(Arg, ModTypes, SeenADTs, Vars));
constructors({t_arrow, _, _}, _ModTypes, _SeenADTs, _Vars) -> ['_'];
constructors(t_atom, _ModTypes, _SeenADTs, _Vars)          -> ['_'];
constructors(t_binary, _ModTypes, _SeenADTs, _Vars)        -> ['_'];
constructors(t_bool, _ModTypes, _SeenADTs, _Vars)          -> [{t_bool, true}, {t_bool, false}];
constructors(t_chars, _ModTypes, _SeenADTs, _Vars)         -> ['_'];
constructors(t_float, _ModTypes, _SeenADTs, _Vars)         -> ['_'];
constructors(t_int, _ModTypes, _SeenADTs, _Vars)           -> ['_'];
constructors({t_list, Elem}, ModTypes, SeenADTs, Vars)      ->
  Base = lists:map(fun(E) -> {t_list, E} end, constructors(Elem, ModTypes, SeenADTs, Vars)),
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
constructors({t_map, _KeyT, _ValT}, _ModTypes, _SeenADTs, _Vars) -> [t_map];
constructors(#t_record{members=Ms}, ModTypes, SeenADTs, Vars)  ->
  lists:map(fun(A) -> {t_record, A} end, assignments(Ms, ModTypes, SeenADTs, Vars));
constructors(t_string, _ModTypes, _SeenADTs, _Vars)              -> ['_'];
constructors({t_tuple, Ms}, ModTypes, SeenADTs, Vars)          ->
  lists:map(fun(A) -> {t_tuple, maps:values(A)} end,
            tuple_patterns(Ms, 1, ModTypes, SeenADTs, Vars));
constructors(t_unit, _ModTypes, _SeenADTs, _Vars)             -> [t_unit];
constructors({type_var, _, Var}, ModTypes, SeenADTs, Vars)    ->
  {Var, C} = lists:keyfind(Var, 1, Vars),
  constructors(C, ModTypes, SeenADTs, Vars);
constructors({unbound, _, _}, _ModTypes, _SeenADTs, _Vars)    -> ['_'].

wildcard_if_seen(Name, ModTypes, SeenADTs, Vars) ->
  case sets:is_element(Name, SeenADTs) of
    true  -> ['_'];
    false ->
      {ok, T} = lookup_type(ModTypes, Name),
      constructors(T, ModTypes, sets:add_element(Name, SeenADTs), Vars)
  end.

lookup_type([], Name) -> {not_found, Name};
lookup_type([#alpaca_type{name={type_name, _, Name}}=T|_], Name) ->
  {ok, T};
lookup_type([_|Rest], Name) ->
  lookup_type(Rest, Name).


assignments([], _ModTypes, _SeenADTs, _Vars) -> [#{}];
assignments([#t_record_member{name=Key, type=T}|Rest], ModTypes, SeenADTs, Vars) ->
  RestAssignments = assignments(Rest, ModTypes, SeenADTs, Vars),
  lists:flatmap(fun(C) ->
    lists:map(fun(A) -> maps:put(Key, C, A) end, RestAssignments)
  end, constructors(T, ModTypes, SeenADTs, Vars)).

tuple_patterns([], _Ix, _ModTypes, _SeenADTs, _Vars) -> [#{}];
tuple_patterns([T|Rest], Ix, ModTypes, SeenADTs, Vars) ->
  RestPatterns = tuple_patterns(Rest, Ix+1, ModTypes, SeenADTs, Vars),
  lists:flatmap(fun(C) ->
    lists:map(fun(A) -> maps:put(Ix, C, A) end, RestPatterns)
  end, constructors(T, ModTypes, SeenADTs, Vars)).

extract_patterns(FunArgPatterns) ->
  lists:map(fun(#alpaca_fun_version{args=Args}) ->
    #alpaca_tuple{values=Args}
  end, FunArgPatterns).

covered('_', Pattern)                      ->
  matches_wildcard(Pattern);
covered({t_adt_cons, Name, CNeedle}, Pattern)    ->
  matches_constructor(Pattern, Name, CNeedle);
covered({t_bool, Boolean}, Pattern)        ->
  matches_bool(Pattern, Boolean);
covered({t_list, empty}, Pattern)          ->
    matches_empty_list(Pattern);
covered({t_list, Elem}, Pattern)           ->
  matches_list(Pattern, Elem);
covered(t_map, _Pattern)                   ->
  true;
covered({t_record, Assignments}, Pattern)  ->
  matches_record(Pattern, Assignments);
covered({t_tuple, Members}, Pattern) ->
  matches_tuple(Pattern, Members);
covered(t_unit, Pattern)                   ->
  matches_unit(Pattern).

to_val(#alpaca_type_apply{name={type_constructor, _, CName}}) -> {t_adt_cons, CName};
to_val({boolean, _Line, Val}) -> {t_bool, Val};
to_val({'_', _Line})          -> '_'.

matches_bool({boolean, _, Bool}, Bool) -> true;
matches_bool(Other, _Bool)             -> matches_wildcard(Other).

matches_constructor(#alpaca_type_apply{name={type_constructor, _, Name},
                                       arg=none}, Name, none) ->
  true;
matches_constructor(#alpaca_type_apply{name={type_constructor, _, Name},
                                       arg=Arg}, Name, CNeedle) ->
  covered(CNeedle, Arg);
matches_constructor(C, _Name, _Needle) -> matches_wildcard(C).

matches_empty_list({nil, _}) -> true;
matches_empty_list(C)        -> matches_wildcard(C).

matches_list(#alpaca_cons{head=H, tail=T}, E) ->
  covered(E, H) andalso matches_wildcard(T);
matches_list(P, _E) ->
  matches_wildcard(P).

matches_record(#alpaca_record{members=Ms}, Assignments) ->
  lists:all(fun(#alpaca_record_member{name=N, val=P}) ->
    covered(maps:get(N, Assignments), P)
  end, Ms);
matches_record(C, _Assignments) -> matches_wildcard(C).

matches_tuple(#alpaca_tuple{values=Patterns}, TElems) ->
  matches(Patterns, TElems);
matches_tuple(Other, _TElems) -> matches_wildcard(Other).

matches([], []) -> true;
matches([Pattern|Patterns], [Constructor|Constructors]) ->
  covered(Constructor, Pattern) andalso matches(Patterns, Constructors).

matches_unit({unit, _}) -> true;
matches_unit(C)         -> matches_wildcard(C).

matches_wildcard({'_', _})       -> true;
matches_wildcard({symbol, _, _}) -> true;
matches_wildcard(_)              -> false.
