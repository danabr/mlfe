%% Performs exhaustiveness checking of pattern matches.
%%
%% Only deals with top level functions, as the typer currently does not
%% expose type information on the expression level.
-module(alpaca_exhaustiveness).

-export([check_exhaustiveness/1]).
-export([print_warning/1]).

-compile([export_all]).

-include("alpaca_ast.hrl").

-type pattern() :: {missing_pattern, term()}.
-type warning() :: {partial_function, Module::atom(), alpaca_fun_def(),
                    [pattern()]}.

print_warning({partial_function, Mod, F, Patterns}) ->
    {symbol, _, Name} = F#alpaca_fun_def.name,
    Arity = F#alpaca_fun_def.arity,
    io:format("Warning: Partial function ~p.~s/~w. Missing patterns:~n",
        [Mod, Name, Arity]),
    lists:foreach(fun(P) ->  print_pattern(P, Name) end, Patterns).

print_pattern({missing_pattern, Args}, FName) ->
    Formatted = lists:map(fun format_pattern/1, Args),
    io:format("  let ~s ~s = ...~n", [FName, string:join(Formatted, " ")]).

format_pattern({t_adt_cons, C, none}) -> C;
format_pattern({t_adt_cons, C, Arg})  ->
    "(" ++ C ++ " " ++ format_pattern(Arg) ++ ")";
format_pattern({t_bool, Bool}) -> atom_to_list(Bool);
format_pattern({t_list, empty}) -> "[]";
format_pattern({t_list, P}) ->
    "(" ++ format_pattern(P) ++ " :: _)";
format_pattern(t_map) -> "#{}";
format_pattern({t_tuple, Elems}) ->
    Parts = lists:map(fun(E) -> format_pattern(E) end, Elems),
    "(" ++ string:join(Parts, ", ") ++ ")";
format_pattern(t_unit) -> "()";
format_pattern({t_record, Assignments}) ->
    Fields = lists:map(fun({K, V}) ->
             atom_to_list(K) ++ " = " ++ format_pattern(V) end,
           maps:to_list(Assignments)),
    "{ " ++ string:join(Fields, ", ") ++ " }";
format_pattern('_') -> "_".

-spec check_exhaustiveness([alpaca_module()]) -> [warning()].
check_exhaustiveness(Mods) ->
    lists:flatmap(fun(M) -> check_exhaustiveness(M, Mods) end, Mods).

check_exhaustiveness(#alpaca_module{functions=Funs}=M, AllMods) ->
    lists:flatmap(fun(F) -> check_exhaustiveness(M, F, AllMods) end, Funs).

check_exhaustiveness(Mod, #alpaca_fun_def{type=Type}=F, AllMods) ->
    case Type of
        {t_arrow, FunArgTypes, _}                  ->
          check_exhaustiveness(Mod, F, FunArgTypes, AllMods);
        {t_receiver, _, {t_arrow, FunArgTypes, _}} ->
          check_exhaustiveness(Mod, F, FunArgTypes, AllMods);
         _                                         -> % Top level value
            []
    end.

check_exhaustiveness(Mod, #alpaca_fun_def{versions=FunArgPatterns}=F,
                     FunArgTypes, AllMods) ->
    case missing_patterns(Mod, FunArgTypes, FunArgPatterns, AllMods) of
        []              ->
            [];
        MissingPatterns ->
            [{partial_function, Mod#alpaca_module.name, F, MissingPatterns}]
    end.

missing_patterns(Mod, FunArgTypes, FunArgPatterns, AllMods) ->
    CoveringPatterns = covering_patterns(FunArgTypes, Mod, AllMods),
    ProvidedPatterns = extract_patterns(FunArgPatterns),
    lists:flatmap(fun({t_tuple, FunArgs}=CovP) ->
        case lists:any(fun(P) -> covered(CovP, P) end, ProvidedPatterns) of
            true  -> [];
            false -> [{missing_pattern, FunArgs}]
        end
      end, CoveringPatterns).

covering_patterns(FunArgTypes, Mod, AllMods) ->
    covering_patterns({t_tuple, FunArgTypes}, Mod, AllMods, sets:new(), []).

covering_patterns(#adt{name=Name, vars=Vars}, Mod, AllMods, SeenADTs, _Vars) ->
    wildcard_if_seen(Name, Mod, AllMods, SeenADTs, Vars);
covering_patterns(#alpaca_type{members=[], name={type_name, _, Name},
                               vars=Vars}, Mod, AllMods, SeenADTs, _Vars) ->
    wildcard_if_seen(Name, Mod, AllMods, SeenADTs, Vars);
covering_patterns(#alpaca_type{members=Members}, Mod, AllMods, SeenADTs,
                  Vars) ->
    lists:flatmap(fun(C) ->
        covering_patterns(C, Mod, AllMods, SeenADTs, Vars)
    end, Members);
covering_patterns(#alpaca_type_tuple{members=Members}, Mod, AllMods, SeenADTs,
                  Vars) ->
    covering_patterns({t_tuple, Members}, Mod, AllMods, SeenADTs, Vars);
covering_patterns(#alpaca_constructor{name={type_constructor, _, N}, arg=none},
                  _Mod, _AllMods, _SeenADTs, _Vars) ->
    [{t_adt_cons, N, none}];
covering_patterns(#alpaca_constructor{name={type_constructor, _, N}, arg=Arg},
             Mod, AllMods, SeenADTs, Vars) ->
    ArgPatterns = covering_patterns(Arg, Mod, AllMods, SeenADTs, Vars),
    lists:map(fun(A) -> {t_adt_cons, N, A} end, ArgPatterns);
covering_patterns({t_arrow, _, _}, _Mod, _AllMods, _SeenADTs, _Vars) ->
    ['_'];
covering_patterns(t_atom, _Mod, _AllMods, _SeenADTs, _Vars) ->
    ['_'];
covering_patterns(t_binary, _Mod, _AllMods, _SeenADTs, _Vars) ->
    ['_'];
covering_patterns(t_bool, _Mod, _AllMods, _SeenADTs, _Vars) ->
    [{t_bool, true}, {t_bool, false}];
covering_patterns(t_chars, _Mod, _AllMods, _SeenADTs, _Vars) ->
    ['_'];
covering_patterns(t_float, _Mod, _AllMods, _SeenADTs, _Vars) ->
    ['_'];
covering_patterns(t_int, _Mod, _AllMods, _SeenADTs, _Vars) ->
    ['_'];
covering_patterns({t_list, Elem}, Mod, AllMods, SeenADTs, Vars) ->
    ElemPatterns = covering_patterns(Elem, Mod, AllMods, SeenADTs, Vars),
    Base = lists:map(fun(E) -> {t_list, E} end, ElemPatterns),
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
covering_patterns({alpaca_map, _KeyT, _ValT}, _Mod, _AllMods, _SeenADTs,
                  _Vars) ->
    [t_map];
covering_patterns({t_map, _KeyT, _ValT}, _Mod, _AllMods, _SeenADTs, _Vars) ->
    [t_map];
covering_patterns(#t_record{members=Ms}, Mod, AllMods, SeenADTs, Vars) ->
    Assignments = assignments(Ms, Mod, AllMods, SeenADTs, Vars),
    lists:map(fun(A) -> {t_record, A} end, Assignments);
covering_patterns(t_string, _Mod, _AllMods, _SeenADTs, _Vars) ->
    ['_'];
covering_patterns({t_tuple, Ms}, Mod, AllMods, SeenADTs, Vars) ->
    lists:map(fun(A) -> {t_tuple, maps:values(A)} end,
              tuple_patterns(Ms, 1, Mod, AllMods, SeenADTs, Vars));
covering_patterns(t_unit, _Mod, _AllMods, _SeenADTs, _Vars) ->
    [t_unit];
covering_patterns({type_var, _, Var}, Mod, AllMods, SeenADTs, Vars) ->
    {Var, C} = lists:keyfind(Var, 1, Vars),
    covering_patterns(C, Mod, AllMods, SeenADTs, Vars);
covering_patterns({unbound, _, _}, _Mod, _AllMods, _SeenADTs, _Vars) ->
    ['_'].

wildcard_if_seen(Name, Mod, AllMods, SeenADTs0, Vars) ->
    case sets:is_element(Name, SeenADTs0) of
        true  ->
            ['_'];
        false ->
            {ok, T} = lookup_type(Name, Mod, AllMods),
            SeenADTs = sets:add_element(Name, SeenADTs0),
            covering_patterns(T, Mod, AllMods, SeenADTs, Vars)
    end.

lookup_type(Name, Mod, AllMods) ->
  case lookup_type(Mod#alpaca_module.types, Name) of
     {ok, _}=Res    ->
        Res;
     {not_found, _} ->
        lookup_type_from_imports(Name, Mod, AllMods)
  end.

lookup_type([], Name) ->
    {not_found, Name};
lookup_type([#alpaca_type{name={type_name, _, Name}}=T|_], Name) ->
    {ok, T};
lookup_type([_|Rest], Name) ->
    lookup_type(Rest, Name).

lookup_type_from_imports(Name, #alpaca_module{type_imports=Imports},
                         AllMods) ->
    #alpaca_type_import{module=ModName} =
        lists:keyfind(Name, #alpaca_type_import.type, Imports),
    
    Mod = lists:keyfind(ModName, #alpaca_module.name, AllMods),
    lookup_type(Mod#alpaca_module.types, Name). 

assignments([], _Mod, _AllMods, _SeenADTs, _Vars) ->
    [#{}];
assignments([#t_record_member{name=Key, type=T}|Rest], Mod, AllMods, SeenADTs,
            Vars) ->
    RestAssignments = assignments(Rest, Mod, AllMods, SeenADTs, Vars),
    lists:flatmap(fun(C) ->
        lists:map(fun(A) -> maps:put(Key, C, A) end, RestAssignments)
    end, covering_patterns(T, Mod, AllMods, SeenADTs, Vars)).

tuple_patterns([], _Ix, _Mod, _AllMods, _SeenADTs, _Vars) ->
    [#{}];
tuple_patterns([T|Rest], Ix, Mod, AllMods, SeenADTs, Vars) ->
    RestPatterns = tuple_patterns(Rest, Ix+1, Mod, AllMods, SeenADTs, Vars),
    lists:flatmap(fun(C) ->
        lists:map(fun(A) -> maps:put(Ix, C, A) end, RestPatterns)
    end, covering_patterns(T, Mod, AllMods, SeenADTs, Vars)).

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
