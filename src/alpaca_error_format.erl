%% -*- coding: utf-8 -*-
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

%% Formatting and translation of error messages.
-module(alpaca_error_format).

-export([fmt/2]).

-ignore_xref([ fmt/2 ]).

-compile({parse_transform, epo_gettext}).

-define(EN_US, "en-US").

%% This function expects all strings passed in to it as part of error messages
%% (e.g. function names) to be valid unicode strings.
-spec fmt({error, term()}, Locale::string()) -> binary().
fmt({error, {parse_error, F, L, E}}, Locale) ->
    File = unicode:characters_to_binary(F, utf8),
    Line = integer_to_binary(L),
    Msg = fmt_parse_error(E, Locale),
    <<File/binary, ":"/utf8, Line/binary, ": "/utf8, Msg/binary, "\n"/utf8>>;
fmt({error, _}=Err, Locale) ->
    Msg = fmt_unknown_error(Err, Locale),
    <<Msg/binary, "\n"/utf8>>.

fmt_parse_error({duplicate_definition, Id}, Locale) ->
    r(_(<<"Duplicate definitition of \"%(id)\".">>, Locale), [{id, Id}]);
fmt_parse_error({duplicate_type, Id}, Locale) ->
    r(_(<<"Duplicate definitition of type \"%(id)\".">>, Locale), [{id, Id}]);
fmt_parse_error({function_not_exported, Mod, Name}, Locale) ->
    r(_(<<"No function %(fun) exported from module %(mod).">>, Locale),
      [{'fun', Name}, {mod, atom_to_binary(Mod, utf8)}]);
fmt_parse_error({invalid_bin_qualifier, Str}, Locale) ->
    r(_(<<"Invalid binary qualifier \"%(qualifier)\".\n"
          "Valid qualifiers are \"end\", \"sign\", \"size\", "
          "\"type\" and \"unit\".">>, Locale),
      [{qualifier, Str}]);
fmt_parse_error({invalid_bin_type, Str}, Locale) ->
    r(_(<<"Invalid binary part type \"%(type)\".\n"
           "Valid types are \"binary\", \"float\", \"int\", and \"utf8\".">>,
        Locale),
      [{type, Str}]);
fmt_parse_error({invalid_endianess, Str}, Locale) ->
    r(_(<<"Invalid endianess \"%(endianess)\"."
           "Did you mean \"big\", \"little\", or \"native\"?">>, Locale),
      [{endianess, Str}]);
fmt_parse_error({invalid_fun_parameter, _}, Locale) ->
    _(<<"Invalid pattern for function argument.">>, Locale);
fmt_parse_error({invalid_top_level_construct, _}, Locale) ->
    _(<<"Invalid top level construct.">>, Locale);
fmt_parse_error({module_rename, Old, New}, Locale) ->
    r(_(<<"Redefintion of module name from \"%(old)\" to \"%(new)\".">>,
        Locale),
      [{old, atom_to_binary(Old, utf8)}, {new, atom_to_binary(New, utf8)}]);
fmt_parse_error(no_module, Locale) ->
    _(<<"No module name defined.\nYou may define it like this: \"module foo\"">>,
      Locale);
fmt_parse_error({no_module, Mod}, Locale) ->
    r(_(<<"Cannot find module \"%(mod)\".">>, Locale), [{module, Mod}]);
fmt_parse_error({syntax_error, ""}, Locale) ->
    _(<<"Incomplete expression.">>, Locale);
fmt_parse_error({syntax_error, Token}, Locale) ->
    r(_(<<"Unexpected token \"%(token)\".">>, Locale), [{token, Token}]);
fmt_parse_error({wrong_type_arity, t_atom, _A}, Locale) ->
    simple_type_arity_error("atom", Locale);
fmt_parse_error({wrong_type_arity, t_binary, _A}, Locale) ->
    simple_type_arity_error("binary", Locale);
fmt_parse_error({wrong_type_arity, t_bool, _A}, Locale) ->
    simple_type_arity_error("bool", Locale);
fmt_parse_error({wrong_type_arity, t_float, _A}, Locale) ->
    simple_type_arity_error("float", Locale);
fmt_parse_error({wrong_type_arity, t_int, _A}, Locale) ->
    simple_type_arity_error("int", Locale);
fmt_parse_error({wrong_type_arity, t_list, A}, Locale) ->
    poly_type_arity_error("list", 1, A, Locale);
fmt_parse_error({wrong_type_arity, t_map, A}, Locale) ->
    poly_type_arity_error("map", 2, A, Locale);
fmt_parse_error({wrong_type_arity, t_pid, A}, Locale) ->
    poly_type_arity_error("pid", 1, A, Locale);
fmt_parse_error({wrong_type_arity, t_string, _A}, Locale) ->
    simple_type_arity_error("string", Locale);
fmt_parse_error(Unknown, Locale) ->
    fmt_unknown_error(Unknown, Locale).

simple_type_arity_error(LiteralType, Locale) ->
    r(_(<<"Type parameter provided for builtin type %(type), "
           "but none was expected.">>, Locale),
      [{type, LiteralType}]).

poly_type_arity_error(LiteralType, ExpectedArity, ActualArity, Locale) ->
    r(_(<<"Wrong number of type parameters provided for builtin type %(type).\n"
          "Expected %(num_expected), but got %(num_supplied).">>, Locale),
      [{type, LiteralType},
       {num_expected, integer_to_binary(ExpectedArity)},
       {num_supplied, integer_to_binary(ActualArity)}]).

fmt_unknown_error(Err, Locale) ->
    r(_(<<"%(raw_error_term)\n"
          "Sorry, we do not have a proper message for this error yet.\n"
          "Please consider filing an issue at "
          "https://www.github.com/alpaca-lang/alpaca.">>, Locale),
      [{raw_error_term, io_lib:format("~tp", [Err])}]).

r(TranslatedStr, Replacements) ->
  lists:foldl(fun({FromAtom, To}, Str) ->
    FromStr = "%\\(" ++ atom_to_list(FromAtom) ++ "\\)",
    re:replace(Str, FromStr, To, [global, unicode, {return, binary}])
  end, TranslatedStr, Replacements).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fmt_unknown_parse_error_test() ->
  File = "/tmp/file.alpaca",
  Line = 10,
  ParseError = unknown,
  Error = {error, {parse_error, File, Line, ParseError}},
  Msg = fmt(Error, "en_US"),
  Expected = <<"/tmp/file.alpaca:10: unknown\n"
               "Sorry, we do not have a proper message for this error yet.\n"
               "Please consider filing an issue at "
               "https://www.github.com/alpaca-lang/alpaca.\n">>,
  ?assertEqual(Expected, Msg).

fmt_unknown_error_test() ->
  Error = {error, unknown},
  Msg = fmt(Error, "en_US"),
  Expected = <<"{error,unknown}\n"
               "Sorry, we do not have a proper message for this error yet.\n"
               "Please consider filing an issue at "
               "https://www.github.com/alpaca-lang/alpaca.\n">>,
  ?assertEqual(Expected, Msg).

-endif.
