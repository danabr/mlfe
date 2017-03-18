%%EPO COMPILED FILE
%% -*- coding: utf-8 -*-
-module('alpaca_compiled_po').
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(nowarn_unused_record).

-record(porec2, {msgstr, msgstr_n = {}, n_max}).
-export([get_record/2, get_idx/2]).
-ignore_xref([get_record/2, get_idx/2]).
get_idx(N, <<"en_US">>) ->
	to_integer(to_integer(N)
			      =/= to_integer(1));
get_idx(N, <<Locale2:2/binary, $_, _/binary>>) ->
	get_idx(N, Locale2);
get_idx(_, _) ->
	0.

get_record(Key, Locale) ->
	case Key of		<<"invalid_bin_type %(type)"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Invalid binary part type \"%(type).\nValid types are \"binary\", \"float, \"int\", and \"utf8\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"invalid_bin_qualifier %(qualifier)"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Invalid binary qualifier \"%(qualifier)\".\nValid qualifiers are \\\"end\"\n-\"\\\", \\\"sign\\\", \\\"size\\\", \\\"type\\\" and \\\"unit\\\".\""/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"type_parameter_given_to_primitive_builtin_type %(type)"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Type parameter provided for built in type \"%(type)\", but none were expected."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"duplicate_definition %(id)"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Duplicate definition of \"%(id)\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"invalid_top_level_construct"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"This construct may not appear at the top level."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"no_module"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"No module name defined.\nYou may define it like this: \"module foo\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"function_not_exported %(mod) %(name)"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"No function \"%(fun)\" exported from module \"%(mod)\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"no_module %(mod)"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Cannot find any module named \"%(mod)\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"duplicate_type_definition %(id)"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Type \"%(id)\" has already been defined."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"unexpected_token %(token)"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Symtax error before \"%(token)\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"builtin_type_arity_error %(num_expected) %(num_supplied)"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Wrong number of type parameters provided for builtin type \"%(type)\".\nExpected %(num_expected), but got %(num_supplied)."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"unknown_error %(raw_error_term)"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"%(raw_error_term)\nSorry, we do not have a proper message for this error yet.\nPlease consider filing an issue at https://www.github.com/alpaca-lang/alpaca."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"module_rename %(old) %(new)."/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Redefinition of module name from \"%(old)\" to \"%(new)\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"invalid_fun_parameter"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Invalid pattern for function argument."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"invalid_endianess %(endianess)"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Invalid endianess \"%(endianess)\".Did you mean \"big\", \"little\", or \"native\"?"/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"incomplete_expression"/utf8>> ->
			case Locale of
				<<"en_US">> -> #porec2{msgstr = <<"Unexpected end of expression."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		_ -> undefined
	end.

to_integer(true) -> to_integer(1);
to_integer(false) -> to_integer(0);
to_integer(N) when is_integer(N) -> N.

to_boolean(true) -> true;
to_boolean(false) -> false;
to_boolean(N) when N > 0 -> to_boolean(true);
to_boolean(N) when N == 0 -> to_boolean(false).
	