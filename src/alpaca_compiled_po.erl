%%EPO COMPILED FILE
%% -*- coding: utf-8 -*-
-module('alpaca_compiled_po').
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(nowarn_unused_record).

-record(porec2, {msgstr, msgstr_n = {}, n_max}).
-export([get_record/2, get_idx/2]).
get_idx(N, <<"sv_SE">>) ->
	to_integer(to_integer(N)
			      =/= to_integer(1));
get_idx(N, <<Locale2:2/binary, $_, _/binary>>) ->
	get_idx(N, Locale2);
get_idx(_, _) ->
	0.

get_record(Key, Locale) ->
	case Key of		<<"Wrong number of type parameters provided for builtin type %(type).\nExpected %(num_expected), but got %(num_supplied)."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Du har angivit fel antal typparametrar till den inbyggda typen %(type).\nFörväntat antal parametrar: %(num_expected).\nAngivet antal parametrar: %(num_supplied)."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Invalid binary qualifier \"%(qualifier)\".\nValid qualifiers are \"end\", \"sign\", \"size\", \"type\" and \"unit\"."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Ogiltigt binärattribut \"%(qualifier).\"\nGiltig attribut är' \"end\", \"sign\", \"size\", \"type\" och \"unit\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Unexpected token \"%(token)\"."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Oväntat ord \"%(token)\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Invalid pattern for function argument."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Ogiltigt mönster för funktionsargument."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"No module name defined.\nYou may define it like this: \"module foo\""/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Du har inte angivit något modulnamn.\nDu kan ange det så här:\nmodule foo"/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Cannot find module \"%(mod)\"."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Kunde inte hitta modulen \"%(mod)\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Invalid top level construct."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Det här uttrycket får inte förekomma på toppnivån."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Type parameter provided for builtin type %(type), but none was expected."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Du har angivit typparametrar till den inbygga typen %(type), men denna typ tar inga typparameterar."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Redefintion of module name from \"%(old)\" to \"%(new)\"."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Försök att ange namn på en redan angiven modul.\nExisterande namn: \"%(old)\".\nNytt namn: \"%(new)\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Invalid endianess \"%(endianess)\".Did you mean \"big\", \"little\", or \"native\"?"/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Okänd byte-ordning. Menade du \"bi\", \"little\", eller \"native\"?"/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Duplicate definitition of \"%(id)\"."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Identifieraren \"%(id)\" används redan."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Invalid binary part type \"%(type)\".\nValid types are \"binary\", \"float\", \"int\", and \"utf8\"."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Ogiltig binärdelstyp \"%(type)\".\nGiltiga typer är \"binary\", \"float\", \"int\", och \"utf8\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"%(raw_error_term)\nSorry, we do not have a proper message for this error yet.\nPlease consider filing an issue at https://www.github.com/alpaca-lang/alpaca."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"%(raw_error_term)\nVi saknar felmeddande för detta fel.\nVänligen överväg att rapportera detta på https://www.github.com/alpaca-lang/alpaca."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Duplicate definitition of type \"%(id)\"."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Det finns redan en typ med namnet \"%(id)\"."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"No function %(fun) exported from module %(mod)."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Modulen %(mod) exporterar ingen funktion med namnet %(fun)."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Incomplete expression."/utf8>> ->
			case Locale of
				<<"sv_SE">> -> #porec2{msgstr = <<"Uttrycket är inte komplett."/utf8>>, msgstr_n = {}, n_max = 0};
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
	