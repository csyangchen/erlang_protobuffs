%%%-------------------------------------------------------------------
%%% @author David Åberg <davabe@hotmail.com>
%%% @copyright (C) 2011, David Åberg
%%% @doc
%%%
%%% @end
%%% Created :  5 Feb 2011 by David Åberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(protobuffs_parser_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

package_test_() ->
    String = "package \"test.package\";",
    Expected = [{package, "test.package"}],
    parse_test(String, Expected).

import_test_() ->
    String = "import \"test.package\";",
    Expected = [{import, "test.package"}],
    parse_test(String, Expected).

message_test_() ->
    String = "message test { required string name "
	     "= 1; }",
    Expected = [{message, "test",
		 [{1, required, "string", "name", none}]}],
    parse_test(String, Expected).

message_default_test_() ->
    String = "message test { optional float value "
	     "= 1 [default=0.01]; optional string "
	     "stringvalue = 2 [default=\"\"];}",
    Expected = [{message, "test",
		 [{1, optional, "float", "value", 1.0e-2},
		  {2, optional, "string", "stringvalue", ""}]}],
    parse_test(String, Expected).

packed_test_() ->
    String = "message test { repeated float values "
	     "= 1 [packed=true]; }",
    Expected = [{message, "test",
		 [{1, repeated_packed, "float", "values", []}]}],
    parse_test(String, Expected).

enum_test_() ->
    String = "enum myenum { value0 = 0; value1 = 1;}",
    Expected = [{enum, "myenum",
		 [{'value0', 0}, {'value1', 1}]}],
    parse_test(String, Expected).

enum_negative_test_() ->
    String = "enum myenum { value0 = 0; value1 = -1; "
	     "value2 = 2147483648; value3 = -2147483647;}",
    Expected = [{enum, "myenum",
		 [{'value0', 0}, {'value1', -1}, {'value2', 2147483648},
		  {'value3', -2147483647}]}],
    parse_test(String, Expected).

service_test_() ->
    String = "service searchservice { rpc search (searchreq"
	     "uest) returns (searchresponse);}",
    Expected = [{service, "searchservice",
		 [{rpc, "search", "searchrequest", "searchresponse"}]}],
    parse_test(String, Expected).

extensions_test_() ->
    String = "message foo { extensions 100 to 199; }",
    Expected = [{message, "foo", [{extensions, 100, 199}]}],
    parse_test(String, Expected).

extend_test_() ->
    String = "extend foo { optional int32 bar = 126; }",
    Expected = [{extend, "foo",
		 [{126, optional, "int32", "bar", none}]}],
    parse_test(String, Expected).

option_test_() ->
    String = "option message_set_wire_format = true;",
    Expected = [{option, message_set_wire_format, true}],
    parse_test(String, Expected).

inner_option_test_() ->
    String = "message foo { option message_set_wire_format "
	     "= true;}",
    Expected = [{message, "foo",
		 [{option, message_set_wire_format, true}]}],
    parse_test(String, Expected).

nested_message_test_() ->
    String = "message test { required nested nested "
	     "= 1; message nested { } }",
    Expected = [{message, "test",
		 [{1, required, "nested", "nested", none},
		  {message, "nested", []}]}],
    parse_test(String, Expected).

parse_test(String, Expected) ->
    {ok, Result, 1} = protobuffs_scanner:string(String),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok, Expected}, Parsed)].
