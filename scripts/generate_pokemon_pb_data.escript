-module(generate_pokemon_pb_data).

-export([main/1]).

main(_Args) ->

    PokemonPb = "src/pokemon_pb.erl",

    {ok, pokemon_pb, Bin} = compile:file(PokemonPb, [debug_info, binary]),
    {ok, {_, [{abstract_code, {_, Forms}}]}} = beam_lib:chunks(Bin, [abstract_code]),

    ModeName = "pokemon_pb_data",

    Content = io_lib:format("
-module(~s).\n
-export([forms/0]).\n
forms() ->\n
~p.
", [ModeName, Forms]),

    Filename = "src/" ++ ModeName ++ ".erl",

    file:write_file(Filename, Content),

    ok.


