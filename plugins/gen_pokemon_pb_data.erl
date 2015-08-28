-module(gen_pokemon_pb_data).

-export([pre_compile/2]).

pre_compile(_, _) ->

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

    write_file(Filename, Content),
    ok.

write_file(FileName, Content) ->
    BinContent = iolist_to_binary(Content),
    case file:read_file(FileName) of
        {ok, BinContent} ->
            unchanged;
        _ ->
            file:write_file(FileName, Content)
    end.