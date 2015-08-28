%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2011, David AAberg
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2011 by David AAberg <davabe@hotmail.com>

-module(protobuffs_file).

-export([open/2, path_open/3, close/1, format/3, request/1, compile_forms/2, write_file/2, mv/2]).

open(File, Options) ->
    file:open(File, Options).

path_open(Path, File, Modes) ->
    file:path_open(Path, File, Modes).

close(FileRef) ->
    file:close(FileRef).

format(FileRef, FormatString, WriteFields) ->
    io:format(FileRef, FormatString, WriteFields).

request(InFile) ->
    io:request(InFile, {get_until, prompt, protobuffs_scanner, token, [1]}).

compile_forms(Forms, Options) ->
    compile:forms(Forms, [return] ++ Options).

write_file(FileName, Content) ->
    BinContent = iolist_to_binary(Content),
    case file:read_file(FileName) of
        {ok, BinContent} ->
            unchanged;
        _ ->
            file:write_file(FileName, Content)
    end.

mv(From, To) ->
    {ok, Content} = file:read_file(From),
    ok = file:delete(From),
    write_file(To, Content).

