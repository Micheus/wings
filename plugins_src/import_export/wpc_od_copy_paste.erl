%%
%%  wpc_od_copy_paste.erl --
%%
%%     Adding OD_CopyPasteExternal support.
%%     It's an easy copy and paste of geometry and common attributes across 3D Applications
%%     https://github.com/heimlich1024/OD_CopyPasteExternal
%%
%%  Copyright (c) 2021 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_od_copy_paste).
-author("micheus").

%% API
-export([init/0,menu/2,command/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/src/wings.hrl").

init() ->
    true.

menu({edit}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({edit,od_copy_to_temp}, #st{selmode=body,sel=[_]}=St) ->
    Exporter = fun(Fun) -> wpa:export_selected(none, Fun, St) end,
    do_export(Exporter, St);
command({edit,od_copy_to_temp}, St) ->
    St;
command({edit,od_past_from_temp}, St) ->
    do_import(St);
command(_, _) ->
    next.

-define(OD_FILE_NAME, "ODVertexData.txt").

menu_entry(Menu) ->
    [{"Copy (to external)", od_copy_to_temp,
      "Copy to a temporary file to be shared via OD_CopyPastExternal"},
     {"Past (from external)", od_past_from_temp,
      "Past from a temporary file shared by other 3D add via OD_CopyPastExternal"},
     separator | Menu].

%%%
%%%  Exporter (Copy to the temporary file)
%%%

do_export(Exporter, _St) ->
    io:format("do_export/1\n",[]),
    Exporter(export_fun(?OD_FILE_NAME)).

export_fun(Attr) ->
    io:format("export_fun/1\n",[]),
    fun(Filename, Contents) ->
        export(Filename, Contents, Attr)
    end.

export(Filename, Contents0, Attr) ->
    io:format("Exporting...\n",[]),
    io:format("Filename: ~p\nAttr: ~p\nContemt: ~p\n\n",[Filename,Attr,Contents0]),
    ok.


do_import(St0) ->
    io:format("Importing...\n",[]),
    case ?SLOW(import_fun(?OD_FILE_NAME, St0)) of
        #st{}=St -> St;
        {error,Reason} -> ?dbg("OD Past failed: ") ++ Reason
    end.

import_fun(Filename, St0) ->
    io:format("import_fun\n  Filename: ~p\n",[Filename]),
    wings_pb:start(?__(1,"reading file")),
    wings_pb:update(1.0),
    case wings_pb:done(import_file(Filename)) of
        {ok,#e3d_file{}=E3DFile} ->
            wings_import:import(E3DFile, St0);
        {error,Reason} ->
            wings_u:error_msg(Reason)
    end.

import_file(Name) ->
%%    set_cwd(dirname(Name)),
%%    wings_wm:send(This, {new_state,St});
    case read_open(Name) of
        {ok,Fd} ->
            Dir = filename:dirname(Name),
            try import_1(Fd) of
                #e3d_file{}=E3dFile ->
                    {ok,E3dFile#e3d_file{dir=Dir}}
            catch
                throw:Error -> Error
            after
                close(Fd)
            end;
        {error,Reason} ->
            {error,file:format_error(Reason)}
    end.

%%%
%%% Utils
%%%

read_open(Name) ->
    case file:open(Name, [read,read_ahead,raw]) of
        {ok,Fd} ->

        {error,_}=Error -> Error
    end.

close({Fd,_}) ->
    file:close(Fd).

