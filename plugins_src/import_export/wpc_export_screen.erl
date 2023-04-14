%%
%%  wpc_export_screen.erl --
%%
%%     Print screen exporting automation.
%%
%%  Copyright (c) 2023 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wpc_export_screen).
-export([init/0,menu/2,command/2]).


-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

init() ->
    true.

menu({tools}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

menu_entry([{_,screenshot,_,_}=H|T]) ->
    [H,menu_entry()|T];
menu_entry([H|T]) ->
    [H|menu_entry(T)];
menu_entry([]) ->
    [menu_entry()].

menu_entry() ->
    [{?__(1,"Batch screenshot..."), batch_screenshot,
      ?__(2,"Grab an image of the window for a list of selected files.")}].


command({tools,batch_screenshot}, _St) ->
    wings_dialog:dialog(true, ?__(1,"Batch Screenshot Options"), dialog(),
               fun(Res) ->
                   io:format("Dialog result: ~p\n",[Res]),
                   {tools,{batch_screenshot,Res}}
               end);
command({tools,{batch_screenshot,Res}}, St) ->
    do_batch_screenshot(Res,St);
command(_,_) ->
    next.


dialog() ->
    Importers = importers(),
    Exts = [{Ext,string:replace(Desc,"|",",")} || {Ext, Desc, _} <- Importers],
    Dir = wings_pref:get_value(current_directory),
    Ps = [{extensions,Exts},{multiple,true},{directory,Dir}],
    Fun = fun(Name) ->
            io:format("Name: ~p\n",[Name]),
            {tools,{batch_screenshot,Name}}
          end,
    wings_plugin:call_ui({file,open_dialog,Ps,Fun}).

%%dialog_op() ->
%%    FileTypes = [{lists:flatten([Val," (*",Key,")"]),Key} || {Key,Val} <- wpa:image_formats()],
%%    [{label_column,[
%%        {?__(2,"Image format")++" ", {menu,FileTypes,".png",[{key,filetype}]}
%%        }]
%%     }].

do_batch_screenshot(Res,St) ->
    io:format("Res: ~p\n",[Res]),
    St.

importers() ->
    Ms = ?GET(wings_plugins),
    Imps = [{M, M:menu({file,import}, [])} || M <- Ms],
    Add = fun({Desc,{Props, Fun}}, Acc) ->
        case proplists:get_value(ext, Props) of
            [_|_] = Ext ->
                [{Ext, Desc, Fun}|Acc];
            undefined ->
                case proplists:get_value(extensions, Props) of
                    undefined -> Acc;
                    Exts0 ->
                        [{Ext, Desc, Fun} || {Ext, _} <- Exts0] ++ Acc
                end
        end
          end,
    Props = fun({_, []}, Acc) -> Acc;
               ({M, [{Longname, Shortname}]}, Acc) ->
                   Add({Longname,M:command({file, {import, Shortname}}, fetch_props)},Acc);
               ({M, [{Longname, Shortname, [option]}]}, Acc) ->
                   Cmd = M:command({file, {import, {Shortname, return}}},fetch_props),
                   Add({Longname,M:command(Cmd, fetch_props)},Acc)
            end,
    lists:foldl(Props, [], Imps).

%%
%%load_project([]) ->
%%    wings_wm:psend(geom,{new, #st{}});
%%load_project([File|Files]) ->
%%    try
%%        load_project(Files),
%%        ?SLOW(wings_ff_wings:export(Auto, false, St))
%%    catch
%%        _:_  ->
%%            io:format()
%%    end
%%end.
%%
%%geom_screen_shot(#st{file=File}=St) ->
%%
%%    {W0,H0} = wings_wm:win_size(geom),
%%    Scale = wings_wm:win_scale(),
%%    case wings_wm:redraw_geom(geom) of
%%        ignore -> ok;
%%        _ ->
%%            W = round(W0*Scale), H = round(H0*Scale),
%%            gl:pixelStorei(?GL_PACK_ALIGNMENT, 1),
%%            gl:readBuffer(?GL_FRONT),
%%            Mem = wings_io:get_buffer(W*H*3, ?GL_UNSIGNED_BYTE),
%%            gl:readPixels(0, 0, W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
%%            ImageBin = wings_io:get_bin(Mem),
%%            Image = #e3d_image{image=ImageBin,width=W,height=H},
%%
%%            Dir = filename:dirname(File),
%%            Ext = filename:extension(File),
%%            FileName = filename:basename(File, Ext),
%%            ImageName = filename:join(Dir, FileName ++ Ext),
%%%%            wings_image:image_write([{image,#e3d_image{filename=ImageName}}, {filename,ImageName}]),
%%
%%            case filelib:ensure_dir(ImageName) of
%%                ok -> e3d_image:save(#e3d_image{filename=SaveFile},ImageName);
%%            end
%%    end.
%%
%%
%%case ?SLOW(wings_ff_wings:export(Auto, false, St)) of
%%
%%            Id = new_temp(Name, Image),
%%            window(Id)
%%    end.
