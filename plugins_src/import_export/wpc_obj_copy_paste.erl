%%
%%  wpc_obj_copy_paste.erl --
%%
%%     Adding temporary obj file copy and past option.
%%     It's an easy copy and paste of geometry and common attributes across 3D Applications
%%
%%  Copyright (c) 2021 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_obj_copy_paste).
-author("micheus").

-export([init/0,menu/2,command/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/src/wings.hrl").

-define(OBJ_FILE_NAME, "temp.obj").



init() ->
    true.

menu({edit}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

%% process only selected objects when in Body selection mode
command({edit,obj_copy_to_temp}, #st{selmode=body,sel=[_]}=St) ->
    do_export(St);
command({edit,obj_copy_to_temp}, St) ->
    St;
command({edit,obj_past_from_temp}, St) ->
    do_import(St);
command(_, _) ->
    next.

menu_entry(Menu) ->
    [{"Copy (to external)", obj_copy_to_temp,
      "Copy to a temporary .obj file to be shared with other 3D app"},
     {"Past (from external)", obj_past_from_temp,
      "Past from a temporary .obj file shared by other 3D app"},
     separator | Menu].

%%%
%%%  Exporter (Copy to the temporary file)
%%%

do_export(St) ->
    FileName = get_temp_dir() ++ ?OBJ_FILE_NAME,
    Attr = [{export_scale, get_pref(export_scale, 1.0)},
            {swap_y_z, get_pref(swap_y_z,false)},
            {include_uvs, true},
            {include_colors, true},
            {include_normals, true},
            {group_per_material, true}],
    wings_export:export(export_fun(Attr), FileName, Attr, St).

export_fun(Attr) ->
    fun(Filename, Contents) ->
        export(Filename, Contents, Attr)
    end.

export(Filename, Contents, Attr) ->
    case e3d_obj:export(Filename, Contents, Attr) of
        ok -> ok;
        {error,_}=Error -> Error
    end.

%%%
%%%  Importer (Past from the temporary file)
%%%

do_import(St0) ->
    FileName = get_temp_dir() ++ ?OBJ_FILE_NAME,
    case ?SLOW(import_fun(FileName, St0)) of
        #st{}=St -> St;
        {error,Reason} -> io:format("OBJ Past failed: ~p\n",[Reason])
    end.

import_fun(FileName, St2) ->
    wings_pb:start("Reading temp file..."),
    Attr = [{import_scale, get_pref(import_scale, 1.0)},
            {swap_y_z, get_pref(swap_y_z,false)}],
    case wings_pb:done(import_file(FileName, Attr)) of
        {ok,#e3d_file{objs=[#e3d_object{name=Name}=Obj|_],mat=Mat,dir=Dir}} ->
            WeNew0 = import_object(Obj),
            {#st{shapes=Shapes0}=St1,NameMap} = wings_material:add_materials(Mat, Dir, St2),
            WeNew = rename_materials(NameMap, WeNew0),

            %% Looking for a preexistent object with that name
            FF = fun(#{id:=Id,name:=Name0}, A) when Name0==Name ->
                        io:format("Id: ~p Name: ~p\n",[Id,Name0]),
                        [Id|A];
                    (#{id:=Id,name:=Name0}, A) ->
                        io:format("NOT -> Id: ~p Name: ~p\n",[Id,Name0]),
                        A
                end,
            Ids0 = wings_obj:fold(FF, [], St1),
            io:format("Name: ~p | Ids0: ~p\n",[Name,Ids0]),
            %% updating shapes in #st{}
            St0 =
                case Ids0 of
                    [] ->
                        io:format("Createing new object\n"),
                        Ids = gb_sets:from_list([St1#st.onext]),
                        wings_obj:new(Name, WeNew, St1);
                    [Id0|_] ->
                        io:format("Using old object: ~p\n",[Id0]),
                        Ids = gb_sets:from_list(Ids0),
                        #we{name=Name0,pst=Pst0} = gb_trees:get(Id0,Shapes0),
                        St1#st{shapes=gb_trees:enter(Id0,WeNew#we{id=Id0,name=Name0,pst=Pst0},Shapes0)}
                end,
            io:format("Ids: ~p\n",[Ids]),
            SF = fun(_, #we{id=Id}) -> gb_sets:is_member(Id, Ids) end,
            St = wings_sel:make(SF, body, St0),
            wings_sel_conv:mode(body, St);
        {error,Reason} ->
            wings_u:error_msg(Reason)
    end.

import_file(Filename, Attr) ->
    case e3d_obj:import(Filename) of
        {ok,E3dFile0} ->
            E3dFile = import_transform(E3dFile0, Attr),
            {ok,E3dFile};
        {error,Error} ->
            {error,Error}
    end.

import_object(#e3d_object{name=_Name,obj=Mesh0}) ->
    Mesh1 = e3d_mesh:merge_vertices(Mesh0),
    Mesh2 = e3d_mesh:clean_faces(Mesh1),
    Mesh3 = e3d_mesh:transform(Mesh2),
    Mesh  = e3d_mesh:hard_edges_from_normals(Mesh3),
    wings_import:import_mesh(material, Mesh).

%%%
%%% Utils
%%%

rename_materials([], We) -> We;
rename_materials([_|_]=NameMap0, We) ->
    NameMap = gb_trees:from_orddict(lists:sort(NameMap0)),
    rename_materials(NameMap, We);
rename_materials(NameMap, We) ->
    MatTab0 = wings_facemat:all(We),
    MatTab = lists:foldl(fun({Face,Mat0}=Pair, A) ->
        case gb_trees:lookup(Mat0, NameMap) of
            none -> [Pair|A];
            {value,Mat} -> [{Face,Mat}|A]
        end
                   end, [], MatTab0),
    wings_facemat:assign(MatTab, We).

get_temp_dir() ->
    {Os,_} = os:type(),
    case get_temp_dir(Os) of
        Dir when Os == win32 ->  Dir ++ "\\";
        Dir ->  Dir ++"/"
    end.

get_temp_dir(Os) ->
    Vars = ["TMPDIR","TEMP","TMP"],
    get_temp_dir_from_var(Os,Vars).

get_temp_dir_from_var(_, []) ->
    case file:get_cwd() of
        {ok,CurDir} -> CurDir;
        _Error -> ""
    end;
get_temp_dir_from_var(Os, [Var|Vars]) ->
    case os:getenv(Var,undefined) of
        undefined -> get_temp_dir_from_var(Os, Vars);
        Dir -> Dir
    end.

get_pref(Key, Def) ->
    wpa:pref_get(wpc_obj, Key, Def).

import_transform(Contents, Attr) ->
    Mat = wpa:import_matrix(Attr),
    e3d_file:transform_matrix(Contents, Mat).
