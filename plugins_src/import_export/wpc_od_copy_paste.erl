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

-export([init/0,menu/2,command/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/src/wings.hrl").

-define(OD_FILE_NAME, "ODVertexData.txt").

-record(ost,
    {v=[],					% Vertices.
     vtp=gb_trees:empty(),	% Texture coordinates for discontinuous UVs.
     vtv=orddict:new(),	    % Texture coordinates for continuous UVs.
     vn=[],					% Vertex normals.
     f=[],					% Faces.
     tx=gb_trees:empty(),   % Texture coordinates table
     mat=gb_sets:new(),		% Materials.

     num_v=0,				% Number of vertices.
     num_vtp=0,				% Number of texture coordinates for discontinuous UVs.
     num_vtv=0,				% Number of texture coordinates for continuous UVs.
     num_vn=0				% Number of vertex normals.
    }).


init() ->
    true.

menu({edit}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

%% process only selected objects when in Body selection mode
command({edit,od_copy_to_temp}, #st{selmode=body,sel=[_]}=St) ->
    Exporter = fun(Fun) -> wpa:export_selected(none, Fun, St) end,
    do_export(Exporter, St);
command({edit,od_copy_to_temp}, St) ->
    St;
command({edit,od_past_from_temp}, St) ->
    do_import(St);
command(_, _) ->
    next.

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

%%%
%%%  Importer (Past from the temporary file)
%%%

do_import(St0) ->
    FileName = get_temp_dir() ++ ?OD_FILE_NAME,
    case ?SLOW(import_fun(FileName, St0)) of
        #st{}=St -> St;
        {error,Reason} -> io:format("OD Past failed: ~p\n",[Reason])
    end.

import_fun(Filename, St0) ->
    wings_pb:start("Reading temp file..."),
    case import_file(Filename) of
        {ok,#e3d_object{obj=Mesh,mat=_Mat}=_E3DObject} ->
            We = wings_we:build(mesh,Mesh),
            wings_pb:update(0.95,"Building object..."),
            St = wings_obj:new("Imported", We, St0),
            wings_pb:done(),
            St;
        {error,Reason} ->
            wings_pb:done(),
            wings_u:error_msg(Reason)
    end.

import_file(Name) ->
    case read_open(Name) of
        {ok,Fd} ->
            try import_1(Fd) of
                #e3d_object{}=E3dObject ->
                    {ok,E3dObject}
            catch
                throw:Error -> Error
            after
                close(Fd)
            end;
        {error,Reason} ->
            {error,file:format_error(Reason)}
    end.

import_1(Fd) ->
    Ost = import_2(Fd,get_token(read_line(Fd)),#ost{}),
    #ost{v=Vtab,tx=TxTab,vtp=Vtp,vtv=Vtv,f=Ftab0,vn=VnTab,mat=Mat} = Ost,
    Ftab = make_ftab(Ftab0,Vtp,Vtv,VnTab),
    Template = #e3d_mesh{type=polygon,vs=Vtab,fs=Ftab,tx=TxTab,ns=VnTab},
    wings_pb:update(0.85,"parsing object data..."),
    Obj = make_object(Template),
    Obj#e3d_object{mat=Mat}.

make_object(Template) ->
    Mesh = e3d_mesh:clean_faces(Template),
    #e3d_object{obj=Mesh}.

import_2(_, eof, #ost{v=Vtab0,tx=TxTab0,f=Ftab0,vn=VnTab0}=Acc) ->
    wings_pb:update(0.75,"Processing data..."),
    %% removing temporary indexes leaving only the UV list
    TxTab = [UV || {_,UV} <- orddict:from_list(gb_trees:values(TxTab0))],
    Vtab = lists:reverse(Vtab0),
    Ftab = lists:reverse(Ftab0),
    VnTab = lists:reverse(VnTab0),
    Acc#ost{v=Vtab,tx=TxTab,f=Ftab,vn=VnTab};
import_2(Fd, {"VERTICES",_Count}, Acc0) ->
    wings_pb:update(0.05,"Reading vertices..."),
    case import_3(Fd, vertex, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd, {"POLYGONS",_Count0}, Acc0) ->
    wings_pb:update(0.20,"Reading polygons..."),
    case import_3(Fd, polygon, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd, {"WEIGHT",_Id}, Acc) ->
    wings_pb:update(0.35,"Reading weights..."),
    import_2(Fd,read_line(Fd),Acc);
import_2(Fd, {"UV",_Id,_Count0}, Acc0) ->
    wings_pb:update(0.40,"Reading UVs..."),
    case import_3(Fd, uv, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd, {"MORPH",_Id}, Acc) ->
    wings_pb:update(0.55,"Reading morphs..."),
    import_2(Fd,read_line(Fd),Acc);
import_2(Fd, {"VERTEXNORMALS",_Id}, Acc0) ->
    wings_pb:update(0.60,"Reading vertex normals..."),
    case import_3(Fd, vertex_normal, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd, _, Acc) ->
    import_2(Fd,get_token(read_line(Fd)),Acc).

import_3(_, _, eof, Acc) -> Acc;
import_3(Fd, Type, "", Acc) ->
    import_3(Fd, Type, read_line(Fd), Acc);
import_3(Fd, vertex=Type, Line0, #ost{v=Vtab,num_v=NumV}=Acc) ->
    case string:tokens(Line0," ") of
        [X,Y,Z] ->
            V = {list_to_float(X), list_to_float(Y), list_to_float(Z)},
            import_3(Fd, Type, read_line(Fd), Acc#ost{v=[V|Vtab],num_v=NumV+1});
        _ -> {Line0,Acc}
    end;
import_3(Fd, polygon=Type, Line0, #ost{f=Ftab,mat=FMat}=Acc) ->
    case string:tokens(Line0,";;") of
        [Fs0,Mat0,_PolyTipe] ->
            case string:lowercase(Mat0) of
                "default" -> Mat = [];
                _ -> Mat = [Mat0]
            end,
            Fs = string:tokens(Fs0,","),
            Vlist = [list_to_integer(V) || V <- Fs],
            import_3(Fd, Type, read_line(Fd), Acc#ost{f=[{Mat,Vlist}|Ftab],mat=gb_sets:add(Mat,FMat)});
        _ -> {Line0,Acc}
    end;
import_3(Fd, uv=Type, Line0, #ost{tx=TxTab0,vtv=Vtv0,vtp=Vtp0,num_vtp=NumVtp0,num_vtv=NumVtv0}=Acc) ->
    case string:tokens(Line0,":") of
        [UV0,Kind|_] = Tokens when Kind=:="PLY"; Kind=:="PNT" ->
            [VId0|_] = lists:reverse(Tokens),
            UV = string:tokens(UV0," "),
            VId = list_to_integer(VId0),
            %% getting/updating the UV table
            case gb_trees:lookup(UV,TxTab0) of
                {value,{Id,_UV}} ->
                    UVId = Id,
                    TxTab = TxTab0;
                none -> 
                    UVp = list_to_tuple([list_to_float(V) || V <- UV]),
                    UVId = gb_trees:size(TxTab0),
                    TxTab = gb_trees:enter(UV,{UVId,UVp},TxTab0)
            end,

            case Tokens of
                [_,"PLY",PId0,"PNT",VId0] ->
                    %% Lookking for the vertex table in the polygon table
                    PId = list_to_integer(PId0),
                    case gb_trees:lookup(PId,Vtp0) of
                        none -> Vtv = [];
                        {value, Value} -> Vtv = Value
                    end,

                    %% Lookking for the polygon's vertex in the vertex table
                    case orddict:find(VId,Vtv) of
                        {ok, _UVId} -> NumVtp = NumVtp0;
                        error -> NumVtp = NumVtp0+1
                    end,
                    %% Updating the polygon's vertex table with the UV info
                    Vtp = gb_trees:enter(PId,orddict:store(VId,UVId,Vtv),Vtp0),

                    import_3(Fd, Type, read_line(Fd), Acc#ost{tx=TxTab,vtp=Vtp,num_vtp=NumVtp});
                [_,"PNT",VId0] ->
                    %% Lookking for the polygon's vertex in the vertex table
                    case orddict:find(VId,Vtv0) of
                        {ok, _UVId} -> NumVtv = NumVtv0;
                        error -> NumVtv = NumVtv0+1
                    end,
                    %% Updating the vertex table with the UV info
                    Vtv = orddict:store(VId,UVId,Vtv0),
                    import_3(Fd, Type, read_line(Fd), Acc#ost{tx=TxTab,vtv=Vtv,num_vtv=NumVtv})
            end;
        _ -> {Line0,Acc}
    end;
import_3(Fd, vertex_normal=Type, Line0, #ost{v=Vtab,num_vn=NumVn}=Acc) ->
    case string:tokens(Line0," ") of
        [X,Y,Z] ->
            V = {list_to_float(X), list_to_float(Y), list_to_float(Z)},
            import_3(Fd, Type, read_line(Fd), Acc#ost{v=[V|Vtab],num_vn=NumVn+1});
        _ -> {Line0,Acc}
    end.

%%%
%%% Utils
%%%

make_ftab(Ftab, Vtp, Vtv, VnTab) ->
    make_ftab(0,Ftab,Vtp,Vtv,array:from_list(VnTab,none),[]).

make_ftab(_,[], _, _, _,  Acc) -> Acc;
make_ftab(PId, [{Mat,Vs}|Fs], Vtp, Vtv, VnTab, FsAcc) ->
    case gb_trees:lookup(PId,Vtp) of
        {value,Vtv0} -> 
            Txs0 = make_ftab_0(Vs,Vtv0,Vtv,[]);
        none -> 
            Txs0 = make_ftab_0(Vs,Vtv,[],[])
    end,
    Ns = case [array:get(V,VnTab) || V <- Vs] of
             [none|_] -> [];
             Ns0 -> Ns0
         end,
    Txs = cleanup_uvs(Txs0),
    io:format(" -> Txs: ~p\n",[Txs]),
    make_ftab(PId+1,Fs,Vtp,Vtv,VnTab,FsAcc++[#e3d_face{mat=Mat,vs=Vs,tx=Txs,ns=Ns}]).

make_ftab_0([], _, _, Acc) -> Acc;
make_ftab_0([VId|Vs], Vtv0, Vtv1, Acc) ->
    case orddict:find(VId,Vtv0) of
        {ok, UVId} -> make_ftab_0(Vs,Vtv0,Vtv1,Acc++[UVId]);
        error -> 
            case orddict:find(VId,Vtv1) of
                {ok, UVId} -> make_ftab_0(Vs,Vtv0,Vtv1,Acc++[UVId]);
                error -> make_ftab_0(Vs,Vtv0,Vtv1,Acc++[none])
            end
    end.

cleanup_uvs(TxIdx) ->
    case [Idx || Idx <- TxIdx, Idx=/=none] of
        [] -> [];
        _ -> TxIdx
    end.

get_temp_dir() ->
    {Os,_} = os:type(),
    case get_temp_dir(Os) of
        Dir when Os == win32 ->  Dir ++ "\\";
        Dir ->  Dir ++"/"
    end.

%% Executing the same steps of Python to get the system temporary directory
get_temp_dir(Os) ->
    Vars = ["TMPDIR","TEMP","TMP"],
    get_temp_dir_from_var(Os,Vars).

get_temp_dir_from_var(Os, []) ->
    get_temp_dir_from_path(Os);
get_temp_dir_from_var(Os, [Var|Vars]) ->
    case os:getenv(Var,undefined) of
        undefined -> get_temp_dir_from_var(Os, Vars);
        Dir -> Dir
    end.

get_temp_dir_from_path(win32) ->
    get_temp_dir_from_path_0(["C:\\TEMP","C:\\TMP","\\TEMP","\\TMP"]);
get_temp_dir_from_path(_) ->
    get_temp_dir_from_path_0(["/tmp", "/var/tmp", "/usr/tmp"]).
get_temp_dir_from_path_0([]) -> 
    case file:get_cwd() of
        {ok,CurDir} -> CurDir;
        _Error -> ""
    end;
get_temp_dir_from_path_0([Dir|Dirs]) ->
    case filelib:is_dir(Dir) of
        false -> get_temp_dir_from_path_0(Dirs);
        true -> Dir
    end.

read_open(Name) ->
    case file:open(Name, [read,read_ahead,raw]) of
        {ok,_Fd}=Ok -> Ok;
        {error,_}=Error -> Error
    end.

close(Fd) ->
    file:close(Fd).

read_line(Fd) ->
    case file:read_line(Fd) of
        {ok, Line} -> string:strip(Line,right,$\n);
        EOF -> EOF
    end.

get_token(eof) -> eof;
get_token(Line) ->
    case string:tokens(Line,":") of
        ["VERTICES"=T,C] -> {T,list_to_integer(C)};
        ["POLYGONS"=T,C] -> {T,list_to_integer(C)};
        ["WEIGHT"=T,N] -> {T,N};
        ["MORPH"=T,N] -> {T,N};
        ["UV"=T,N,C] -> {T,N,list_to_integer(C)};
        ["VERTEXNORMALS"=T,C] -> {T,list_to_integer(C)};
        _ -> Line
    end.

%%collect_vs([V|Vs], Ost) ->
%%    [collect_vtxref(V, Ost)|collect_vs(Vs, Ost)];
%%collect_vs([], _Ost) -> [].
%%
%%collect_vtxref(S, Ost) ->
%%    case collect_vtxref_1(S, []) of
%%        [V] -> collect_vtxref_2(V, none, none, Ost);
%%        [V,Vt] -> collect_vtxref_2(V, Vt, none, Ost);
%%        [V,Vt,Vn|_] -> collect_vtxref_2(V, Vt, Vn, Ost)
%%    end.
%%
%%collect_vtxref_1([], Acc) -> lists:reverse(Acc);
%%collect_vtxref_1(S0, Acc) ->
%%    {Ref,S} = collect_one_vtxref(S0),
%%    collect_vtxref_1(S, [Ref|Acc]).
%%
%%collect_vtxref_2(V0, Vt0, Vn0, #ost{num_v=NumV,num_vt=NumVt,num_vn=NumVn}) ->
%%    V = resolve_vtxref(V0, NumV),
%%    Vt = resolve_vtxref(Vt0, NumVt),
%%    Vn = resolve_vtxref(Vn0, NumVn),
%%    {V,Vt,Vn}.
%%
%%resolve_vtxref(none, _) -> none;
%%resolve_vtxref(V, _) when V > 0 -> V-1;
%%resolve_vtxref(V0, N) when V0 < 0 ->
%%    case N+V0 of
%%        V when V >= 0 -> V
%%    end.
%%
%%collect_one_vtxref(S) ->
%%    collect_one_vtxref(S, []).
%%
%%collect_one_vtxref([$/|S], Acc) ->
%%    collect_one_vtxref_done(S, Acc);
%%collect_one_vtxref([H|T], Acc) ->
%%    collect_one_vtxref(T, [H|Acc]);
%%collect_one_vtxref([], Acc) ->
%%    collect_one_vtxref_done([], Acc).
%%
%%collect_one_vtxref_done(S, []) -> {none,S};
%%collect_one_vtxref_done(S, V0) -> {list_to_integer(lists:reverse(V0)),S}.
