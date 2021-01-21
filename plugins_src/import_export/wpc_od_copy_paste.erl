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
    #ost{v=Vtab0,vtp=TxTab1p,vtv=TxTab1v,f=Ftab1,vn=VnTab0,mat=Mat,num_vtp=NVtp} = Ost,
    Vtab = lists:reverse(Vtab0),
    Ftab0 = lists:reverse(Ftab1),
    VnTab = lists:reverse(VnTab0),
    PLY = NVtp>0,
    if PLY -> TxTab1 = TxTab1p;
    true -> TxTab1 = TxTab1v
    end,
    {Ftab,TxTab} = make_ftab(Ftab0, TxTab1, VnTab, PLY),
    Template = #e3d_mesh{type=polygon,vs=Vtab,fs=Ftab,tx=TxTab,ns=VnTab},
    Obj = make_object(Template),
    wings_pb:update(0.85,"Building object..."),
    Obj#e3d_object{mat=Mat}.

make_object(Template) ->
    Mesh = e3d_mesh:clean_faces(Template),
    #e3d_object{obj=Mesh}.

import_2(_,eof,Acc) ->
    wings_pb:update(0.75,"Processing data..."),
    Acc;
import_2(Fd,{"VERTICES",_Count},Acc0) ->
    wings_pb:update(0.05,"Reading vertices..."),
    case import_3(Fd, vertex, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd,{"POLYGONS",_Count0},Acc0) ->
    wings_pb:update(0.20,"Reading polygons..."),
    case import_3(Fd, polygon, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd,{"WEIGHT",_Id},Acc) ->
    wings_pb:update(0.35,"Reading weights..."),
    import_2(Fd,read_line(Fd),Acc);
import_2(Fd,{"UV",_Id,_Count0},Acc0) ->
    wings_pb:update(0.40,"Reading UVs..."),
    case import_3(Fd, uv, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd,{"MORPH",_Id},Acc) ->
    wings_pb:update(0.55,"Reading morphs..."),
    import_2(Fd,read_line(Fd),Acc);
import_2(Fd,{"VERTEXNORMALS",_Id},Acc0) ->
    wings_pb:update(0.60,"Reading vertex normals..."),
    case import_3(Fd, vertex_normal, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd,_,Acc) ->
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
import_3(Fd, uv=Type, Line0, #ost{vtv=Vtv,vtp=Vtp,num_vtp=NumVtp0,num_vtv=NumVtv0}=Acc) ->
    case string:tokens(Line0,":") of
        [UV0,"PLY",PId0,"PNT",VId0] ->
            PId = list_to_integer(PId0),
            VId = list_to_integer(VId0),
            UV = string:tokens(UV0," "),
            [U,V] = [list_to_float(V) || V <- UV],
            VtpI =
                case gb_trees:lookup(PId,Vtp) of
                    none -> [];
                    {value, Value0} -> Value0
                end,
            case orddict:find(VId,VtpI) of
                {ok, Value} -> 
                    UVp = Value,
                    NumVtp = NumVtp0;
                error -> 
                    UVp = {U,V},
                    NumVtp = NumVtp0+1
            end,
            import_3(Fd, Type, read_line(Fd), Acc#ost{vtp=gb_trees:enter(PId,orddict:store(VId,UVp,VtpI),Vtp),
                                                      num_vtp=NumVtp});
        [UV0,"PNT",VId0] ->
            VId = list_to_integer(VId0),
            UV = string:tokens(UV0," "),
            [U,V] = [list_to_float(V) || V <- UV],
            case orddict:find(VId,Vtv) of
                {ok, Value} -> 
                    UVp = Value,
                    NumVtv = NumVtv0;
                error -> 
                    UVp = {U,V},
                    NumVtv = NumVtv0+1
            end,
            import_3(Fd, Type, read_line(Fd), Acc#ost{vtv=orddict:store(VId,UVp,Vtv),num_vtv=NumVtv+1});
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

make_ftab(Ftab, TxTab, VnTab, TxByFace) ->
    make_ftab(0,Ftab,TxTab,array:from_list(VnTab,none),TxByFace,{[],[]}).

make_ftab(F, [{Mat,Vs}|Fs], TxTab, VnTab, TxByFace, {FsAcc,TxAcc0}) ->
    case TxByFace of
        true ->
            case gb_trees:lookup(F,TxTab) of
                none -> 
                    _Txs = [], 
                    TxIdx = [],
                    TxAcc = TxAcc0;
                {value,IdVs} -> 
                    {_,TxIdx,Txs} = make_ftab_0(Vs,IdVs,{length(TxAcc0),[],[]}),
                    TxAcc = TxAcc0++Txs
                end;
        false -> 
            {_,TxIdx,Txs} = make_ftab_0(Vs,TxTab,{length(TxAcc0),[],[]}),
            TxAcc = TxAcc0++Txs
    end,
    Ns = case [array:get(V,VnTab) || V <- Vs] of
             [none|_] -> [];
             Ns0 -> Ns0
         end,
    make_ftab(F+1,Fs,TxTab,VnTab,TxByFace,{[#e3d_face{mat=Mat,vs=Vs,tx=TxIdx,ns=Ns}|FsAcc],TxAcc});
make_ftab(_,[], _, _, _, Acc) -> Acc.

make_ftab_0([], _, {Tx,TxIdx,TxAcc}=Acc) ->
    case [Idx || Idx <- TxIdx, Idx=/=none] of
        [] -> {Tx,[],TxAcc};
        _ -> Acc
    end;
make_ftab_0([V|Vs], IdVs, {Tx,TxIdx,TxAcc}) ->
    case orddict:find(V,IdVs) of
        {ok,UV} -> make_ftab_0(Vs,IdVs,{Tx+1,TxIdx++[Tx],TxAcc++[UV]});
        error -> make_ftab_0(Vs,IdVs,{Tx,TxIdx++[none],TxAcc})
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
