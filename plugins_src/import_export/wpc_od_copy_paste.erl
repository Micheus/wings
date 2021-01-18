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
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/src/wings.hrl").

-define(OD_FILE_NAME, "ODVertexData.txt").
-define(ROUND_CONST,1000000000000000.0).

-record(ost,
    {v=[],					% Vertices.
     f=[],					% Faces.
     vtp=gb_trees:empty(),	% Texture coordinates for discontinuous UVs.
     vtv=orddict:new(),	    % Texture coordinates for continuous UVs.
     vn=[],					% Vertex normals.
     vc=[],                 % Vertex colors for each face's vertex [{{F,V},CIdx}|_].
     tx=gb_trees:empty(),   % Texture coordinates table.
     mat=gb_sets:new(),     % Materials.
     col=gb_trees:empty()   % Colors table ( Importing: {Col,{Idx,Col}} / [{Idx,Col}] )
    }).


init() ->
    true.

menu({edit}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({edit,od_copy_to_temp}, St) ->
    do_export(St);
command({edit,od_past_from_temp}, St) ->
    do_import(St);
command(_, _) ->
    next.

menu_entry(Menu) ->
    [{?__(1,"Copy (to external)"), od_copy_to_temp,
      ?__(2,"Copy the selected object to a temporary file to be shared via OD_CopyPastExternal")},
     {?__(3,"Past (from external)"), od_past_from_temp,
      ?__(4,"Past an object from a temporary file shared by other 3D via OD_CopyPastExternal")},
     separator | Menu].

%%%
%%%  Exporter (Copy to the temporary file)
%%%

%% process only selected objects when in Body selection mode
do_export(#st{selmode=body,sel=[_]}=St) ->
    FileName = get_temp_dir() ++ ?OD_FILE_NAME,
    Attr = [{export_scale, 1.0},
            {swap_y_z, false},
            {include_uvs, true},
            {include_colors, true},
            {include_normals, true},
            {group_per_material, true}],
    wings_export:export(export_fun(Attr), FileName, Attr, St);
do_export(St) ->
    wings_u:message(message(must_select)),
    St.

export_fun(Attr) ->
    fun(Filename, Contents) ->
        export(Filename, Contents, Attr)
    end.

export(Filename, #e3d_file{objs=Objs}, _Attr) ->
    case export_1(Objs,[]) of
        [#ost{}|_]=Osts ->
            case write_open(Filename) of
                {ok,Fd} ->
                    try export_3(Fd,Osts) of
                        ok -> ok
                    catch
                        throw:Error -> Error
                    after
                        close(Fd)
                    end;
                {error,Reason} ->
                    {error,file:format_error(Reason)}
            end;
        _ ->
            error
    end.

export_1([],Acc) -> Acc;
export_1([#e3d_object{obj=Obj}|Objs], Acc) ->
    Ost0 = export_2(vertex,Obj,#ost{}),
    Ost1 = export_2(polygon,Obj,Ost0),
    export_1(Objs,[Ost1|Acc]).

export_2(vertex, #e3d_mesh{vs=Vs}, Acc) ->
    Acc#ost{v=Vs};
export_2(polygon, #e3d_mesh{fs=Fs1,tx=TxTab0,ns=NTab,vc=VcTab}, Acc) ->
    TxTab = [{round(X*?ROUND_CONST)/?ROUND_CONST,round(Y*?ROUND_CONST)/?ROUND_CONST} || {X,Y} <- TxTab0],
    Zip =
        fun(Vs,Other) when length(Vs)=:=length(Other) ->
                lists:zip(Vs,Other);
            (_,_) -> []
        end,
    Fs0 = [{Vs,MatName,Zip(Vs,Txs),Zip(Vs,Vcs),Zip(Vs,Ns)} || #e3d_face{vs=Vs,mat=MatName,tx=Txs,vc=Vcs,ns=Ns} <- Fs1],
    {_,Fs,Tx,Ns,Vc} =
        lists:foldl(fun({Vs,MatName,VsTx,VsVc,VsNs}, {F, FAcc,TxAcc,NsAcc,VcAcc})->
                        {F+1,
                         [{Vs,MatName}|FAcc],
                         prepare(tx,{F,VsTx},TxTab,TxAcc),
                         prepare(ns,VsNs,NTab,NsAcc),
                         prepare(vc,{F,VsVc},VcTab,VcAcc)}
                    end, {0,[],[],[],[]}, Fs0),
    Acc#ost{f=lists:reverse(Fs),tx=lists:reverse(Tx),vn=lists:reverse(Ns),vc=lists:reverse(Vc)}.

prepare(_, [], _, Acc) -> Acc;
prepare(tx, {F,VsTx}, TxTab1, Acc) ->
    {TxTab0,_} = lists:mapfoldl(fun(A,Idx)-> {{Idx,A},Idx+1} end, 0, TxTab1),
    TxTab = gb_trees:from_orddict(TxTab0),
    lists:foldl(fun({V,TxIdx}, Acc0)->
                    Tx =
                        case gb_trees:lookup(TxIdx,TxTab) of
                            none -> {0.0,0.0};
                            {value,Value} -> Value
                        end,
                    [{F,V,Tx}|Acc0]
                end,Acc,VsTx);
prepare(ns, VsNs, NsTab1, Acc) ->
    {NsTab0,_} = lists:mapfoldl(fun(A,Idx)-> {{Idx,A},Idx+1} end, 0, NsTab1),
    NsTab = gb_trees:from_orddict(NsTab0),
    lists:foldl(fun({V,NsIdx}, Acc0)->
                    case gb_trees:lookup(NsIdx,NsTab) of
                        none -> Acc0;
                        {value,Value} -> [{V,Value}|Acc0]
                    end
                end,Acc,VsNs);
prepare(vc, {F,VsVc}, VcTab1, Acc) ->
    {VcTab0,_} = lists:mapfoldl(fun(A,Idx)-> {{Idx,A},Idx+1} end, 0, VcTab1),
    VcTab = gb_trees:from_orddict(VcTab0),
    lists:foldl(fun({V,CIdx}, Acc0)->
                    case gb_trees:lookup(CIdx,VcTab) of
                        none -> Acc0;
                        {value,Value} -> [{F,V,Value}|Acc0]
                    end
                end,Acc,VsVc).

export_3(_, []) -> ok;
export_3(Fd, [#ost{v=Vs,f=Fs,tx=UVs,vn=Ns,vc=Vcs}|Osts]) ->
    export_3(vs,Fd,Vs),
    export_3(polygon,Fd,Fs),
    export_3(uv,Fd,UVs),
    export_3(vertex_normal,Fd,Ns),
    export_3(vertex_color,Fd,Vcs),
    export_3(Fd,Osts).

export_3(_, _, []) -> ok;
export_3(vs, Fd, Vs) ->
    write_line(Fd, io_lib:format("VERTICES:~w", [length(Vs)])),
    [write_line(Fd, io_lib:format("~f ~f ~f",[X,Y,Z])) || {X,Y,Z} <- Vs];
export_3(polygon, Fd, Fs) ->
    write_line(Fd, io_lib:format("POLYGONS:~w", [length(Fs)])),
    Write =
        fun({Vs,[MatName]}) ->
            VsStr = lists:flatten([io_lib:format("~w,",[V]) || V <- Vs]),
            write_line(Fd,io_lib:format("~s;;~ts;;FACE",[string:left(VsStr,string:len(VsStr)-1),atom_to_list(MatName)]))
        end,
    [Write(F) || F <- Fs];
export_3(uv, Fd, UVs) ->
    write_line(Fd, io_lib:format("UV:UVMap:~w", [length(UVs)])),
    [write_line(Fd, io_lib:format("~f ~f:PLY:~w:PNT:~w",[X,Y,F,V])) || {F,V,{X,Y}} <- UVs];
export_3(vertex_normal, Fd, Ns) ->
    write_line(Fd, io_lib:format("VERTEXNORMALS:~w", [length(Ns)])),
    [write_line(Fd, io_lib:format("~f ~f ~f",[X,Y,Z])) || {X,Y,Z} <- Ns];
export_3(vertex_color, Fd, Vcs) ->
    write_line(Fd, io_lib:format("VERTEXCOLORS:~w", [length(Vcs)])),
    Write =
        fun({F,V,C}) ->
            if tuple_size(C) > 3 -> A = element(4,C);
            true -> A = 1.0
            end,
            write_line(Fd, io_lib:format("~f ~f ~f ~f:PLY:~w:PNT:~w",[element(1,C),element(2,C),element(3,C),A,F,V]))
        end,
    [Write(Vc) || Vc <- Vcs].

write_line(Fd, Str) ->
    io:format(Fd,"~ts",[Str]),
    io:nl(Fd).

%%%
%%%  Importer (Past from the temporary file)
%%%

do_import(St) ->
    FileName = get_temp_dir() ++ ?OD_FILE_NAME,
    case filelib:is_file(FileName) of
        true ->
            ?SLOW(import_fun(FileName, St));
        false ->
            wings_u:message(message(not_shared))
    end.

import_fun(Filename, #st{mat=StMat0}=St0) ->
    wings_pb:start("Reading temp file..."),
    case import_file(Filename) of
        {ok,#e3d_object{obj=Mesh,mat=Mat0}=_E3DObject} ->
            We = wings_we:build(mesh,Mesh),
            StMat = process_materials(Mat0,StMat0),
            {St1,_} = wings_material:add_materials(StMat, St0),
            wings_pb:update(0.95,"Building object..."),
            St = wings_obj:new("Imported", We, St1),
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
    #ost{v=Vtab0,tx=TxTab,vtp=Vtp,vtv=Vtv,f=Ftab0,vn=VnTab0,mat=Mat,vc=FVcTab0,col=VcTab0} = Ost,

    Vtab = lists:reverse(Vtab0),
    VnTab = lists:reverse(VnTab0),
    FVcTab = lists:reverse(FVcTab0),
    VcTab = [C || {_,C} <- orddict:from_list(gb_trees:values(VcTab0))],
    Ftab = make_ftab(Ftab0,Vtp,Vtv,VnTab,FVcTab),
    Template = #e3d_mesh{type=polygon,vs=Vtab,fs=Ftab,tx=TxTab,ns=VnTab,vc=VcTab},
    wings_pb:update(0.85,"parsing object data..."),
    Mesh = e3d_mesh:clean_faces(Template),
    #e3d_object{obj=Mesh,mat=Mat}.

import_2(_, eof, Acc) ->
    finish_import(Acc);
import_2(Fd, {"VERTICES",_Count}, Acc0) ->
    wings_pb:update(0.05,"Reading vertices..."),
    case import_3(Fd, vertex, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd, {"POLYGONS",_Count0}, Acc0) ->
    wings_pb:update(0.15,"Reading polygons..."),
    case import_3(Fd, polygon, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd, {"WEIGHT",_Id}, Acc) ->
    wings_pb:update(0.25,"Reading weights..."),
    import_2(Fd,read_line(Fd),Acc);
import_2(Fd, {"UV",_Id,_Count0}, Acc0) ->
    wings_pb:update(0.35,"Reading UVs..."),
    case import_3(Fd, uv, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd, {"MORPH",_Id}, Acc) ->
    wings_pb:update(0.45,"Reading morphs..."),
    import_2(Fd,read_line(Fd),Acc);
import_2(Fd, {"VERTEXNORMALS",_Id}, Acc0) ->
    wings_pb:update(0.55,"Reading vertex normals..."),
    case import_3(Fd, vertex_normal, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd, {"VERTEXCOLORS",_Id}, Acc0) ->
    wings_pb:update(0.65,"Reading vertex colors..."),
    case import_3(Fd, vertex_color, read_line(Fd), Acc0) of
        {Line,Acc} -> import_2(Fd,get_token(Line),Acc);
        Acc -> Acc
    end;
import_2(Fd, _, Acc) ->
    import_2(Fd,get_token(read_line(Fd)),Acc).

import_3(_, _, eof, Acc) ->
    finish_import(Acc);
import_3(Fd, Type, "", Acc) ->
    import_3(Fd, Type, read_line(Fd), Acc);
import_3(Fd, vertex=Type, Line0, #ost{v=Vtab}=Acc) ->
    case string:tokens(Line0," ") of
        [X,Y,Z] ->
            V = {list_to_float(X), list_to_float(Y), list_to_float(Z)},
            import_3(Fd, Type, read_line(Fd), Acc#ost{v=[V|Vtab]});
        _ -> {Line0,Acc}
    end;
import_3(Fd, polygon=Type, Line0, #ost{f=Ftab,mat=FMat}=Acc) ->
    case string:tokens(Line0,";;") of
        [Fs0,Mat0,_PolyTipe] ->
            case string:lowercase(Mat0) of
                "default" -> Mat = [default];
                _ -> Mat = [list_to_atom(Mat0)]
            end,
            Fs = string:tokens(Fs0,","),
            Vlist = [list_to_integer(V) || V <- Fs],
            import_3(Fd, Type, read_line(Fd), Acc#ost{f=[{Mat,Vlist}|Ftab],mat=gb_sets:add(Mat,FMat)});
        _ -> {Line0,Acc}
    end;
import_3(Fd, uv=Type, Line0, #ost{tx=TxTab0,vtv=Vtv0,vtp=Vtp0}=Acc) ->
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
                    %% Looking for the vertex table in the polygon table
                    PId = list_to_integer(PId0),
                    case gb_trees:lookup(PId,Vtp0) of
                        none -> Vtv = [];
                        {value, Value} -> Vtv = Value
                    end,
                    %% Updating the polygon's vertex table with the UV info
                    Vtp = gb_trees:enter(PId,orddict:store(VId,UVId,Vtv),Vtp0),

                    import_3(Fd, Type, read_line(Fd), Acc#ost{tx=TxTab,vtp=Vtp});
                [_,"PNT",VId0] ->
                    %% Updating the vertex table with the UV info
                    Vtv = orddict:store(VId,UVId,Vtv0),
                    import_3(Fd, Type, read_line(Fd), Acc#ost{tx=TxTab,vtv=Vtv})
            end;
        _ -> {Line0,Acc}
    end;
import_3(Fd, vertex_normal=Type, Line0, #ost{v=Vtab}=Acc) ->
    case string:tokens(Line0," ") of
        [X,Y,Z] ->
            V = {list_to_float(X), list_to_float(Y), list_to_float(Z)},
            import_3(Fd, Type, read_line(Fd), Acc#ost{vn=[V|Vtab]});
        _ -> {Line0,Acc}
    end;
import_3(Fd, vertex_color=Type, Line0, #ost{vc=Vtab,col=Ctab0}=Acc) ->
    case string:tokens(Line0,":") of
        [Color,"PLY",F,"PNT",V] ->
            Acc0 =
                case string:tokens(Color," ") of
                    [R,G,B,A] ->
                        Col = {list_to_float(R), list_to_float(G), list_to_float(B), list_to_float(A)},
                        case gb_trees:lookup(Col,Ctab0) of
                            {value,Value} ->
                                {CIdx,_} = Value,
                                Ctab = Ctab0;
                            none ->
                                CIdx = gb_trees:size(Ctab0),
                                Ctab = gb_trees:insert(Col,{CIdx,Col},Ctab0)
                        end,
                        Acc#ost{vc=[{{list_to_integer(F),list_to_integer(V)},CIdx}|Vtab],col=Ctab};
                    _ ->
                        Acc
                end,
            import_3(Fd, Type, read_line(Fd), Acc0);
        _ -> {Line0,Acc}
    end.

finish_import(#ost{tx=TxTab0,f=Ftab0}=Acc) ->
    wings_pb:update(0.75,"Processing data..."),
    %% removing temporary indexes leaving only the UV list
    TxTab = [UV || {_,UV} <- orddict:from_list(gb_trees:values(TxTab0))],
    Ftab = lists:reverse(Ftab0),
    Acc#ost{tx=TxTab,f=Ftab}.

%%%
%%% Utils
%%%

make_ftab(Ftab, Vtp, Vtv, VnTab, FVcTab) ->
    make_ftab(0,Ftab,Vtp,Vtv,array:from_list(VnTab,none),FVcTab,[]).

make_ftab(_,[], _, _, _, _, Acc) -> Acc;
make_ftab(F, [{Mat,Vs}|Fs], Vtp, Vtv, VnTab, FVcTab, FsAcc) ->
    case gb_trees:lookup(F,Vtp) of
        {value,Vtv0} -> 
            Txs0 = make_ftab_uv(Vs,Vtv0,Vtv,[]);
        none -> 
            Txs0 = make_ftab_uv(Vs,Vtv,[],[])
    end,
    Txs = cleanup(Txs0),
    Vcs = [make_ftab_vc(F,V,FVcTab) || V <- Vs],
    Ns = case [array:get(V,VnTab) || V <- Vs] of
             [none|_] -> [];
             Ns0 -> Ns0
         end,
    make_ftab(F+1,Fs,Vtp,Vtv,VnTab,FVcTab,FsAcc++[#e3d_face{mat=Mat,vs=Vs,vc=Vcs,tx=Txs,ns=Ns}]).

make_ftab_uv([], _, _, Acc) -> Acc;
make_ftab_uv([VId|Vs], Vtv0, Vtv1, Acc) ->
    case orddict:find(VId,Vtv0) of
        {ok, UVId} -> make_ftab_uv(Vs,Vtv0,Vtv1,Acc++[UVId]);
        error -> 
            case orddict:find(VId,Vtv1) of
                {ok, UVId} -> make_ftab_uv(Vs,Vtv0,Vtv1,Acc++[UVId]);
                error -> make_ftab_uv(Vs,Vtv0,Vtv1,Acc++[none])
            end
    end.

make_ftab_vc(F, V, FVcTab) ->
    case lists:keyfind({F,V},1,FVcTab) of
        {_, CId} -> CId;
        false -> none
    end.

process_materials(Mat0, StMat) ->
    lists:foldl(fun([MatName], Acc)->
                    case gb_trees:is_defined(MatName,StMat) of
                        false ->
                            TxId = bg_img_id(),
                            Mat = [{maps,[{diffuse,TxId}]},{opengl,[]}],
                            [{MatName, Mat}|Acc];
                        true -> Acc
                    end
                end, [], gb_sets:to_list(Mat0)).

cleanup([I|_]=Idx) ->
    case cleanup(Idx,I) of
        [] -> [];
        _ -> Idx
    end.

cleanup([],_) -> [];
cleanup([none|Idx],I) -> cleanup(Idx,I);
cleanup([I|Idx],I) -> cleanup(Idx,I);
cleanup(Idx,_) -> Idx.

bg_img_id() ->
    Is = wings_image:images(),
    case [ImId || {ImId,#e3d_image{name="auvBG"}} <- Is] of
        [ImId] -> ImId;
        _ -> wings_image:new("auvBG",wpc_autouv:bg_image())
    end.

get_temp_dir() ->
    {Os,_} = os:type(),
    case get_temp_dir(Os) of
        Dir when Os == win32 ->  Dir ++ "\\";
        Dir ->  Dir ++"/"
    end.

get_temp_dir(Os) ->
    Vars = ["TMPDIR","TEMP","TMP"],
    get_temp_dir_from_var(Os,Vars).

get_temp_dir_from_var(Os, []) ->
    get_temp_dir_from_path(Os);
get_temp_dir_from_var(Os, [Var|Vars]) ->
    case os:getenv(Var) of
        false -> get_temp_dir_from_var(Os, Vars);
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

write_open(Name) ->
    case file:open(Name, [write]) of
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
        ["UV"=T,N,C] -> {T,N,list_to_integer(C)};
        ["MORPH"=T,N] -> {T,N};
        ["VERTEXNORMALS"=T,C] -> {T,list_to_integer(C)};
        ["VERTEXCOLORS"=T,C] -> {T,list_to_integer(C)};
        _ -> Line
    end.

message(must_select) ->
    ?__(1,"One object must be selected in selection body mode.");
message(not_shared) ->
    ?__(2,"No object was shared.").