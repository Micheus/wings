%%
%%  wpc_plane_cut.erl --
%%
%%
%%  Copyright (c) 2016 Micheus.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_loft).
-export([init/0,menu/2,command/2]).

%%-export([update_dlist/3,draw/4,get_data/3]).
-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).

%%%-include("wings.hrl").
-include_lib("wpc_intersect.hrl").

-import(lists, [foldl/3,reverse/1]).

-define(INFINITY, 3.402823e+38).  %% 32 bits float max
-define(ZERO, 1.0e+10).           %% values less than it will be trunked to zero
-define(PTRN_UP, {0.0,1.0,0.0}).
-define(PTRN_DIRECTION, {1.0,0.0,0.0}).
-define(TAG, wpc_sweep_along).
-define(KEY(K), {?TAG,(K)}).
key(Key) -> {key,?KEY(Key)}.


init() ->
    true.

%%%
%%% Insert menu heading
%%%

menu({face},Menu)->
    reverse(parse(Menu, [], false));
menu(_,Menu) ->
    Menu.

parse([{_,sweep,_}=A|Rest], NewMenu, false) ->
    parse(Rest, [menu_heading(),A|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found);
parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [menu_heading()|NewMenu].

menu_heading() ->
    [{?__(1,"Sweep"),{sweep_along,sweep_along_menu()}}].

sweep_along_menu() ->
    fun(help,_) ->
            {?__(1,"Extrude a face along a path defined by a selection"),
             ?__(2,"Extrude MMB"),
             ?__(3,"Extrude RMB")};
       (1,_) -> {sweep_along, path};
       (2,_) -> {sweep_along,{'ASK',[axis]}};
       (3,_) -> {sweep_along,{'ASK',[axis]}};
       (_,_) -> ignore
    end.


%%%
%%% Commands
%%%

command({sweep_along,{'ASK',_}=Ask}, St) ->
    io:format("ASK\n",[]),
    sweep_along_path(Ask, St);
command({sweep_along,Mode}, St) ->
    case Mode of
        path -> wings:ask(sel_path_ask(), St, fun sweep_along_path/2);
        _ -> ok
    end;
command(_Cmd,_) ->
                                                %    io:format("Cmd: ~p\n",[_Cmd]),
    next.


%%
%%  Respond to commands
%%

sweep_along_path({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun sweep_along_path/2);
sweep_along_path({Up0,{PweId,Pinfo,Closed}}, #st{shapes=Shp1, sel=[{Id1,Sel}]}=St) ->
    %% save the original #we{} owner of the pattern region
    We1 = gb_trees:get(Id1, Shp1),
    RgnLst = wings_sel:face_regions(Sel, We1),
    [Rgn0] =
        lists:foldr(fun(RgnSet, []) ->
                            [RgnSet];
                       (RgnSet, [RgnSetOld]=Acc) ->
                            if size(RgnSet) > size(RgnSetOld) ->
                                    [RgnSetOld];
                               true -> Acc
                            end
                    end, [], RgnLst),

    %% uses the normal from the "largest" region as reference for flatten operation
    RgnN = wings_face:normal(gb_sets:largest(Rgn0), We1),
    Center0 = wings_sel:center(St),

    %% flatten the selected region creating the pattern
    {_, St0} = wings_face_cmd:command({flatten,{RgnN, Center0}}, St),

    %% prepare the pattern data to be used to create the faces to the "pipe"
    {_,_,_, PtrnScale, _} = PtrnInfo = prepare_pattern(Up0, RgnN, Center0, St0),

    %% prepare the path data using the original shapes
    {Shortest, Path} = prepare_path(Pinfo, PweId, Shp1),
    Parameters = {Path, PtrnInfo},
    Name = init_dialog(title,[]),
    Frame = init_dialog(body,[linear, lm_hard, Closed, 0, PtrnScale, Shortest/2.0, false]),

%%    ets:new(wpc_sweep_along,[named_table,public,ordered_set]),
%%    ets:insert(wpc_sweep_along,{path,[]}),
    wings_dialog:dialog(Name, {preview,Frame},
                        fun
                            ({dialog_preview, Options}) ->
                               {preview, St, extrude_along(Parameters, Options,St)};
                            (cancel) ->
%%                               ets:delete(wpc_sweep_along),
                               St;
                            (Options) ->
                                %% ets:delete(wpc_sweep_along),
                               {commit, St, extrude_along(Parameters, Options,St)}
                       end).

prepare_pattern(Up1, RgnN, Center, St) ->
    %% get the pattern data from the selected faces
    {VsShp, PtrnShape1, PtrnOutline1} = get_pattern_info(St),

    %% prepare an array of vertices and create a translation list for their index
    {VsRef, VtabShp2} = create_vs_ref(VsShp),

    PtrnShape = translate_vs_ref(VsRef, PtrnShape1),
    PtrnOutline0 = translate_vs_ref(VsRef, PtrnOutline1),
    PtrnOutline = lists:flatten(PtrnOutline0),

    %% validate and fix the Up vector - it must be perpendicular to RgnN vector
    Up0 = force_perpendicular(Up1,RgnN),
%%    io:format("Up1:~p\nUp0:~p\nRgnN:~p\n------\n\n",[Up1,Up0,RgnN]),
    %% find the center to translate pattern to origin
    M0 = e3d_mat:translate(e3d_vec:neg(Center)),
    %% align pattern normal to x axis
    AlignX = e3d_mat:rotate_s_to_t(RgnN, ?PTRN_DIRECTION),
    M1 = e3d_mat:mul(AlignX, M0),

    %% align the pattern in accord with direction vector
    VtabShp1 = translate_vs(M1, VtabShp2),

    %% normalize the pattern size
    {Min,Max} = e3d_bv:box(array:to_list(VtabShp1)),
    Rect = build_rect(Min,Max),
    {Xsz,Ysz,Zsz} = e3d_vec:sub(Max, Min),
    Size = max(Xsz, max(Ysz, Zsz)),
    Scale = 1.0 / Size,
    SizMat = e3d_mat:scale(Scale),

    %% align the initial Up vector in accord with direction vector
    Up = e3d_mat:mul_vector(AlignX, Up0),
    AlignY = e3d_mat:rotate_s_to_t(Up, ?PTRN_UP),

    M2 = e3d_mat:mul(SizMat, AlignY),
    VtabShp = translate_vs(M2, VtabShp1),

    {PtrnShape, PtrnOutline, VtabShp, Scale, Rect}.

create_vs_ref(Vs) ->
    {_, VRef, Vtab} =
        lists:foldl(fun(V, {Idx, VpRef, Acc}) ->
                            Vp = gb_trees:get(V, Vs),
                            {Idx+1, gb_trees:insert(V, Idx, VpRef), array:set(Idx, Vp, Acc)}
                    end, {0, gb_trees:empty(), array:new()}, gb_trees:keys(Vs)),
    {VRef, Vtab}.

translate_vs_ref(VRef, Fs) ->
    Fun =
        fun(Es) ->
                lists:foldl(fun ({E,Vps0,Vpe0}, Acc) ->
                                    Acc ++[{E, gb_trees:get(Vps0,VRef), gb_trees:get(Vpe0,VRef)}]
                            end, [], Es)
        end,
    [Fun(Es) || Es <- Fs].

translate_vs(Mt, Vtab) ->
    array:foldl(fun(Idx, Vp0, Acc) ->
                        Vp = e3d_mat:mul_point(Mt, Vp0),
                        array:set(Idx, Vp, Acc)
                end, array:new(), Vtab).


get_pattern_info(#st{shapes=Shp,sel=[{Id,Sel}]}) ->
    We = gb_trees:get(Id, Shp),
    %% process the outline edges to create the intermediary segments
    {_, PtrnOutline} = get_edge_data(Sel, We),
    %% process the faces which will be used to "close the pipe"
    {VsShp, PtrnShape} = get_face_data(Sel, We),
    {VsShp, PtrnShape, PtrnOutline};
get_pattern_info(#st{}) -> [].

get_face_data([], _) -> [];
get_face_data(Fs, We) ->
    gb_sets:fold(fun(F, {VpList0, Acc})->
                         {VpList1, Einfo} = get_edge_data(gb_sets:from_list([F]), We),
                         VpList =
                             lists:foldl(fun({Key,Value}, Acc0)->
                                                 gb_trees:enter(Key, Value, Acc0)
                                         end, VpList0, gb_trees:to_list(VpList1)),
                         %%                         Finfo = get_edges_seq(Einfo,[]),
                         {VpList, Acc ++Einfo}
                 end, {gb_trees:empty(),[]}, Fs).

get_edge_data([],_)-> [];
get_edge_data(Fs, #we{es=Etab, vp=Vtab}=We)->
    Edges = wings_face:outer_edges(Fs, We),
    {VpList, Einfo0} =
        lists:foldl(fun(E, {Vlst2, Acc}) ->
                            #edge{vs=Vs,ve=Ve,lf=Lf} = array:get(E, Etab),
                            Vpe = array:get(Ve, Vtab),
                            Vps = array:get(Vs, Vtab),
                            Vlst1 = gb_trees:enter(Ve,Vpe,Vlst2),
                            Vlst = gb_trees:enter(Vs,Vps,Vlst1),
                            Ei =
                                case gb_sets:is_member(Lf,Fs) of
                                    true -> {E, Vs, Ve};
                                    _ ->  {E, Ve, Vs}
                                end,
                            {Vlst, Acc++[Ei]}
                    end, {gb_trees:empty(),[]}, Edges),
    {VpList, get_edges_seq(Einfo0,[])}.

get_edges_seq([], Acc) -> Acc;
get_edges_seq([H|T], Acc) ->
    {NewList,Seg} = get_edges_seq_0(H, T, {[],[H]}),
    get_edges_seq(NewList, Acc++[Seg]).

get_edges_seq_0({_,_,Vb}, List, {_,Seq}=Acc) ->
    case lists:keytake(Vb, 2, List) of
        {value, H0, List2} ->
            get_edges_seq_0(H0, List2, {List2, Seq++[H0]});
        _ -> Acc
    end.

init_dialog(title,_) ->
    ?__(1,"Extrude along a path options");
init_dialog(body, [Mode, LmOpt, Closed, Steps, PtrnScale, Scale, Flip]) ->
    Hook_Show =
        fun(Key, Value, Store) ->
                case Key of
                    ?KEY(mode) ->
                        wings_dialog:show(?KEY(lm_option), Value =:= linear, Store)
                end
        end,

    [{vframe,[
              {vframe,[
                       {hradio, [
                                 {?__(4,"Linear"), linear},
                                 {?__(5,"Curve"), curve}
                                ], Mode, [key(mode), {hook, Hook_Show},{title,?__(3,"Path mode")}]},
                       {hradio, [
                                 {?__(7,"Hard corner"), lm_hard},
                                 {?__(8,"Soft corner"), lm_soft}
                                ], LmOpt, [key(lm_option),{title,?__(6,"Linear mode options")}]},
                       {?__(9,"Closed path"), Closed, [key(closed)]},
                       {label_column,[
                                      {?__(10,"Steps"),{slider, {text, Steps, [key(steps),{range,{0,100}}]}}}
                                     ]}
                      ],[{title,?__(2,"Curve settings")}]},
              {hframe,[
                       {label_column,[
                                      {?__(12,"Scale"),
                                       {hframe, [
                                                 {text,Scale,[key(scale), {range,{0.0001,infinity}}]},
                                                 {label, (" (" ++ io_lib:format("~.6f%",[PtrnScale]) ++ ")")}
                                                ]}}]},
                       {?__(13,"Flip horizontal"), Flip, [key(flip)]}
                      ],[{title,?__(11,"Pattern settings")}]}
             ]}].

extrude_along({Path, PtrnInfo}, Options, St) ->
    %%    {PtrnShape, VtabShp, PtrnOutline, VtabOut, PtrnScale} = PtrnInfo,
    _Mode = proplists:get_value(?KEY(mode),Options),
    _LmOpt = proplists:get_value(?KEY(lm_option),Options),
    Closed = proplists:get_value(?KEY(closed),Options),
    Scale = proplists:get_value(?KEY(scale),Options),
    %%    io:format("Mode: ~p | Option: ~p\nClosed: ~p\nScale: ~p\n",[Mode,LmOpt,Closed,Scale]),

    {_, _, _, _, _Rect} = PtrnInfo,
    NewPath = calc_linear(Path, Closed),
    io:format("NewPath: ~p\n",[NewPath]),
%%    ets:insert(wpc_sweep_along,{path,NewPath}),
    NewWe = build_pipe(Scale, PtrnInfo, NewPath),
    wings_shape:new("Sweep", NewWe, St).

build_pipe(Scale, {PtrnShape0, PtrnOutline0, VtabShp1, _, _}, [First|NewPath0]=_Path) ->
    VtabShp0 = scale(Scale, VtabShp1),
    VtabOut0 =
        lists:foldl(fun({_, Ve, _}, Acc) ->
                            Vpos = array:get(Ve, VtabShp0),
                            array:set(Ve, Vpos,Acc)
                    end, array:new(), PtrnOutline0),
    %% vertices location for the faces (region) in the tail of the stripe - the start point
    VtabTail = rotate_region(First, VtabShp0),
    %% vertices location for the faces (region) in the head of the stripe - the end point
    Last = lists:last(NewPath0),
    VtabHead = rotate_region(Last, VtabShp0),

    %% vertices location and sequence to build each ring of the stripe - the intermediary data
    {_, PtrnOutline2, VtabOut2} =
        lists:foldl(fun(Pinfo, {PinfoOld, AccOutline0, AccVtabOut0}) ->
                            %% duplicate the original vertices and relocate them
                            _VtabOut1 = project_region(PinfoOld, Pinfo, VtabOut0),
                            VtabOut1 = rotate_region(Pinfo, VtabOut0),
                            IdxOff = array:size(AccVtabOut0),
                            {_, AccVtabOut} =
                                array:sparse_foldl(fun(_, Value, {Idx, Acc}) ->
                                                           {Idx+1, array:set(Idx, Value, Acc)}
                                                   end, {IdxOff, AccVtabOut0}, VtabOut1),
                            AccOutline =
                                lists:foldl(fun({E, Ve, Vs}, Acc) ->
                                                    Acc ++[{E, Ve+IdxOff, Vs+IdxOff}]
                                            end, [], PtrnOutline0),
                            {Pinfo, AccOutline0++[AccOutline], AccVtabOut}
                    end, {First, [PtrnOutline0], VtabTail}, NewPath0),

    %%io:format("=====\nPtrnOutline2: ~p\n-----\nVtabOut2: ~p\n=====\n\n",[PtrnOutline2, VtabOut2]),

    FsTail = build_pipe_tail(PtrnShape0),
%    io:format("=====\nPtrnOutline2: ~p\n=====\n\n",[PtrnOutline2]),
    Fs0 = build_pipe_faces(PtrnOutline2),
    {Vs0, FsHead} = build_pipe_head(PtrnShape0, VtabOut2, VtabHead),
    Vs = array:to_list(Vs0),
%    io:format("=====\nFsTail: ~p\n-----\nFs0: ~p\n-----\nFsHead: ~p\n=====\n\n",[FsTail, Fs0, FsHead]),
    Fs = build_pipe_info(FsTail, Fs0, FsHead),
%    io:format("=====\nFs: ~p\n-----\nVs: ~p\n=====\n\n",[Fs, Vs]),
    wings_we:build(Fs, Vs).

build_pipe_faces([]) -> [];
build_pipe_faces([FIni|Fs0]) ->
    {_, Fs} = lists:foldl(fun(Es, {Fprev, Acc0}) ->
                        Acc2 = lists:foldl(fun({E, Ve0, Vs0}, Acc1) ->
                                            {E, Ve1, Vs1} = lists:keyfind(E, 1, Es),
                                            Acc1++[{default, [Ve0, Vs0, Vs1, Ve1]}]
                                    end, Acc0, Fprev),
                        {Es, Acc2}
                end, {FIni, []}, Fs0),
    Fs.

build_pipe_tail(PtrnShape) ->
    lists:foldl(fun(Es, Acc0) ->
                        F =
                            lists:foldl(fun({_, Ve, _}, Acc1) ->
                                                Acc1++[Ve]
                                        end, [], Es),
                        Acc0++[{default, lists:reverse(F)}]
                end, [], PtrnShape).

build_pipe_head(PtrnShape, Vs, VtabHead) ->
    {_, Vs3, Fs} =
        lists:foldl(fun(Es, {Idx0, Vs0, Acc0}) ->
                            {Idx1, Vs1, F} =
                                lists:foldl(fun({_, Ve0, _}, {Idx2, Vs2, Acc2}) ->
                                                Vp = array:get(Ve0, VtabHead),
                                                {Idx2+1, array:set(Idx2, Vp, Vs2), Acc2++[Idx2]}
                                            end, {Idx0, Vs0, []}, Es),
                            {Idx1, Vs1, Acc0++[{default, F}]}
                    end, {array:size(Vs), Vs, []}, PtrnShape),
%%    io:format("=====\nPtrnShape: ~p\n=====\nPtrnOut:~p\n=====\nVtabHead: ~p\nFs: ~p\nVs: ~p\nVs3: ~p\n\n\n",[PtrnShape, PtrnOut, VtabHead, Fs, Vs, Vs3]),
    {Vs3, Fs}.

build_pipe_info(FsTail, [], FsHead) -> FsTail ++ FsHead;
build_pipe_info(FsTail, Fs0, FsHead) -> FsTail ++ Fs0 ++ FsHead.

project_region({_Vp0,Vd0,_Vn0,_}, {Vp,Vd,Vn,Vtg}=PInfo, Vtab) ->
%%    DotProd = e3d_vec:dot(Vd0,Vtg),
    if
        Vd0 =:= Vn ->
            Plane = e3d_vec:plane(Vp, Vtg),
            A = e3d_vec:degrees(Vtg, Vd),
            CosA = math:cos(-A/180.0*math:pi()),
            MRot = build_mat_rotation(Vd, Vn),
            Centre = e3d_vec:zero(),
            array:foldl(
                fun(V, Vpos1, Acc)->
                    Vpos0 = rotate(Vpos1, Vp, Centre, MRot),
                    Adj = e3d_vec:plane_dist(Vpos0, Plane),
                    Hip = Adj/CosA,
                    Vpos = e3d_vec:add(Vpos0, e3d_vec:mul(Vd,-Hip)),
                    array:set(V, Vpos, Acc)
                end, array:new(), Vtab);
%%            IntersectData = #intersect_data{lineDir = Vd,
%%                planeNorm = Vtg, planePoint = Vp, lineDotPlane = DotProd},
%%            MRot = build_mat_rotation(Vd, Vn),
%%            Centre = e3d_vec:zero(),
%%            array:foldl(
%%                fun(V, Vpos1, Acc)->
%%                    Vpos0 = rotate(Vpos1, Vp0, Centre, MRot),
%%                    wpc_intersect_vertex:intersect_vertex(V, Acc, IntersectData#intersect_data{linePoint = Vpos0})
%%                end, array:new(), Vtab);
        true ->
            io:format("just moved - parallel\n",[]),
            rotate_region(PInfo, Vtab)
    end.

rotate_region({Vp,_Vd,Vn,Vtg}, Vtab) ->
    MRot = build_mat_rotation(Vtg, Vn),
    Centre = e3d_vec:zero(),
    array:foldl(
        fun(V, Vpos0, Acc)->
            VPos = rotate(Vpos0, Vp, Centre, MRot),
            array:set(V, VPos, Acc)
        end, array:new(), Vtab).

build_mat_rotation(Vtg, Vn) ->
    %% matrix to align pattern direction to curve tangent
    MVtg = e3d_mat:rotate_s_to_t(?PTRN_DIRECTION, Vtg),
    %% calc new normal for the previous normal (region orientation)
    Vn0 = e3d_mat:mul_point(MVtg, Vn), % translate it using the new tangent
    %%    VnDeg = calc_degree(VnP0, Vn), % find the rotation in tangent to align the orientations
    %%    MVn = e3d_mat:rotate(VnDeg, Vtg),
    MVn = e3d_mat:rotate_s_to_t(?PTRN_UP, Vn0),
    e3d_mat:mul(MVtg, MVn).

scale(Scale, {_,_,_}=Vpos) ->
    M0 = e3d_mat:scale(Scale),
    e3d_mat:mul_point(M0,Vpos);
scale(Scale, Vtab) ->
    array:foldl(fun(V, Value, Acc) ->
        array:set(V, scale(Scale, Value), Acc)
                end, Vtab, Vtab).

rotate(Vpos1, Vp, Center, MRot) ->
    Vpos0 = e3d_vec:sub(Vpos1, Center),
    Vpos = e3d_mat:mul_point(MRot, Vpos0),
    e3d_vec:add(Vp, Vpos).

calc_linear([{Vpf,Vdf,_Vnf,Enf},{Vpl,Vdl,_Vnl,Enl}], _) ->
    NewFirst = {Vpf,Vdf,Enf,Vdf},
    NewLast = {Vpl,Vdl,Enl,Vdl},
    [NewFirst,NewLast];
calc_linear([H|T], Closed) when Closed=:=false ->
    Corners = calc_linear([H,lists:last(T)], Closed),
    [First,Last] = calc_linear_closed(Corners, Closed),
    Middle = calc_linear_0(First, lists:sublist(T, length(T)-1), []),
    [First]++Middle++[Last].

calc_linear_0(_, [], Acc) -> Acc;
calc_linear_0({_VpL,VdL,_VnL,VtgL}, [{Vp,Vd,Vn,_En}|T], Acc) ->
    Vtg = calc_tg(VtgL, e3d_vec:neg(VdL), Vd),
    Info = {Vp,Vd,Vn,Vtg},
    calc_linear_0(Info, T, Acc++[Info]).

calc_linear_closed([First,Last],false) -> [First,Last];
calc_linear_closed([{VpF,VdF,VnF,TgF},{VpL,VdL,VnL,TgL}],true) ->
    VdL0 = e3d_vec:norm(e3d_vec:sub(VpL,VpF)),
    TgL0 = calc_tg(TgL, VdL0, e3d_vec:neg(VdL)),
    TgF0 = calc_tg(TgF, VdF, e3d_vec:neg(VdL0)),
    [{VpF,VdF,VnF,TgF0},{VpL,VdL0,VnL,TgL0}].

%%calc_degree(V0, V1) ->
%%    Degrees = e3d_vec:degrees(V1, V0),
%%    Side = e3d_vec:plane_side(V0, {V1, 0.0}),
%%    if Side =< 0.0 -> -Degrees;
%%       true -> Degrees
%%    end.

calc_tg(DefTg, V0, V1) ->
    case calc_tg(V0, V1) of
        V0 -> DefTg;
        Tg -> Tg
    end.

calc_tg(V0, V1) ->
    case is_colinear(V1, V0) of
        true -> V1;
        _ -> e3d_vec:norm(e3d_vec:sub(V1, V0))
    end.

is_colinear(Va0, Vb0) ->
    Va1 = e3d_vec:norm(Va0),
    Vb1 = e3d_vec:norm(Vb0),
    DotProd = e3d_vec:dot(Vb1, Va1),
    abs(DotProd) > 0.001.

%    Degrees = e3d_vec:degrees(Vb1, Va1),
%    (Degrees =< 0.001) or (Degrees >= 179.999).

prepare_path(Pinfo, Id, Shp) ->
    We = gb_trees:get(Id, Shp),
    {Short, Last, Acc} =
        lists:foldl(fun({_,En,Vs,Ve}, {Short0,_,Acc}) ->
                            Vp1 = wings_vertex:pos(Vs,We),
                            Vp2 = wings_vertex:pos(Ve,We),
                            Vn1 = wings_vertex:normal(Vs,We),
                            Vn2 = wings_vertex:normal(Ve,We),
                            Direction = e3d_vec:sub(Vp2,Vp1),
                            Vd = e3d_vec:norm(Direction),
                            Distance = e3d_vec:len(Direction),
                            Short =
                                if Distance < Short0 -> Distance;
                                   true -> Short0
                                end,
                            {Short, {Vp2,Vd,Vn2,En}, Acc++[{Vp1,Vd,Vn1,En}]}
                    end, {?INFINITY,{},[]}, Pinfo),
    {Short, Acc++[Last]}.

                                                %bbox(#we{vp=Vtab}) ->
                                                %    e3d_bv:box(array:sparse_to_list(Vtab)).


%%%
%%%  functions for orientation vector adjustment
%%%
force_perpendicular(Vec, VecRef) ->
    %%    Vec = e3d_vec:norm(Vec0),
    %%    VecRef = e3d_vec:norm(VecRef0),
    Degree = e3d_vec:degrees(Vec, VecRef),
    %%    DotProd = e3d_vec:dot(Vec, VecRef),
    if abs(Degree) =/= 90.0 ->
            io:format("\nNot perpendicular: ~p\n",[Degree]),
            Nor = e3d_vec:cross(VecRef, rot_axis_vec(main_axis(Vec))),
            Rot = e3d_mat:rotate(+90.0, Nor),
            e3d_mat:mul_vector(Rot, VecRef);
       true ->
            io:format("\nPerpendicular: Nothing to be calculated\n",[]),
            Vec
    end.

main_axis({X,Y,Z}) ->
    if (abs(X) > abs(Y)) ->
            if (abs(Z) > abs(X)) -> z;
               true -> x
            end;
       true ->
            if (abs(Z) > abs(Y)) -> z;
               true -> y
            end
    end.

rot_axis_vec(x) -> {0.0,0.0,1.0}; % rotate vector in x around of z axis
rot_axis_vec(y) -> {0.0,0.0,1.0}; % rotate vector in y around of x axis
rot_axis_vec(z) -> {1.0,0.0,0.0}. % rotate vector in z around of x axis


build_rect({Xn,Yn,Zn}=Min,{Xx,Yx,Zx}=Max) ->
    [Max,{Xx,Yx,Zn},Min,{Xn,Yn,Zx}].

%%pto_on_plane(LinePoint, LineDir, PlanePoint, PlaneNorm) ->
%%    DotProd = e3d_vec:dot(LineDir, PlaneNorm),
%%    X = e3d_vec:dot(e3d_vec:sub(PlanePoint, LinePoint),PlaneNorm)/DotProd,
%%    e3d_vec:add(LinePoint, e3d_vec:mul(LineDir, X)).


%%%
%%%  functions for orientation vector adjust
%%%
%%normal_faces(We, RgnN) ->
%%    InvRgnN = e3d_vec:neg(RgnN),
%%    Fs = wpa:faces(We),
%%    lists:foldr(fun(F, {Ffs,Tfs}=Acc) ->
%%                        Fn = wings_face:normal(F,We),
%%                        Hn = e3d_vec:dot(Fn, RgnN),
%%                        Tn = e3d_vec:dot(Fn, InvRgnN),
%%                        if Hn >= 0.001 ->
%%                                {gb_sets:add(F, Ffs), Tfs};
%%                           true ->
%%                                if Tn >= 0.001 ->
%%                                        {Ffs, gb_sets:add(F, Tfs)};
%%                                   true -> Acc
%%                                end
%%                        end
%%                end, {gb_sets:new(),gb_sets:new()},Fs).


%%%
%%% some helpful test and investigation functions
%%%

sel_path_ask() ->
    Desc = ?__(1,"Select a sequence of edges to define the path"),
    Fun =
        fun(check, St) ->
                check_sel_path(St);
           %%           (exit, {_,_,#st{selmode=vertex,sel=[Sel]}=St}) ->
           %%                io:format("Exiting vertex ...\n",[]),
           %%                case check_sel_path(St) of
           %%                    {_,[]} -> {result, Sel};
           %%                    {_,_} -> error
           %%                end;
           (exit, {_,_,#st{shapes=Shs,selmode=edge,sel=[{Id,Sel}]}=St}) ->
                case check_sel_path(St) of
                    {_,[]} ->
                        We = gb_trees:get(Id, Shs),
                        Elst = build_ord_path(Sel, We, []),
                        case Elst of
                            none -> error;
                            _ -> {result,{Id,Elst,is_closed(Elst)}}
                        end;
                    {_,_} -> error
                end;
           (exit,_) -> error
        end,
    SelAsk = selection_ask([orientation]),
    {SelAsk++[{Fun,Desc}],[],[],[vertex,edge]}.


build_ord_path(Edges, #we{es=Etab}=We, []) ->
    Vs0 = gb_sets:fold(
            fun(E, Acc) ->
                    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf} = array:get(E, Etab),
                    N = e3d_vec:average(wings_face:normal(Lf, We),
                                        wings_face:normal(Rf, We)),
                    [{E,N,Va,Vb}|Acc]
            end, [], Edges),
    Vs = ord_path(Vs0),
    validate_path(Vs0,Vs).

ord_path([_]=Vs) -> Vs;
ord_path([V|Vs]) -> ord_path_right(Vs, V, [V]).

ord_path_right([], _, Acc) -> Acc;
ord_path_right(Vs0, {_,_,_Va,Vb}, Acc) ->
    case lists:keytake(Vb,3,Vs0) of
        {value, V, Vs} -> ord_path_right(Vs, V, Acc++[V]);
        _ ->
            case lists:keytake(Vb,4,Vs0) of
                {value, {E,N,Va0,Vb0}, Vs} ->
                    ord_path_right(Vs, {E,N,Vb0,Va0}, Acc++[{E,N,Vb0,Va0}]);
                _ ->
                    [V0|_] = Acc,
                    ord_path_left(Vs0, V0, Acc)
            end
    end.

ord_path_left([], _, Acc) -> Acc;
ord_path_left(Vs0, {_,_,Va,_Vb}=V0, Acc) ->
    case lists:keytake(Va,3,Vs0) of
        {value, {E,N,Va0,Vb0}, Vs} ->
            V1 = lists:last(Acc),
            ord_path_right(Vs, V1, [{E,N,Vb0,Va0}]++Acc);
        _ ->
            case lists:keytake(Va,4,Vs0) of
                {value, V, Vs} ->
                    V1 = lists:last(Acc),
                    ord_path_right(Vs, V1, [V]++Acc);
                _ ->
                    [_|Vs1] = Vs0,
                    ord_path_left(Vs1, V0, Acc)
            end
    end.

validate_path(Vs0, [V0|Vs1]=Vs) when (length(Vs) =:= length(Vs0)) ->
    case validate_path_0(V0,Vs1) of
        true  -> Vs;
        _ -> none
    end;
validate_path(_, _) -> none.

validate_path_0(_, []) -> true;
validate_path_0({_,_,_,Vb}, [{_,_,Vb,_}=V|Vs]) ->
    validate_path_0(V,Vs);
validate_path_0(_, _) -> false.

is_closed([]) -> false;
is_closed([_]) -> false;
is_closed([H|Path]) ->
    {_,_,Vs,_} = H,
    {_,_,_,Ve} = lists:last(Path),
    Vs =:= Ve.

selection_ask(Asks) ->
    selection_ask(Asks,[]).

selection_ask([],Ask) -> lists:reverse(Ask);
selection_ask([origin|Rest],Ask) ->
    Desc = ?__(1,"Select start point for sweep_along operation"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([o_normal|Rest],Ask) ->
    Desc = ?__(2,"Select a direction vector for the start point (tipically the region normal)"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([destination|Rest],Ask) ->
    Desc = ?__(3,"Select end point for sweep_along operation"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([d_normal|Rest],Ask) ->
    Desc = ?__(4,"Select a direction vector for the end point"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([orientation|Rest],Ask) ->
    Desc = ?__(5,"Select an orientation vector to define the up side"),
    selection_ask(Rest,[{axis,Desc}|Ask]).


check_sel_path(#st{sel=[]}) ->
    {none,?__(1,"Nothing selected")};
check_sel_path(#st{selmode=Mode,sel=[{_,Sel}]}) when Mode =:= edge; Mode =:= vertex ->
    if Mode =:= vertex ->
            Vc = gb_sets:size(Sel),
            if (Vc =< 1) ->
                    {none,?__(3,"The path must be defined for at least two vertices")};
               true -> {none,[]}
            end;
       true -> {none,[]}
    end;
check_sel_path(#st{selmode=Mode,sel=[{_,_}|_]}) when Mode =:= edge; Mode =:= vertex ->
    {none,?__(2,"The selected edges or vertices cannot be from multple objects")};
check_sel_path(_) ->
    {none,?__(4,"Only edges or vertices may be selected")}.



%%
%%add_pst(InfData,Pst) ->
%%    case gb_trees:lookup(?MODULE, Pst) of
%%        none ->
%%            Data = gb_trees:empty(),
%%            NewData = gb_trees:insert(path,InfData,Data),
%%            gb_trees:insert(?MODULE,NewData,Pst);
%%        {_,Data} ->
%%            NewData = gb_trees:enter(path,InfData,Data),
%%            gb_trees:update(?MODULE,NewData,Pst)
%%    end.
%%
%%update_dlist({path,Path}, #dlo{plugins=Pdl}=D, _) ->
%%    case Path of
%%        [] ->
%%            D#dlo{plugins=[{?MODULE,none}|Pdl]};
%%        _ ->
%%            Draw = edge_fun(Path),
%%            D#dlo{plugins=[{?MODULE,Draw}|Pdl]}
%%    end.
%%
%%edge_fun(Path) ->
%%    {EdBin,ClBin} = pump_edges(Path),
%%    [VboEs,VboCl] = gl:genBuffers(2),
%%    gl:bindBuffer(?GL_ARRAY_BUFFER, VboEs),
%%    gl:bufferData(?GL_ARRAY_BUFFER, byte_size(EdBin), EdBin, ?GL_STATIC_DRAW),
%%    gl:bindBuffer(?GL_ARRAY_BUFFER, VboCl),
%%    gl:bufferData(?GL_ARRAY_BUFFER, byte_size(ClBin), ClBin, ?GL_STATIC_DRAW),
%%    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
%%    N = byte_size(EdBin) div 12,
%%    D = fun() ->
%%                gl:bindBuffer(?GL_ARRAY_BUFFER, VboEs),
%%                gl:vertexPointer(3, ?GL_FLOAT, 0, 0),
%%                gl:bindBuffer(?GL_ARRAY_BUFFER, VboCl),
%%                gl:colorPointer(3, ?GL_FLOAT, 0, 0),
%%                gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
%%                gl:enableClientState(?GL_COLOR_ARRAY),
%%                gl:enableClientState(?GL_VERTEX_ARRAY),
%%                gl:drawArrays(?GL_LINES, 0, N),
%%                gl:disableClientState(?GL_VERTEX_ARRAY),
%%                gl:disableClientState(?GL_COLOR_ARRAY)
%%        end,
%%    {call,D,[{vbo,VboEs},{vbo,VboCl}]}.
%%
%%pump_edges(Path) ->
%%    pump_edges_1(Path, {<<>>,<<>>}).
%%
%%pump_edges_1([], Bin) -> Bin;
%%pump_edges_1([{{X1,Y1,Z1}=V1,_,V2,V3}|T],{Edge,Col}) ->
%%    {X2,Y2,Z2} = e3d_vec:add(V1,e3d_vec:mul(V2, 0.25)),
%%    {X3,Y3,Z3} = e3d_vec:add(V1,e3d_vec:mul(V3, 0.25)),
%%    {AR1,AG1,AB1} = {AR2,AG2,AB2} = {1.0,0.0,0.0},
%%    {FR1,FG1,FB1} = {FR3,FG3,FB3} = {0.0,0.0,1.0},
%%    Edge0 = <<Edge/binary,X1:?F32,Y1:?F32,Z1:?F32,X2:?F32,Y2:?F32,Z2:?F32,
%%              X1:?F32,Y1:?F32,Z1:?F32,X3:?F32,Y3:?F32,Z3:?F32>>,
%%    Col0 = <<Col/binary,AR1:?F32,AG1:?F32,AB1:?F32,AR2:?F32,AG2:?F32,AB2:?F32,
%%             FR1:?F32,FG1:?F32,FB1:?F32,FR3:?F32,FG3:?F32,FB3:?F32>>,
%%    pump_edges_1(T,{Edge0,Col0}).
%%
%%get_data(update_dlist, _Data, Acc) ->  % for draw lists
%%    case ets:lookup(wpc_sweep_along,path) of
%%        [{path,[]}] ->
%%            {ok, Acc};
%%        [{path,Path}] ->
%%            {ok, [{plugin, {?MODULE, {path, Path}}}|Acc]};
%%        _ ->
%%            {ok, Acc}
%%    end.
%%
%%draw(plain, Path, _D, _SelMode) ->
%%    gl:lineWidth(1.5),
%%    wings_dl:call(Path);
%%draw(_,_,_,_) -> ok.
