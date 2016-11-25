%%
%%  wpc_lathe --
%%
%%     Lathe Plugin
%%
%%  Copyright (c) 2016 Micheus
%%  
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_lathe).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").
-import(math, [cos/1,sin/1,pi/0]).

init() -> true.

menu({face}, []) ->
    menu();
menu({face}, Menu) ->
    menu()++Menu;
menu(_, Menu) -> Menu.

menu() ->
    [{lathe(),lathe,?__(2,"Create an object by rotating a 2D shape around a fixed axis"),[option]}].

lathe() ->
    ?__(1,"Lathe").

command({face,{lathe, Ask}}, St) -> make_lathe(Ask, St);
command({lathe, Ask}, St) -> make_lathe(Ask, St);
command(_, _) -> next.

%%% The rest are local functions.

%%%
%%% lathe
%%%

lathe_dialog() ->
    [{label_column, [
	{?__(1,"Degrees"), {text,45.0,[{key,degrees},{range,{0.0,360.0}}]}},
	{?__(2,"Sections"), {text,2,[{key,sections},{range,{2,infinity}}]}}]
     },
     {?__(4,"Flatten region"),false,[{key,flatten}]},
     panel,
     {hradio, [
	{wings_util:stringify(x),x},
	{wings_util:stringify(y),y},
	{wings_util:stringify(z),z}],
	y, [{key,axis},{title,?__(5,"Reference axis")}]},
     wings_shapes:transform_obj_dlg()].

make_lathe(Arg, #st{selmode=face, sel=[{_,_}]}=St) when is_atom(Arg) ->
    Qs = lathe_dialog(),
    Label = ?__(1,"Lathe Options"),
    wings_dialog:dialog_preview({face,lathe}, Arg, Label, Qs, St);
make_lathe(Arg, #st{selmode=face, sel=[{Id,Faces}], shapes=Shps0}=St) ->
    ArgDict = dict:from_list(Arg),
    Degrees = dict:fetch(degrees, ArgDict),
    Sections = dict:fetch(sections, ArgDict),
    Axis = dict:fetch(axis, ArgDict),
%    Flatten = dict:fetch(flatten, ArgDict),
    Modify = [{dict:fetch(rot_x, ArgDict), dict:fetch(rot_y, ArgDict), dict:fetch(rot_z, ArgDict)},
	      {dict:fetch(mov_x, ArgDict), dict:fetch(mov_y, ArgDict), dict:fetch(mov_z, ArgDict)},
	      dict:fetch(ground, ArgDict)],

    We0 = gb_trees:get(Id, Shps0),
    RgnsLst = wings_sel:face_regions(Faces,We0),

    %% uses the normal from the "largest" region as reference for flatten operation
    RgnN = get_region_normal(RgnsLst, We0),
    Center0 = wings_sel:center(St),

    %% flatten the selected region creating the pattern
    {_, #st{shapes=Shps}} = wings_face_cmd:command({flatten,{RgnN, Center0}}, St),
    We = gb_trees:get(Id, Shps),

    %% prepare the pattern data to be used to create the faces to the "pipe"
    Shapes0 =
	lists:foldl(fun(Rgn, {AccEs,AccFs})->
			PtrnOutline = get_edge_data(Rgn, We),
			PtrnShape = get_face_data(Rgn, We),
			case AccEs of
			    [] -> {PtrnOutline,PtrnShape};
			    _ -> {AccEs++PtrnOutline,AccFs++PtrnShape}
			end
		    end, {[],[]}, RgnsLst),
    Shapes = process_pattern(Shapes0),
    build_lathe(Degrees, Sections, Axis, Modify, Shapes);
make_lathe(_, St) -> St.

get_region_normal(RgnsLst, We) ->
    [Rgn] =
	lists:foldr(fun(RgnSet, []) ->
			    [RgnSet];
		       (RgnSet, [RgnSetOld]=Acc) ->
			    if size(RgnSet) > size(RgnSetOld) ->
			    [RgnSetOld];
			    true -> Acc
			    end
		    end, [], RgnsLst),
    wings_face:normal(gb_sets:largest(Rgn), We).



%%%%%
%%%%% Lathe
%%%%%

build_lathe(Degrees, Seg, Axis, [Rot, Mov, Ground], {OutVs, FaceVs, VsOutline, VsFaces}=Shapes) ->
    io:format("\n===========\nShape: ~p\n==========\n",[Shapes]),
    Vs0 = build_lathe_vertices(Degrees, Seg, Axis, OutVs, FaceVs),
    io:format("\n------------------\nVs(~p): ~p\n\n",[length(Vs0),Vs0]),
    OutLen = lists:flatlength(OutVs),
    FaceLen = lists:flatlength(FaceVs),
    Fs = build_lathe_faces(Degrees, Seg, {OutLen,FaceLen}, VsOutline, VsFaces),
    io:format("\n------------------\nFs(~p): ~p\n",[length(Fs),Fs]),

    Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
    {new_shape,lathe(),Fs,Vs}.

fix_axis_dist(x, {_,Y,Z}) -> {0.0,Y,Z};
fix_axis_dist(y, {X,_,Z}) -> {X,0.0,Z};
fix_axis_dist(z, {X,Y,_}) -> {X,Y,0.0}.

build_lathe_vertices(Degrees, Seg, Axis0, OutVs, FaceVs) ->
    {Min,_Max} = e3d_bv:box(OutVs),
%    {Xsz,Ysz,Zsz} = e3d_vec:sub(Max, Min),
    Axis = wings_util:make_vector(Axis0),
    Radius = fix_axis_dist(Axis0,Min),
    Closed = Degrees =:= 360.0,
    Vs =
	case Closed of
	    true ->
		%Seg,
		Delta = Degrees/Seg,
	    	[transform_shape(I, Radius, Delta, Axis, Min, OutVs) || I <- lists:seq(0,Seg)];
	    _ ->
		StopGap = OutVs++FaceVs,
		Delta = Degrees/Seg+1,
		VsStart = transform_shape(0, Radius, Delta, Axis, Min, StopGap),
		VsEnd = transform_shape(Seg, Radius, Delta, Axis, Min, StopGap),
		VsMiddle = [transform_shape(I, Radius, Delta, Axis, Min, StopGap) || I <- lists:seq(1,Seg)],
		VsStart ++ VsMiddle ++ VsEnd
	end,
    lists:flatten(Vs).

transform_shape(I, Radius, Delta, Axis, Min, VsList) ->
    Mt = e3d_mat:translate(Radius),
    Mr = e3d_mat:rotate(Delta*I,Axis),
    Mm = e3d_mat:mul(Mt,Mr),
    lists:foldr(fun(V0, Acc) ->
		    V = e3d_vec:sub(V0,Min),
		    Acc++[e3d_mat:mul_vector(Mm,V)]
		end,[],VsList).

build_lathe_faces(Degrees, Seg0, {OutLen, FaceLen}, VsOutline, VsFaces) ->
    Closed = Degrees =:= 360.0,
    {StartIdx, Seg} =
	case Closed of
	    true -> {0,Seg0};
	    _ -> {(OutLen+FaceLen)*2, Seg0-1}
	end,
    io:format("VsOutline: ~p\nVsFaces: ~p\nOutLen: ~p\nFaceLen: ~p\nStartIdx: ~p\nSeg: ~p\n\n",[VsOutline, VsFaces, OutLen, FaceLen, StartIdx,Seg]),

    Sides =
	lists:foldl(fun(S, AccSds) ->
			lists:foldl(fun(Vs, AccSds0) ->
					Idx = StartIdx+(OutLen*S),
					build_face(Vs, Idx, AccSds0)
				    end, AccSds, VsOutline)
		    end, [], lists:seq(0,Seg)),

    Fs =
	case Closed of
	    true ->
		lists:foldl(fun(Vs, AccSds0) ->
		    lists:foldl(fun([_], Acc) -> Acc;
				   ([V1|[V2|_]], Acc) ->
				       Idx = StartIdx+OutLen*Seg,
				       [[V1+Idx,V2+Idx,V2,V1]|Acc]
				end, AccSds0, Vs)
			    end, Sides, VsOutline);
	    _ ->
		FsStart = VsFaces,
		FsEnd = [[(V+StartIdx) || V <- Vs] || Vs <- VsFaces],
		FsStart ++ Sides ++ FsEnd
	end,
    Fs.

build_face([_], _, Acc) -> Acc;
build_face([V1|[V2|_]=H], Idx, Acc) ->
    io:format(" -> V1: ~p | V2: ~p | Idx: ~p\n",[V1,V2,Idx]),
    Acc0 = [V1,V2,V2+Idx,V1+Idx],
    build_face(H, Idx, [Acc0|Acc]).

get_face_data([], _) -> [];
get_face_data(Fs, We) when is_list(Fs) ->
    get_face_data(gb_sets:from_list(Fs), We);
get_face_data(Fs, We) ->
    gb_sets:fold(fun(F, Acc)->
		    Einfo = get_edge_data([F], We),
		    [Finfo] = get_edges_seq(Einfo,[]),
		    Acc ++Finfo
		 end, [], Fs).

get_edge_data([], _)-> [];
get_edge_data(Fs, We) when is_list(Fs) ->
    get_edge_data(gb_sets:from_list(Fs), We);
get_edge_data(Fs, #we{es=Etab, vp=Vtab}=We) ->
    Edges = wings_face:outer_edges(Fs, We),
    Einfo =
	lists:foldl(fun(E, Acc) ->
			#edge{vs=Vs,ve=Ve,lf=Lf} = array:get(E, Etab),
			Vps = array:get(Vs, Vtab),
			Vpe = array:get(Ve, Vtab),
			Ei =
			    case gb_sets:is_member(Lf,Fs) of
				true -> {E, Vps, Vpe};
				_ ->  {E, Vpe, Vps}
			    end,
			Acc++[Ei]
		    end, [], Edges),
    get_edges_seq(Einfo,[]).

get_edges_seq([], Acc) -> Acc;
get_edges_seq([H|T], Acc) ->
    {NewList,Seg} = get_edges_seq_0(H, T, {[],[H]}),
    get_edges_seq(NewList, Acc++[Seg]).

get_edges_seq_0(_, [], Acc) -> Acc;
get_edges_seq_0({_,_,Vb}, List, {_,Seq}=Acc) ->
    case lists:keytake(Vb, 2, List) of
	{value, H0, List2} ->
	    get_edges_seq_0(H0, List2, {List2, Seq++[H0]});
	_ -> Acc
    end.

%%
%%
%%
process_pattern({RgnOutline,Faces0}) ->
    %% mapping the vertices of the regions' outline
    {Idx, VsMap0, VsOutline} =
	lists:foldl(fun(Es, {Idx0,Acc0,Vs0}) ->
			{Idx1,Acc1,Vs1} =
			    lists:foldl(fun({_,_Ve,Vs}, {Idx, Acc, Vcc}) ->
					    case gb_sets:size(Acc) of
						0 -> {Idx+1, gb_trees:enter(Vs, Idx, Acc), [Idx]};
						_ ->
						    case gb_trees:lookup(Vs, Acc) of
							none ->
							    {Idx+1, gb_trees:enter(Vs, Idx, Acc),[Idx|Vcc]};
							{value, Idx2} ->
							    {Idx, Acc, [Idx2|Vcc]}
						    end
					    end
					end, {Idx0,Acc0,[]}, Es),
	    		{Idx1,Acc1,[lists:reverse(Vs1)|Vs0]}
		    end, {0, gb_trees:empty(), []}, RgnOutline),

    %% mapping the vertices of the regions' faces (some are already in the outline)
    {_, VsMap1, VsFaces} =
	lists:foldl(fun(Fs, {Idx1,Outline2,Acc0}) ->
	    		{Idx2,Outline4,Fs0} =
			    lists:foldl(fun({_,_,Vs}, {Idx3,Outline3,Acc}) ->
					    case gb_trees:lookup(Vs, Outline3) of
						none ->
						    {Idx3+1, gb_trees:enter(Vs, Idx3, Outline3), Acc++[Idx3]};
						{value, V} -> {Idx3, Outline3, Acc++[V]}
					    end
					end, {Idx1,Outline2,[]}, Fs),
			case Acc0 of
			    [] -> {Idx2,Outline4,[Fs0]};
			    _ -> {Idx2,Outline4,Acc0++[Fs0]}
			end
		    end, {Idx, VsMap0, []}, Faces0),

    VsTmp0 = coord_to_index(gb_trees:to_list(VsMap0)),
    VsTmp1 = coord_to_index(gb_trees:to_list(VsMap1)),
    VsTmp2 = lists:subtract(VsTmp1, VsTmp0),
    OutVs  = [Coord || {_,Coord} <-VsTmp0],
    FaceVs = [Coord || {_,Coord} <-VsTmp2],

%%    io:format("VsTmp0: ~p\n",[VsTmp0]),
%%    io:format("VsTmp1: ~p\n",[VsTmp1]),
%%    io:format("VsMap: ~p\n",[VsMap]),
%%    io:format("VsOutline: ~p\n",[VsOutline]),
%%    io:format("VsFaces: ~p\n",[VsFaces]),
    {OutVs, FaceVs, VsOutline, VsFaces}.

coord_to_index(Src) ->
    lists:sort([{Value,Key} || {Key,Value} <- Src]).