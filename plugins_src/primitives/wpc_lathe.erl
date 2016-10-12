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

make_lathe(Arg, St) when is_atom(Arg) ->
    Qs = lathe_dialog(),
    Label = ?__(1,"Lathe Options"),
    wings_dialog:dialog_preview({face,lathe}, Arg, Label, Qs, St);
make_lathe(Arg, St) ->
    ArgDict = dict:from_list(Arg),
    Degrees = dict:fetch(degrees, ArgDict),
    Sections = dict:fetch(sections, ArgDict),
    Axis = dict:fetch(axis, ArgDict),
    Flatten = dict:fetch(flatten, ArgDict),
    Modify = [{dict:fetch(rot_x, ArgDict), dict:fetch(rot_y, ArgDict), dict:fetch(rot_z, ArgDict)},
	      {dict:fetch(mov_x, ArgDict), dict:fetch(mov_y, ArgDict), dict:fetch(mov_z, ArgDict)},
	      dict:fetch(ground, ArgDict)],
    Shape =
	wings_sel:fold(fun(Faces, We, Acc) ->
			    Rgns = wings_sel:face_regions(Faces,We),
			    EdgeList0 =
				lists:foldl(fun(Rgn, Acc0)->
						Edges = wings_face:outer_edges(Rgn,We),
						OrdEdges1 = wings_edge_loop:edge_links(Edges,We),
				    		OrdEdges2 =
						    lists:foldr(fun(Vs, Acc1)->
								    [V|_]=Vs0=[V1 || {_, V1, _} <- Vs],
								    Acc1++[Vs0++[V]]
								end,[], OrdEdges1),
				    		[Acc0|OrdEdges2]
					    end,[],Rgns),
			    Vs0 = lists:flatten(EdgeList0),
			    io:format("EdgeLists0: ~p\nFlatten: ~p\n\n",[EdgeList0,Vs0]),
			    N = wings_face:face_normal_cw(Vs0,We),
			    #we{vp=Vtab}=We0 =
				if Flatten -> wings_vertex:flatten(Vs0,N,We);
				    true -> We
				end,
			    Bb = wings_vertex:bounding_box(Vs0, We0),
			    EdgeList =
				lists:foldl(fun([], Acc0)-> Acc0;
					    (VsRgn, Acc0)->
						Acc0 ++[[array:get(Va,Vtab) || Va <- VsRgn]]
					    end,[],EdgeList0),
			    Acc++[{Bb,N,EdgeList}]
		       end, [], St),
    make_lathe(Degrees, Sections, Axis, Modify, Shape).

%%%%%
%%%%% Lathe
%%%%%

make_lathe(Degrees, Sections, Axis, [Rot, Mov, Ground], Shape) ->
    io:format("Shape: ~p\n\n------------------\n",[Shape]),
    {Vs0,Fs} = lathe_verts(Degrees, Sections, Axis, Shape),
    io:format("\n------------------\nVs: ~p\nFs: ~p\n",[Vs0,Fs]),
    Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
    {new_shape,lathe(),Fs,lists:flatten(Vs)}.

fix_axis_dist(x, {_,Y,Z}) -> {0.0,Y,Z};
fix_axis_dist(y, {X,_,Z}) -> {X,0.0,Z};
fix_axis_dist(z, {X,Y,_}) -> {X,Y,0.0}.

lathe_verts(Degrees, Sections0, Axis0, Shapes) ->
    Axis = wings_util:make_vector(Axis0),
    Closed = Degrees =:= 360.0,
    Sections =
    	if Closed -> Sections0;
	true -> Sections0+1
	end,
    Delta = Degrees/Sections,
    {Vs,Fs} =
	lists:foldl(fun({[{X1,Y1,Z1},{X2,Y2,Z2}],_,VsList0}, Acc)->
			Min = {min(X1,X2),min(Y1,Y2),min(Z1,Z2)},
	    		Radius = fix_axis_dist(Axis0,Min),
	    		lists:foldr(fun([_|VsList1], {VsAcc,FsAcc})->
			    VsList = [e3d_vec:sub(V,Min) || V <- VsList1],
			    Vs0 = lathe_verts(Sections, Radius, Delta, Axis, lists:reverse(VsList)),
			    %Vs0 = lathe_verts(Sections, Radius, Delta, Axis, VsList),
			    Fs0 = lathe_faces(length(VsList),Sections,Closed),
			    {[Vs0|VsAcc], FsAcc++Fs0}
			end, Acc,VsList0)
		    end, {[],[]}, Shapes),
    {lists:flatten(Vs),Fs}.

lathe_verts(S, Radius, Delta, Axis, VsList) ->
    RotFun =
	fun(I) ->
	    Mt = e3d_mat:translate(Radius),
	    Mr = e3d_mat:rotate(Delta*I,Axis),
	    Mm = e3d_mat:mul(Mt,Mr),
	    [e3d_mat:mul_vector(Mm,V) || V <- VsList]
	end,
    Vs = [RotFun(I) || I <- lists:seq(0,S-1)],
    lists:flatten(Vs).

lathe_faces(N,S,Closed) ->
    N0 = N*(S-1),
    Ns = lists:seq(0, N-1),
    Ss = lists:seq(0, S-2),
    Sides = [[M*N+I, M*N+((I+1) rem N), M*N+(N+(I+1) rem N), M*N+(N+I)] || M <- Ss, I <- Ns],
    case Closed of
	true ->
	    Fun =
	    fun(I) ->
		{I1,I2} =
		if I=:=0 -> {N,N0+N};
		    true -> {I,N0+I}
		end,
		[I, I1-1, I2-1, N0+I]
	    end,
	    Close = [Fun(I) || I <- Ns],
	    lists:append(Sides,Close);
	_ ->
	    Front = lists:reverse(lists:seq(0, N-1)),
	    Back = lists:seq(N0, N0+N-1),
	    [Front, Back | Sides]
    end.
