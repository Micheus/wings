%%
%%  wpc_bump_surface.erl --
%%
%%     A surface creator by using a heiht map.
%%
%%  Copyright (c) 2011 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_bump_surface).

-define(NEED_OPENGL, 1).

-include("wings.hrl").
-include("e3d.hrl").
-include("e3d_image.hrl").
-export([init/0,menu/2,command/2]).

init() -> true.


menu({tools}, Menu) ->
    Menu ++ [separator,
	     {?__(1,"Bump Surface Editor Window"),bump_surface_editor_window,
	      ?__(2,"Open the bump surface editor window for creation of a surface object")}];
menu(_Dbg, Menu) ->
    Menu.

command({tools,bump_surface_editor_window}, St) ->
	start_bump_editor(St);
command({Name}, St) ->
	load_bump_image(Name, St);
command(_, _) -> 
    next.

build_shape(Prefix, Fs, Vs, #st{onext=Oid}=St) ->
    We = wings_we:build(Fs, Vs),
    Name = Prefix++integer_to_list(Oid),
    wings_shape:new(Name, We, St).
    
start_bump_editor(St) ->
	load_bump_image().

load_bump_image() ->
    Ps = [{extensions,wpa:image_formats()}],
    wpa:import_filename(Ps, fun(N) -> {N} end).

load_bump_image(Name, St) ->
    Ps = [{filename,Name}],
    case wpa:image_read(Ps) of
	{error,Error} ->
	    wpa:error_msg(?__(1,"Failed to load file: ~s\n"),
		      [file:format_error(Error)]);
	Image ->
		case wings_image:maybe_exceds_opengl_caps(Image) of
		{error,GlErr} ->
			wpa:error_msg(?__(3,"The image cannot be loaded.~nFile: \"~s\"~n GLU Error: ~p - ~s~n"),
				  [Name,GlErr, glu:errorString(GlErr)]);
		Image0 ->
			process_surface(Image0, St)
		end
    end.
    
process_surface_1(#e3d_image{width=W,height=H,bytes_pp=Bpp,image=Pixels}, St) ->
    io:format("Bump surfice using fixed value~n", []),
 	Vs=[{-0.5,1.0,0.5},{-0.5,1.0,-0.5},{0.5,1.0,0.5},{0.5,1.0,-0.5},{-0.5,-0.5,0.5},{-0.5,-0.5,-0.5},{0.5,-0.5,0.5},{0.5,-0.5,-0.5}],
 	Fs=[[4,6,2,0],[2,3,7,6],[4,5,7,6],[0,2,3,1],[5,1,3, 7],[4,0,1,5]],
% 	Vs=[{-0.5,-0.5,0.5},{-0.5,1.0,0.5},{0.5,1.0,0.5},{0.5,-0.5,0.5},{-0.5,-0.5,-0.5},{-0.5,1.0,-0.5},{0.5,1.0,-0.5},{0.5,-0.5,-0.5}],
% 	Fs=[[0,3,2,1],[2,3,7,6],[0,4,7,3],[1,2,6,5],[4,5,6,7],[0,1,5,4]],
    io:format(" Elements of the Vs list: ~w~n", [Vs]),
    io:format(" Elements of the Fs list: ~w~n", [Fs]),
    io:format("Calling build_shape~n=========================~n", []),

 	build_shape("bump_surface",Fs,Vs,St).

process_surface(#e3d_image{width=W,height=H,bytes_pp=Bpp,image=Pixels}, St) ->
    io:format("Image size: ~px~p  Bpp: ~w~n", [W,H,Bpp]),
    Bpp0=Bpp-1,
	Vs1 = << << R:1/binary >> || << R:1/binary,_:Bpp0/binary >> <= Pixels >>,
%	{MinY,MaxY}=bin_range(Vs1),
%    io:format("Heigh value extracted from binary data: ~p~n=========================~n", [Vs1]),
    Dx=(W-1)/2.0,
    Dz=(H-1)/2.0,
    %{Dx,-0.5,Dz},{-Dx,-0.5,Dz},{-Dx,-0.5,-Dz},{Dx,-0.5,-Dz}
	Vs0 = [gen_vertex(W,H,Vs1,X,Z) || 
    		X <- lists:seq(0,W-1), 
    		Z <- lists:seq(0,H-1)],
    VsBot = [{X,-0.1,-Z} || X <- [-Dx,Dx], Z <- [-Dz,Dz]],
    Vs = lists:append(Vs0,VsBot),
%    io:format("VERTEX LIST~n Quantity of elements of the list Vs: ~w~n", [length(Vs)]),
%    io:format(" Elements of the Vs list: ~w~n", [Vs]),

    Fs0 = [gen_face(W,H,X,Z) || 
    		X <- lists:seq(0,W-2), 
    		Z <- lists:seq(0,H-2)],
%    io:format(" Elements of the Fs0 list: ~w~n", [Fs0]),
    VsDx=W*H,
%    Fs1 = [[VsDx,VsDx+2,VsDx+3,VsDx+1]|Fs0],
    Fs1 = [[VsDx,VsDx+1,VsDx+3,VsDx+2]],
%    io:format(" Elements of the Fs1 list: ~w~n", [Fs1]),
    Fs2 = [lists:append([VsDx+1,VsDx],[I|| I<-lists:seq(0,H-1)])],  %North
%    io:format(" Elements of the Fs2 list: ~w~n", [Fs2]),
    Fs3 = [lists:append([VsDx+2,VsDx+3],[H*W-1-I|| I<-lists:seq(0,H-1)])],  %South
%    io:format(" Elements of the Fs3 list: ~w~n", [Fs3]),
    Fs4 = [lists:append([VsDx,VsDx+2],[(W-1)*H-I|| I<-lists:seq(0,(W-1)*H,H)])],  %Est
%    io:format(" Elements of the Fs4 list: ~w~n", [Fs4]),
    Fs5 = [lists:append([VsDx+3,VsDx+1],[I+H-1|| I<-lists:seq(0,(W-1)*H,H)])],  %West
%    io:format(" Elements of the Fs5 list: ~w~n", [Fs5]),
    Fsa=lists:append(Fs0,Fs1),
    Fsb=lists:append(Fs2,Fs3),
    Fsc=lists:append(Fs4,Fs5),
    Fsd=lists:append(Fsa,Fsb),
    Fs=lists:append(Fsc,Fsd),
%    io:format("FACE LIST~n Quantity of elements of the list Fs: ~w~n", [length(Fs)]),
%    io:format(" Elements of the Fs list: ~w~n", [Fs]),
%    io:format("~nSt: ~p~n~n", [St]),

 	build_shape("bump_surface",Fs,Vs,St).

gen_vertex(MaxX,MaxZ,Ybin,X,Z) ->
	Idx=MaxX*Z+X,
	case Idx==0 of 
	true ->
		<<Y0:1/binary,_/binary>>=Ybin;
	false ->
		<<_:Idx/binary,Y0:1/binary,_/binary>>=Ybin
	end,
	[Y]=binary_to_list(Y0),
	Dx=(MaxX-1)/2.0,
	Dz=(MaxZ-1)/2.0,
	{X-Dx,(Y/255)*10.0,-Z+Dz}.
%	{X-Dx,1.0,-Z+Dz}.
    
gen_face(MaxX,MaxZ,X,Z) ->
% io:format("MaxX: ~w  MaxZ: ~w  || X: ~w Z: ~w~n", [MaxX,MaxZ,X,Z]),
	V0=(X*MaxZ)+Z,
	V1=V0+MaxZ,
	V2=V1+1,
	V3=V0+1,
  [V0,V1,V2,V3].

create_window() ->
	ok.