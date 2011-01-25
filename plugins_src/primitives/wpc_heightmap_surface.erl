%%
%%  wpc_heightmap_surface.erl --
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

-module(wpc_heightmap_surface).

-define(NEED_OPENGL, 1).

-include("wings.hrl").
-include("e3d.hrl").
-include("e3d_image.hrl").
-export([init/0,menu/2,command/2]).

init() -> true.

menu({shape}, Menu) ->
    Menu ++ [{?__(1,"Heightmap Surface..."),heightmap_surface_editor,
	      	  ?__(2,"L: Create Open the heightmap surface editor window for creation of a surface object")}];
menu(_,Menu) -> 
	Menu.

command({shape,heightmap_surface_editor}, St) ->
	start_heightmap_editor(St);
command({Name}, St) ->
	load_heightmap_image(Name, St);
command(_, _) -> 
    next.

build_shape(Prefix, Fs, Vs, #st{onext=Oid}=St) ->
%build_shape(Prefix, Fs, Vs, Vc, He, #st{onext=Oid}=St) ->
%	Mesh=#e3d_mesh{type=polygon,vs=Vs,vc=Vc,tx=[],ns=[],fs=Fs,he=He},
%	We = wings_we:build(body, Mesh),
	We = wings_we:build(Fs,Vs),
%	wings_we:invert_normals(We),
    Name = Prefix++integer_to_list(Oid),
    wings_shape:new(Name, We, St).
    
start_heightmap_editor(St) ->
	load_heightmap_image().

load_heightmap_image() ->
    Ps = [{extensions,wpa:image_formats()}],
    wpa:import_filename(Ps, fun(N) -> {N} end).

load_heightmap_image(Name, St) ->
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
  
process_surface(#e3d_image{width=W,height=H,bytes_pp=Bpp,image=Pixels}, St) ->
%    io:format("Image size: ~px~p  Bpp: ~w~n", [W,H,Bpp]),
%   Bpp0=Bpp-1,
%	Vs1 = << << R:1/binary >> || << R:1/binary,_:Bpp0/binary >> <= Pixels >>,
	Vs1=list_to_binary(pixel_to_height(Pixels, Bpp)),
	
%	{MinY,MaxY}=bin_range(Vs1),
    Factors=get_factor(H,W,16,8),
    Offset={(H-1)/2.0,(W-1)/2.0},

%%  Top vertex calculated from image dimensions(W,H => Z,X) and color (Y)
	Vs0 = [gen_vertex(W,Offset,Factors,Vs1,X,Z) || 
    		X <- lists:seq(0,H-1), 
    		Z <- lists:seq(0,W-1)],
%%  Bottom vertex
    VsBot = [{X,-0.1,Z} || {X,_,Z} <- Vs0],
%%  All vertex to build surface
    Vs = lists:append(Vs0,VsBot),

    VsDx=W*H,
    Fs0 = [gen_face(W,X,Z,0) || 
    		Z <- lists:seq(0,W-2), 
    		X <- lists:seq(0,H-2)],
    Fs1 = [gen_face(W,X,Z,VsDx) || 
    		Z <- lists:seq(0,W-2), 
    		X <- lists:seq(0,H-2)],
    Fs2 = [[I,I+1,VsDx+I+1,VsDx+I] || I<-lists:seq(0,W-2)],         %North
    Fs3 = [[VsDx-W+I,2*VsDx-W+I,2*VsDx-W+I+1,VsDx-W+I+1] || I<-lists:seq(0,W-2)],  %South
    Fs4 = [[I+W,VsDx+I+W,VsDx+I,I]|| I<-lists:seq(W-1,(H-1)*W,W)],  %Est
    Fs5 = [[I,VsDx+I,VsDx+I+W,I+W]|| I<-lists:seq(0,(H-2)*W,W)],    %West
    Fsa=lists:append(Fs0,Fs1),
    Fsb=lists:append(Fs2,Fs3),
    Fsc=lists:append(Fs4,Fs5),
    Fsd=lists:append(Fsa,Fsb),
    Fs=lists:append(Fsc,Fsd),
	Vc=[],
	He=[],
 	build_shape("heightmap_surface",Fs,Vs,St).
% 	build_shape("heightmap_surface",Fs,Vs,Vc,He,St).

ratio(D, D) -> {1.0,1.0};
ratio(W, H) when W < H -> {1.0,H/W};
ratio(W, H) -> {W/H,1.0}.
	
get_factor(W0,H0,Max,FctY) ->
	{Rw,Rh}=ratio(W0,H0),
	{Max/W0*Rw,FctY,Max/H0*Rh}.
	
pixel_to_height(Pixel,Bpp) ->
	case Pixel of
	<<>> ->
		[];
	_ ->
		case Bpp > 2 of
		true ->
			Bpp0=Bpp-3,
			<<R0:1/binary,G0:1/binary,B0:1/binary,_:Bpp0/binary,T/binary>>=Pixel,
			[R]=binary_to_list(R0),
			[G]=binary_to_list(G0),
			[B]=binary_to_list(B0),
			[[rgb_to_mono(R,G,B)]|pixel_to_height(T,Bpp)];
		false ->
			Bpp0=Bpp-1,
			<<R0:1/binary,_:Bpp0/binary,T/binary>>=Pixel,
			[R]=binary_to_list(R0),
			[[R]|pixel_to_height(T,Bpp)]
		end
	end.

rgb_to_mono(R,G,B) when R==G;G==B->
	R;
rgb_to_mono(R,G,B) -> 
	trunc(170*R/255 + 85*G/255 + B/255).

gen_vertex(MaxZ,Offset,Factor,Ybin,X,Z) ->
	Idx=MaxZ*X+Z,
	case Idx==0 of 
	true ->
		<<Y0:1/binary,_/binary>>=Ybin;
	false ->
		<<_:Idx/binary,Y0:1/binary,_/binary>>=Ybin
	end,
	[Y]=binary_to_list(Y0),
	gen_vertex(Offset,Factor,X,Y,Z).
	
gen_vertex({Dx,Dz},{FctX, FctY, FctZ},X,Y,Z) ->
	{FctX*(X-Dx),FctY*(Y/255),FctZ*(-Z+Dz)}.
    
gen_face(MaxZ,X,Z,Offset) ->
	V0=(X*MaxZ)+Z+Offset,
	V1=V0+MaxZ,
	V2=V1+1,
	V3=V0+1,
	case Offset > 0 of
	true -> [V0,V3,V2,V1];
	false -> [V0,V1,V2,V3] 
	end.

create_window() ->
	ok.