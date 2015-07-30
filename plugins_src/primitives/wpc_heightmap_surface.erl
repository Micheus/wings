%%  wpc_heightmap_surface.erl --
%%
%%     A surface creator by using a heiht map (gray scale or color: red->blue)
%%  Copyright (c) 2011 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_heightmap_surface).

-define(NEED_OPENGL, 1).
-define(PREVIEW_SIZE, 128).
-define(MAX_SOURCE_SIZE, 384).
-define(MAX_HUE_VALUE, 360.0).
-define(MAX_RGB_VALUE, 255).
-define(BOTTOM_Y,-0.02).
-define(HUE_PARAMS,{1.0,1.0}).
-define(RGB_PARAMS,{1.0,0.0}).

-include("wings.hrl").
-include("e3d_image.hrl").

-export([init/0,menu/2,command/2]).

init() -> true.

menu({shape}, Menu) ->
    parse(Menu, [], false);
menu(_,Menu) -> 
	Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [heightmap_heading()|NewMenu];
parse(Menu, [], false) ->
    parse([], [heightmap_heading(),separator|Menu], true);
parse([{"ManifoldLab ... (ml)",_,_}=T|Rest], NewMenu, false) ->
    parse(Rest, [separator,heightmap_heading(),T|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

heightmap_heading() ->	
	Ft=filetypename(wpa:image_formats()),
	Ft0=string:join(Ft,", "),
	{?__(1,"Micheus add on..."),
	  {bymicheus, [{?__(2,"Height Map Surface..."), heightmap_surface_editor, 
    				io_lib:format(?__(3,"File browse for") ++ " ~s "++ ?__(4,"and then provide option"),[Ft0])}]}
    		}.	

filetypename([]) -> "";
filetypename([{Ext,_}|T]) ->
	Ext0=string:substr(Ext, 2),
	[Ext0|filetypename(T)].
	
command({shape,{process_surface,Args}}, St) ->
	process_surface(Args,St);
command({shape,{bymicheus,heightmap_surface_editor}}, St) ->
	load_heightmap_image(St);
command({Name}, St) ->
	load_heightmap_image(Name,St);
command(_, _) -> 
    next.

build_shape(Prefix, Fs, Vs, He, #st{onext=Oid}=St) ->
	We=case He of
	[] ->
		wings_we:build(Fs,Vs);
	He0 ->
		wings_we_build:we(Fs,Vs,He0)
	end,
    Name = Prefix++integer_to_list(Oid),
    wings_shape:new(Name, We, St).
	
load_heightmap_image(_) ->
    ImgDir = wings_pref:get_value(image_repository, wings_pref:get_value(current_directory)),
    Ps = [{extensions,wpa:image_formats()},{directory,ImgDir}],
    wpa:import_filename(Ps, fun(N) -> {N} end).

load_heightmap_image(Name,St) ->
    Ps = [{filename,Name}],
    case wpa:image_read(Ps) of
	{error,Error} ->
	    wpa:error_msg(?__(1,"Failed to load file:")++" ~s\n",
		      [file:format_error(Error)]);
	Image ->
		ImgDir = filename:dirname(Name),
		wings_pref:set_value(image_repository, ImgDir),
		Image0=maybe_new_chanel(Image),
		#e3d_image{width=W,height=H,bytes_pp=Bpp}=Image0,
		case maybe_resize(Image0,?MAX_SOURCE_SIZE) of
		{error,GlErr} ->
			wpa:error_msg(?__(2,"The image cannot be loaded.\nFile:") ++ "\"~s\"\n" ++ ?__(3,"GLU Error:") ++ " ~p - ~s\n",
				  [Name,GlErr, glu:errorString(GlErr)]);
		Image1 ->
			heightmap_dialog(Image1,W,H,Bpp,St)
		end
    end.

maybe_new_chanel(#e3d_image{bytes_pp=1,image=Pixels}=Image) ->
	NewPix= [mount_bit(G,G,G) || <<G:1/binary>> <= Pixels],
	PixBin= list_to_binary(NewPix),
	Image#e3d_image{image=PixBin,bytes_pp=4,type=r8g8b8a8};	
maybe_new_chanel(#e3d_image{bytes_pp=3,image=Pixels}=Image) ->
	NewPix= [mount_bit(R,G,B) || <<R:1/binary,G:1/binary,B:1/binary>> <= Pixels],
	PixBin= list_to_binary(NewPix),
	Image#e3d_image{image=PixBin,bytes_pp=4,type=r8g8b8a8};
maybe_new_chanel(Image) -> Image.
	
mount_bit(R,G,B) ->
	[R,G,B,<<0>>].

heightmap_dialog(Image,WSrc,HSrc,BppSrc,_) ->
	PreviewInfo=create_img_preview(Image),
	Image0=check_is_grayscale(Image),
	#e3d_image{width=W,height=H,bytes_pp=Bpp,image=Pixels}=Image0,
	case {WSrc,HSrc} of
	{W,H} ->
		Wstr=" ",
		Hstr=" ";
	{_,_} ->
		Wstr=io_lib:format(" (~w)",[WSrc]),
		Hstr=io_lib:format(" (~w)",[HSrc])
	end,
	case BppSrc of
	Bpp ->
		Bstr=" ";
	_ ->
		Bstr=io_lib:format(" (~w)",[BppSrc])
	end,

	Els=pixel_to_height(Pixels, Bpp),
	MinE=lists:min(Els),
	MaxE=lists:max(Els),

	Fxz=get_factor(W,H,10),
	Ws=W*Fxz,
	Hs=H*Fxz,
	Es=3.0,
	
	Fields_Hook = fun (Event, Params) ->
					case Event of
					update ->
						{Var,_,Val,Store}=Params,
						Store1=case Var of
						ar ->
							calc_aspect_ratio(W,H,gb_trees:get(ws,Store),Val,Store);
						ws ->
							calc_aspect_ratio(W,H,Val,gb_trees:get(ar,Store),Store);
						tcut_lo ->
							update_range_slider(Var,Val,Store,Bpp);
						tcut_hi ->
							update_range_slider(Var,Val,Store,Bpp);
						scut_lo ->
							update_range_text(Var,Val,Store,Bpp);
						scut_hi ->
							update_range_text(Var,Val,Store,Bpp);
						_ ->
							Store
						end,
						{store,gb_trees:update(Var,Val,Store1)};
					is_disabled ->
						{Var,_,Store}=Params,
						case Var of
						hs ->
							gb_trees:get(ar,Store);
						_ ->
							false
						end;
					_ -> 
						void
					end
				end,
    PrwImage = fun(X0,Y0,W0,H0,_) ->
		      img_preview(X0,Y0,W0,H0,PreviewInfo)
	      end,
    PrwRangeLo = fun(X0,Y0,W0,H0,Store) ->
		      range_preview(X0,Y0,W0,H0,Store,0,Bpp)
	      end,
    PrwRangeHi = fun(X0,Y0,W0,H0,Store) ->
		      range_preview(X0,Y0,W0,H0,Store,1,Bpp)
	      end,
    Fun = fun (Res) -> Res0={Res,{Els,MaxE}}, 
	    	{shape,{process_surface,Res0}} 
	      end,
	if Bpp > 2 ->
		HiValue=?MAX_HUE_VALUE,
		{Hue_p1,Hue_p2}=?HUE_PARAMS, % ref. to sat,val
		TextLo=MinE*100.0,      
		TextHi=MaxE*100.0,
		LoSlider={slider,[{color,{h,hue_p1,hue_p2}},{range,{0.0,?MAX_HUE_VALUE}},{key,scut_lo},{value,HiValue-MinE*HiValue},{hook,Fields_Hook}]},
	HiSlider={slider,[{color,{h,hue_p1,hue_p2}},{range,{0.0,?MAX_HUE_VALUE}},{key,scut_hi},{value,HiValue-MaxE*HiValue},{hook,Fields_Hook}]};
	true ->
		HiValue=?MAX_RGB_VALUE,
		{Hue_p1,Hue_p2}=?RGB_PARAMS, % ref. to hue,sat
		TextLo=MinE*100.0,      
		TextHi=MaxE*100.0,
		LoSlider={slider,[{color,{v,hue_p1,hue_p2}},{range,{0.0,1.0}},{key,scut_lo},{value,MinE},{hook,Fields_Hook}]},
		HiSlider={slider,[{color,{v,hue_p1,hue_p2}},{range,{0.0,1.0}},{key,scut_hi},{value,MaxE},{hook,Fields_Hook}]}
	end,
	
    Dialog =
			[{hframe,[
				{vframe,[
				   {custom,?PREVIEW_SIZE+2,?PREVIEW_SIZE+2,PrwImage},
				   {label,io_lib:format(?__(1,"Width:")++" ~w~s",[W,Wstr])},
				   {label,io_lib:format(?__(2,"Height:")++" ~w~s",[H,Hstr])},
				   {label,io_lib:format(?__(3,"Channels:")++" ~w~s",[Bpp,Bstr])},
				   {label,io_lib:format(?__(4,"Size:")++" ~s",[getkpixel(W,H)])},
				   {label,io_lib:format(?__(20,"Min Value:")++" ~.3f%",[MinE*100.0])},
				   {label,io_lib:format(?__(21,"Max Value:")++" ~.3f%",[MaxE*100.0])},
				   {value,W,[{key,iw}]},
				   {value,H,[{key,ih}]} ],
				  		[
				   {title,?__(13,"Image Info")} ]
				},
				{vframe,[
				   {hframe,[
				   	  {?__(5,"Keep aspect ratio"),true,[{hook,Fields_Hook},{key,ar}]} ]
				   },
				   {hframe,[
				   	  {vframe,[
				   	  	 {label,?__(6,"Width (X)")},
				   	  	 {label,?__(7,"Length (Z)")},
				   	  	 {label,?__(8,"Height (Y)")},
				 		 {label,?__(15,"Lower cut (%)")},
				 		 {label,?__(16,"High cut (%)")} ]
					  },
					  {vframe,[
					  	 {vframe,[
							{text,Ws,[{range,{0.01,infinity}},{width,6},{hook,Fields_Hook},{key,ws}]},
							{text,Hs,[{range,{0.01,infinity}},{width,6},{hook,Fields_Hook},{key,hs}]},
							{text,Es,[{range,{0.01,infinity}},{width,6},{key,es}]} ]
						 },
					  	 {vframe,[
							{value,HiValue,[no_focus,{key,hi_value}]},
							{value,Hue_p1,[no_focus,{key,hue_p1}]},
							{value,Hue_p2,[no_focus,{key,hue_p2}]},
					  	 	{hframe, [
								{text,TextLo,[{range,{0.0,100.0}},{width,5},{hook,Fields_Hook},{key,tcut_lo}]},
						 		{custom,?LINE_HEIGHT-2,?LINE_HEIGHT,PrwRangeLo},
						 		LoSlider ]
							},
					  	 	{hframe, [
								{text,TextHi,[{range,{0.0,100.0}},{width,5},{hook,Fields_Hook},{key,tcut_hi}]},
						 		{custom,?LINE_HEIGHT-2,?LINE_HEIGHT,PrwRangeHi},
						 		HiSlider ]
							} ]
						 } ]
					  } ]
				   },
				   {hframe,[
				   	  {vframe,[
				   	  	 {?__(17,"Use minor height as ground value"),false,[{key,gv}]},
				   	     separator,
				   	  	 {?__(18,"Create bottom faces"),false,[{key,bf}]},
				   	  	 {?__(19,"Create side faces"),false,[{key,sf}]},
				   	  	 {?__(11,"Create hard edges"),true,[{key,he}]},
				   	     separator,
				   	     {label,?__(9,"* It is highly recommended you use a small image")},
				   	     {label,?__(10,"(up to 65K pixels) to avoid memory problems.")} ]
				   	  } ]
				   } ],
				   		[
				   {title,?__(14,"Parameters")} ]
				} ]
			} ],
    wings_ask:dialog(?__(12,"Create a surface from height map"), Dialog, Fun).

getkpixel(W,H) ->
	Kp=W*H,
	case Kp>1000 of
	true -> integer_to_list(trunc(Kp/1000))++?__(1,"K pixels");
	false -> integer_to_list(Kp)++?__(2," pixels")
	end.

calc_aspect_ratio(W,H,Ws,Calc,Store) when Calc ->
	gb_trees:update(hs, H*(Ws/W), Store);
calc_aspect_ratio(_,_,_,_,Store) ->
	Store.

img_preview(X,Y,W,H,{Image,MaxU,MaxV}) ->
	wings_io:sunken_rect(X, Y, W, H),
    gl:enable(?GL_TEXTURE_2D),
	[TxId] = gl:genTextures(1),
	gl:bindTexture(?GL_TEXTURE_2D, TxId),
	gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    Format = texture_format(Image),
	#e3d_image{width=W0,height=H0,image=Bits}=Image,
    gl:texImage2D(?GL_TEXTURE_2D, 0, internal_format(Format),
                    W0, H0, 0, Format, ?GL_UNSIGNED_BYTE, Bits),
    W1=trunc(W0*MaxU),
    H1=trunc(H0*MaxV),
    Dx=(W-W1)div 2,
    Dy=(H-H1)div 2,
	draw_image(X+Dx,Y+Dy,W1,H1,TxId),
	wings_gl:deleteTextures([TxId]).

update_range_slider(Var,Val,Store,Bpp) ->    
	Var0=if Var==tcut_lo ->
		scut_lo;
	true ->
		scut_hi
	end,
	if Bpp>2 ->  % hue slider
		Val0=(100.0-Val)/100.0*?MAX_HUE_VALUE;
	true ->  % val slider
		Val0=Val/100.0
	end,
	gb_trees:update(Var0,Val0,Store).

update_range_text(Var,Val,Store,Bpp) ->
	Var0=if Var==scut_lo ->
		tcut_lo;
	true ->
		tcut_hi
	end,
	if Bpp>2 ->  % hue slider
		H=Val,
		S=gb_trees:get(hue_p1,Store),
		V=gb_trees:get(hue_p2,Store),
		Val0=((?MAX_HUE_VALUE-Val)/?MAX_HUE_VALUE)*100.0;
	true ->  % val slider
		H=gb_trees:get(hue_p1,Store),
		S=gb_trees:get(hue_p2,Store),
		V=Val,
		Val0=Val*100.0
	end,
	Color=wings_color:hsv_to_rgb(H,S,V),
	Store0=gb_trees:update(Var0,Val0,Store),
	gb_trees:update(Var,Color,Store0).
    
range_preview(X,Y,W,_,Store,Type,Bpp) ->
	Hue_p0=if Type==0 ->
		gb_trees:get(scut_lo,Store);
	true ->
		gb_trees:get(scut_hi,Store)
	end,
	if Bpp>2 ->  % hue slider
		Hue=Hue_p0,
		Sat=gb_trees:get(hue_p1,Store),
		Val=gb_trees:get(hue_p2,Store);
	true ->  % val slider
		Hue=gb_trees:get(hue_p1,Store),
		Sat=gb_trees:get(hue_p2,Store),
		Val=Hue_p0
	end,
	Color=wings_color:hsv_to_rgb(Hue,Sat,Val),
	wings_io:raised_rect(X, Y+2, W, ?LINE_HEIGHT-1, Color).
    
maybe_resize(#e3d_image{width=W,height=H}=Image, Dim) ->
	Max=max(W,H),
	if Max>Dim ->
		Fct=Dim/Max,
		case resize_image(Image,trunc(W*Fct),trunc(H*Fct)) of
		{error,_}=Error -> 
			Error;
		Image0 -> 
			Image0
		end;
	true -> 
		Image
	end.

create_img_preview(#e3d_image{width=W,height=H}=Image) ->
	case maybe_scale(Image) of
	{error,_}=Error ->
		Error;
	Image0 ->
		#e3d_image{width=W0,height=H0}=Image0,
		Image1=maybe_resize(Image0,?PREVIEW_SIZE),
		MaxU=W/W0,
		MaxV=H/H0,
		{Image1,MaxU,MaxV}
	end.
	
check_is_grayscale(#e3d_image{width=W,height=H,bytes_pp=Bpp,image=Pixels}=Image)->
	case Bpp > 2 of
	true ->
		Bpp0=Bpp-3,
		Ca = [ R || << R:1/binary, G:1/binary, B:1/binary, _:Bpp0/binary >> <= Pixels, R==G, G==B ],
		case (lists:flatlength(Ca) == W*H) of
		true ->
			NewPixel=list_to_binary(Ca),
			Image#e3d_image{type=g8,bytes_pp=e3d_image:bytes_pp(g8),image=NewPixel};
		_ ->
			Image  %% if a false gray-scale image be used we will get a color slider and the "monochromatic" image on preview - we should convert it to gray-scale before to use 
		end;
	_ ->
		Image
	end.

process_surface(DlgParams, St) ->
	{[{iw,W},{ih,H},{ar,_},{ws,Wm},{hs,Hm},{es,Em},{hi_value,_},{hue_p1,_},{hue_p2,_},{tcut_lo,CLo},{scut_lo,_},
	  {tcut_hi,CHi},{scut_hi,_},{gv,GroundValue},{bf,BottomFace},{sf,SideFace},{he,HardEdges}],{Els,_}}=DlgParams,
	  
	Before = erlang:now(),
	wings_pb:start(?__(1,"Building surface")),
	?SLOW(Nv=create_normalized_vertices(W,H,CLo/100.0,CHi/100.0,GroundValue,BottomFace,SideFace,Els)),

	wings_pb:update(0.50, ?__(2,"Scaling normalized vertices...")),
	?SLOW(Vs=[scale_vertices(Vertice,Wm,Em,Hm) || Vertice <- Nv]),

    wings_pb:update(0.70, ?__(3,"Computing the faces for surface...")),
    ?SLOW({Fs,He}=create_faces(W,H,BottomFace,SideFace)),
	Time = timer:now_diff(erlang:now(), Before),
	Str = format_time(Time),
	io:format("~s - ~s\n", [Str,"Time spent to compute the object data. Now, Wings3d is working..."]),

    wings_pb:update(0.85, ?__(4,"Building surface object...")),
%	Vc=[],
% 	build_shape_1("heightmap_surface",Fs,Vs,Vc,He,St).
	He0=if HardEdges==true -> He;
		true -> []
	end,
 	St0=build_shape("heightmap_surface",Fs,Vs,He0,St),
	wings_pb:update(1.0, ?__(5,"Done")),
	wings_pb:done(),
	St0.

create_normalized_vertices(W,H,CLo,CHi,GroundValue,BottomFace,SideFace,Els) ->
    Dx=(W-1)/2.0,
    Dz=(H-1)/2.0,
    if GroundValue==true ->
    	Dy=CLo;
    true ->
    	Dy=0.0
    end,

%  Computing the normalized heigth for each vertice that defines the grid
	?SLOW(VeticeList=gen_vertices(W,H,0,CLo,CHi,Els)),
%  Normalizing the X and Z coordenates that defines the grid
	VsTop=[{(X-Dx)/W,Y-Dy,(Z-Dz)/H} || {X,Y,Z} <- VeticeList],

%  Computing the vertices for the Bottom
	if BottomFace==true ->
	    VsBot = [{X,?BOTTOM_Y,Z} || {X,_,Z} <- VsTop];
	true ->
		if SideFace==true ->
			VNd = [{(X-Dx)/W,?BOTTOM_Y,(0.0-Dz)/H}		|| X <-lists:seq(0,W-1)],
			VEd = [{(W-1-Dx)/W,?BOTTOM_Y,(Z-Dz)/H}		|| Z <-lists:seq(1,H-2)],
			VSd = [{(W-X-Dx)/W,?BOTTOM_Y,(0.0+Dz)/H}	|| X <-lists:seq(1,W)],
			VWd = [{(0.0-Dx)/W,?BOTTOM_Y,(H-1-Z -Dz)/H}	|| Z <-lists:seq(1,H-2)],
			VB1 = lists:append(VNd,VEd),
			VB2 = lists:append(VSd,VWd),
			VsBot = lists:append(VB1,VB2);
		true ->
			VNd = [{(X-Dx)/W,?BOTTOM_Y,(0.0-Dz)/H}		|| X <- [0,W-1]],
			VSd = [{(W-X-1-Dx)/W,?BOTTOM_Y,(0.0+Dz)/H}	|| X <- [0,W-1]],
			VsBot = lists:append(VNd,VSd)
		end
	end,
    lists:append(VsTop,VsBot).

create_faces(W,H,BottomFace,SideFace) ->
%  Defining the faces of the surface's top
    Fs0 = [gen_face(W,X,Z,0) ||
    		Z <- lists:seq(0,W-2), 
    		X <- lists:seq(0,H-2)],

%  Defining the faces of the surface's bottom
    VsDx=W*H,
	if BottomFace==true ->
		Fs1 = [gen_face(W,X,Z,VsDx) || 
				Z <- lists:seq(0,W-2), 
				X <- lists:seq(0,H-2)];
	true ->
		if SideFace==true ->
			FSn = [VsDx+I			|| I<-lists:seq(0,W-1)],
			FSe = [VsDx+W-1+I		|| I<-lists:seq(1,H-2)],
			FSs = [VsDx+W+H-2+I		|| I<-lists:seq(0,W-1)],
			FSw = [VsDx+W*2+H-3+I	|| I<-lists:seq(1,H-2)],
			FS1 = lists:append(FSn,FSe),
			FS2 = lists:append(FSs,FSw),
			Fs1 = [lists:append(FS1,FS2)];
		true ->
			Fs1 = [[VsDx,VsDx+1,VsDx+2,VsDx+3]]
		end
	end,

%  Defining the edges to be used to build the side faces of the surface
	FNu = [I			|| I<-lists:seq(0,W-1)],			% North upper
	FSu = [VsDx-I		|| I<-lists:seq(1,W)],				% South upper
	FWu = [(H-1)*W-I	|| I<-lists:seq(0,(H-1)*W,W)],		% West upper
	FEu = [I+W-1		|| I<-lists:seq(0,(H-1)*W,W)],		% Est upper
	if (BottomFace==true) andalso (SideFace==true) ->
		FNd = [VsDx+I		|| I<-lists:seq(0,W-1)],		% North down
		FSd = [VsDx*2-1-I	|| I<-lists:seq(0,W-1)],		% South down
		FWd = [VsDx*2-W-I	|| I<-lists:seq(0,(H-1)*W,W)],	% West down
		FEd = [VsDx+W-1+I	|| I<-lists:seq(0,(H-1)*W,W)];	% Est down
	true ->
		if SideFace==true ->
			FNd = [VsDx+I			|| I<-lists:seq(0,W-1)],
			FSd = [VsDx+W+H-2+I		|| I<-lists:seq(0,W-1)],
			FWd = lists:append([VsDx+W*2+H-3+I	|| I<-lists:seq(0,H-2)],[VsDx]),
			FEd = [VsDx+W-1+I		|| I<-lists:seq(0,H-1)];
		true ->
			if BottomFace==true ->
				FNd = [VsDx+W-I		|| I<-lists:seq(1,W)],
				FSd = [VsDx*2-W+I	|| I<-lists:seq(0,W-1)],
				FWd = [VsDx+I		|| I<-lists:seq(0,(H-1)*W,W)],
				FEd = [VsDx*2-I-1	|| I<-lists:seq(0,(H-1)*W,W)];
			true ->
				FNd = [VsDx+1,VsDx],
				FSd = [VsDx+3,VsDx+2],
				FWd = [VsDx,VsDx+3],
				FEd = [VsDx+2,VsDx+1]
			end
		end
	end,
	
%  Defining the faces of the surface's sides
	if SideFace==true ->
		Fs2 = gen_face_1(FNu,FNd),  %North
		Fs3 = gen_face_1(FSu,FSd),  %South
		Fs4 = gen_face_1(FWu,FWd),  %West
		Fs5 = gen_face_1(FEu,FEd);  %Est
	true ->
		Fs2 = [lists:append(FNu,FNd)],  %North
		Fs3 = [lists:append(FSu,FSd)],  %South
		Fs4 = [lists:append(FWu,FWd)],  %West
		Fs5 = [lists:append(FEu,FEd)]  %Est
	end,
    Fsa = lists:append(Fs0,Fs1),
    Fsb = lists:append(Fs2,Fs3),
    Fsc = lists:append(Fs4,Fs5),
    Fsd = lists:append(Fsa,Fsb),
    Fs=lists:append(Fsc,Fsd),
    
%  Defining the hard edges of the surface
	if (SideFace==true) ->
		He1 = build_hard_edge(FNu,lists:reverse(FNd)),  %North
		He2 = build_hard_edge(FSu,lists:reverse(FSd)),  %South
		He3 = build_hard_edge(FWu,lists:reverse(FWd)),  %West
		He4 = build_hard_edge(FEu,lists:reverse(FEd));  %Est
    true ->
		He1 = build_hard_edge(FNu,FNd),  %North
		He2 = build_hard_edge(FSu,FSd),  %South
		He3 = build_hard_edge(FWu,FWd),  %West
		He4 = build_hard_edge(FEu,FEd)  %Est
    end,
    Hea = lists:append(He1,He2),
    Heb = lists:append(He3,He4),
    He=lists:append(Hea,Heb),
    {Fs,He}.

scale_vertices({X,Y,Z},Sx,_,Sz) when Y==?BOTTOM_Y ->
	{X*Sx,Y,Z*Sz};
scale_vertices({X,Y,Z},Sx,Sy,Sz) ->
	{X*Sx,Y*Sy,Z*Sz}.
	
get_factor(W0,H0,Dmax) when W0>H0 ->
	Dmax/W0;
get_factor(_,H0,Dmax) ->
	Dmax/H0.

%% Conversion of the binary data from image to elevation list	
pixel_to_height(Pixel,Bpp) ->
	case Pixel of
	<<>> ->
		[];
	_ ->
		case Bpp > 2 of
		true ->  %% r8g8b8 and r8g8b8a8
			Bpp0=Bpp-3,
			<<R0:1/binary,G0:1/binary,B0:1/binary,_:Bpp0/binary,T/binary>>=Pixel,
			[R]=binary_to_list(R0),
			[G]=binary_to_list(G0),
			[B]=binary_to_list(B0),
			[rgb_to_height(R,G,B)/?MAX_HUE_VALUE|pixel_to_height(T,Bpp)];
		false ->  %% g8 and g8a8
			Bpp0=Bpp-1,
			<<R0:1/binary,_:Bpp0/binary,T/binary>>=Pixel,
			[R]=binary_to_list(R0),
			[R/?MAX_RGB_VALUE|pixel_to_height(T,Bpp)]
		end
	end.

%% Conversion of rgb values to elevation
%% When image is a grayscale the max value is 255
% |  R  |  G  |  B  |
% +-----+-----+-----+
% | 255 | 255 | 255 | white  (the max height)
% |  :  |  :  |  :  | gray scale
% |  0  |  0  |  0  | black  (the min height)
% +-----+-----+-----+
% This will convert bad HUE color to a more proper value
rgb_to_height(V,V,V) ->
	V/?MAX_RGB_VALUE*?MAX_HUE_VALUE;
%% When image is colored - and using the HUE gradient - the max value is 359.9
%% Hue value is used in order to remove Saturation or lighting from the image's color
% |  R  |  G  |  B  |
% +-----+-----+-----+
% | 255 |  0  |     | Red  => Hue scale = 0° (the max height)
% |  :  |  :  |     | yellow
% |  0  | 255 |  0  | green
% |     |  :  |  :  | cyan
% |  0  |  0  | 255 | blue
% |  :  |     |  :  | violet
% | 254 |     |  0  | pink  => Hue scale = 359.999°  (the min height)
% +-----+-----+-----+
% After get the HUE value, it should be inverted
rgb_to_height(R,G,B) ->
	{Hue,_,_}=wings_color:rgb_to_hsv(R,G,B),
	?MAX_HUE_VALUE-Hue.

%% Extract the vertices list from the elevation list (E values)
%% W is the width of the image
gen_vertices(_,_,_,_,_,[]) -> [];		
gen_vertices(W,H,Idx,CutLo,CutHi,[E|T]) ->
	X=Idx rem W,
	Z=Idx div W,
    Y=if CutLo>CutHi ->
		if (E>CutHi) andalso (E<CutLo) ->  % it means that E is between CutHi and CutLo
			Offset=(CutLo-CutHi)/2,
			if E<(CutHi+Offset) ->  % round to Max value
				1.0;
			true ->  % round to CutLo value
				CutLo
			end;
		E<CutHi ->  % set to max value
			1.0; 
		true ->
			E
		end;
	(E<CutLo) ->
		CutLo;
	(E>CutHi) ->
		CutHi;
	true ->
		E
	end,
	[{X,Y,Z}|gen_vertices(W,H,Idx+1,CutLo,CutHi,T)].
	
%% Computes the vertice's index that define the top and bottom faces
%% Offset will be different of zero if faces are in bottom of surface 
gen_face(MaxZ,X,Z,Offset) ->
	V0=(X*MaxZ)+Z+Offset,
	V1=V0+MaxZ,
	V2=V1+1,
	V3=V0+1,
	build_face(V0,V1,V2,V3,Offset).

%% Computes the vertice's index that define the side faces
gen_face_1([],[]) -> [];
gen_face_1([V0|Tu],[V3|Td]) ->
	gen_face_2(V0,V3,Tu,Td).

gen_face_2(_,_,[],[]) -> [];
gen_face_2(V0,V3,[V1|Tu],[V2|Td]) ->
	[build_face(V0,V1,V2,V3,0)|gen_face_2(V1,V2,Tu,Td)].

build_face(V0,V1,V2,V3,0) -> [V0,V1,V2,V3];
build_face(V0,V1,V2,V3,_) -> [V0,V3,V2,V1].

%% Computes the vertice's index that define the edge
build_hard_edge([],[]) -> [];
build_hard_edge([Vu|Tu],Vld) ->
	Vld0=lists:append(Vld,[Vu]),
	Vl=lists:append(Tu,Vld0),
	build_hard_edge_1(Vu,Vl).

build_hard_edge_1(_,[]) -> [];
build_hard_edge_1(V0,[V1|T1]) ->
	[{V0,V1}|build_hard_edge_1(V1,T1)].


%% ==============================================
%% Copied from wings_image.erl    
%% begin    
draw_image(X, Y, W, H, TxId) ->
    Ua = 0, Ub = 1,
    Va = 0, Vb = 1,
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2i(Ua, Va),
    gl:vertex2i(X, Y),
    gl:texCoord2i(Ua, Vb),
    gl:vertex2i(X, Y+H),
    gl:texCoord2i(Ub, Vb),
    gl:vertex2i(X+W, Y+H),
    gl:texCoord2i(Ub, Va),
    gl:vertex2i(X+W, Y),
    gl:'end'().

maybe_scale(#e3d_image{width=W0,height=H0}=Image) ->
%%  case wings_gl:is_ext({2,0}, 'GL_ARB_texture_non_power_of_two') of
%%  Aarg ATI doesn't support ARB_NPOT textures, though it report GL_VER >= 2.0
    case maybe_exceds_opengl_caps(Image) of
        {error,_}=Error ->
            Error;
        Image1 ->
            #e3d_image{width=W1,height=H1}=Image1,
            case {W1,H1} of
                {W0,H0} ->
                    GL_ARB = wings_gl:is_ext('GL_ARB_texture_non_power_of_two');
                {_,_} -> GL_ARB = false
            end,
            case GL_ARB of
                true ->
                    Image;
                false ->
                    case {nearest_power_two(W1),nearest_power_two(H1)} of
                        {W1,H1} ->
                            Image1;
                        {W,H} ->
                            resize_image(Image1, W, H)
                    end
            end
    end.
    
maybe_exceds_opengl_caps(#e3d_image{width=W0,height=H0}=Image) ->
    MaxSize = hd(gl:getIntegerv(?GL_MAX_TEXTURE_SIZE)),
    case need_resize_image(W0, H0, MaxSize) of
        true ->
            ScaleFactor = case W0 > H0 of
                true ->
                    MaxSize/W0;
                false ->
                    MaxSize/H0
            end,
            W = trunc(W0*ScaleFactor),
            H = trunc(H0*ScaleFactor),
            resize_image(Image, W, H);
        false ->
            Image
    end.

resize_image(#e3d_image{width=W0,height=H0,bytes_pp=BytesPerPixel,
  image=Bits0,type=Type}=Image, W, H) ->
    Out = wings_io:get_buffer(BytesPerPixel*W*H, ?GL_UNSIGNED_BYTE),
    Format = texture_format(Image),
    GlErr =glu:scaleImage(Format, W0, H0, ?GL_UNSIGNED_BYTE,
        Bits0, W, H, ?GL_UNSIGNED_BYTE, Out),
    case GlErr of
        0 ->
            Bits = wings_io:get_bin(Out),
            Image#e3d_image{width=W,height=H,bytes_pp=BytesPerPixel,image=Bits,type=Type};
        _ ->
            {error,GlErr}
    end.

need_resize_image(W, H, Max) when W > Max; H > Max ->
    true;
need_resize_image(_, _, _) ->
    false.

texture_format(#e3d_image{type=r8g8b8}) -> ?GL_RGB;
texture_format(#e3d_image{type=r8g8b8a8}) -> ?GL_RGBA;
texture_format(#e3d_image{type=b8g8r8}) -> ?GL_BGR;
texture_format(#e3d_image{type=b8g8r8a8}) -> ?GL_BGRA;
texture_format(#e3d_image{type=g8}) -> ?GL_LUMINANCE;
texture_format(#e3d_image{type=a8}) -> ?GL_ALPHA.

internal_format(?GL_BGR) -> ?GL_RGB;
internal_format(?GL_BGRA) -> ?GL_RGBA;
internal_format(Else) -> Else.

%% Copied from wings_image.erl    
%% end	

%% ==============================================
%% copied from wpc_image.erl
%% begin
nearest_power_two(N) ->
    nearest_power_two(N, 1).

nearest_power_two(N, B) when N =< B -> B;
nearest_power_two(N, B) -> nearest_power_two(N, B bsl 1).
%% Copied from wpc_image.erl    
%% end	

%% want wings could export
%  wings_image.erl 
%  -> maybe_scale/1
%  -> resize_image/3
%  -> internal_format/1
%  -> texture_format/1
%  -> draw_image/5
%
% wpc_image.erl
%  -> zeroes/1
%  -> nearest_power_two/1

format_time(Ms) ->
    MsStr = integer_to_list(Ms rem 1000000) ++ "us",
    case Ms div 1000000 of
	0 ->
	    MsStr;
	Sec ->
	    integer_to_list(Sec) ++ "s " ++ MsStr
    end.

