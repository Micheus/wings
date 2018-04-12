%%  wpc_heightmap_surface.erl --
%%
%%     A surface creator by using a heiht map (gray scale or color: red->blue[raibow])
%%  Copyright (c) 2011,2012,2013 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_heightmap_surface).

-export([update_dlist/3, draw/4, get_data/3, merge_we/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-define(PREVIEW_SIZE, 128).
-define(START_DENSITY, 30.0).
-define(MAX_SOURCE_WEIGTH, 384*384).
-define(MAX_HUE_VALUE, 360.0).
-define(MAX_RGB_VALUE, 255).
-define(BOTTOM_Y, 0.0).
-define(HUE_PARAMS, {1.0,1.0}).
-define(RGB_PARAMS, {1.0,0.0}).
-define(CR_START, 3).
-define(CR_W, 200-1).  % 0-100 => x2 => (100x2)-1 = 199
-define(CR_H, 12).
-define(CR_WIDTH, ?CR_W+2*?CR_START).
-define(CR_HEIGHT, ?CR_H+2*?CR_START).
-define(SHAPE_NAME, "heightmap_surface").

-include("wings.hrl").
-include("e3d_image.hrl").

-export([init/0,menu/2,command/2]).

-record(cr,
    {xo=0,
     yo=0,
     sel_key=none,
     name=custom,
     color_keys=[{0,{0.0,0.0,0.0}},{?CR_W,{1.0,1.0,1.0}}]
    }).
-record(img_info,
    {work_image=none,
     prev_info=none,    % ImgId,W,H
     src_info=none,		% FileName,W,H,Bpp
     w=0,
     h=0,
     bpp=0,
     hmin=0,
     hmax=0
    }).

init() -> true.

menu({shape}, Menu) ->
    [[heightmap_heading(),separator],Menu];
menu(_,Menu) ->
    Menu.

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

command({shape,{bymicheus,heightmap_surface_editor}}, St) ->
    load_heightmap_image(St);
command({Name}, St) ->
    load_heightmap_image(Name,St);
command(_, _) ->
    next.

load_heightmap_image(_) ->
    ImgDir = wings_pref:get_value(image_repository, wings_pref:get_value(current_directory)),
    Ps = [{extensions,wpa:image_formats()},{directory,ImgDir}],
    wpa:import_filename(Ps, fun(N) -> {N} end).

load_heightmap_image(FileName,St) ->
    case read_image(FileName) of
    error -> keep;
    {SrcInfo,Image0} ->
        wings_wm:message(?__(1,"Resizing the image if needed...")),
        case maybe_resize(Image0) of
        {error,GlErr} ->
            wpa:error_msg(?__(2,"The image cannot be resized.\nFile:") ++ "\"~s\"\n" ++
                          ?__(3,"GLU Error:") ++ " ~p - ~s\n",
                  [FileName,GlErr, glu:errorString(GlErr)]);
        #e3d_image{width=W,height=H}=Image ->
            process_elevation(SrcInfo,Image,init_density(W,H)),
            make_heightmap(St)
        end
    end.

read_image(FileName0) ->
    Ps = [{filename,FileName0}],
    wings_wm:message(?__(1,"Reading image file...")),
    case wpa:image_read(Ps) of
    {error,Error} ->
        wpa:error_msg(?__(2,"Failed to load file:")++" ~s\n",
              [file:format_error(Error)]),
        error;
    Image ->
        wings_pref:set_value(image_repository, filename:dirname(FileName0)),
        #e3d_image{filename=FileName,width=W,height=H,bytes_pp=Bpp}=Image,
        SrcInfo={FileName,W,H,Bpp},
        wings_wm:message(?__(3,"Optimizing amount of color channel...")),
        {SrcInfo,maybe_new_chanel(Image)}
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

make_heightmap(St) ->
    {dialog,Dlg,Fun}=make_heightmap_dialog(St,[init]),
    wings_ask:dialog(?__(12,"Create a surface from height map"),{preview,Dlg},Fun).


make_heightmap_dialog(St,Arg) ->
    #st{shapes=Shapes,onext=Oid}=St,
    {dialog,
     heightmap_dialog(Arg),
     fun
        ({dialog_preview,Res}) ->
            #st{shapes=Shapes_Prw}=St_Prw=get_current_state(),
            St1=case gb_trees:lookup(Oid,Shapes_Prw) of
                {value, #we{pst=Pst_Prw}=We_Prw} ->
                    case gb_trees:lookup(?MODULE,Pst_Prw) of
                        {value, {_,Old_Res}} ->
                           % if no field has been changed we have nothing to do
                            case changed_field(Old_Res,Res) of
                                none -> St_Prw;
                                Field ->
                                    case Field of
                                        {Fld, _} when Fld=:=density; Fld=:=iw; Fld=:=ih ->
                                            process_density(Res);
                                        {cr, _} ->
                                            build_color_ramp(Res);
                                        _ -> ok
                                    end,
                                    Params=process_surface(Res),
                                    Pst_Prw0=gb_trees:enter(?MODULE,{Params,Res},Pst_Prw),
                                    St_Prw#st{shapes=gb_trees:enter(Oid,We_Prw#we{pst=Pst_Prw0},Shapes_Prw)}
                            end;
                        _ ->  % something is wrong if it got here
                            St_Prw
                    end;
                _ ->  % first time that preview is called
                    {Vs,Fs}=make_preview(),
                    Params=process_surface(Res),
                    #we{pst=Pst}=We=wings_we:build(Fs,Vs),
                    Pst0=gb_trees:enter(?MODULE,{Params,Res},Pst),
                    Name=?SHAPE_NAME++integer_to_list(Oid),
                    St_Prw=get_current_state(),
                    St_Prw#st{shapes=gb_trees:enter(Oid,We#we{name=Name, pst=Pst0},Shapes)}
            end,
            {preview,St1,St1};
        (cancel) ->
            cleanup(),
            St;
        (Res) ->
            ArgDict = dict:from_list(Res),
            case dict:fetch(rebuild,ArgDict) of
                true ->
                    {dialog,Dlg,Fun}=make_heightmap_dialog(St,Res),
                    {dialog,{preview,Dlg},Fun};
                _ ->
                    #st{shapes=Shapes_Prw}=get_current_state(),
                    St1=case gb_trees:lookup(Oid,Shapes_Prw) of
                        {value, #we{pst=Pst_Prw}} ->
                            {_, {Params,Old_Res}}=gb_trees:lookup(?MODULE,Pst_Prw),
                            Field=changed_field(Old_Res,Res),
                            if Field=:=none ->  % if preview in automatic mode - it's all ready
                                process_surface(Params,Res,St);
                            true ->
                                process_surface(Res,St)
                            end;
                        _ ->  % if preview in manual mode and use didn't use Update before hit Ok
                            process_surface(Res,St)
                    end,
                    cleanup(),
                    {commit,St,St1}
            end
      end}.


heightmap_dialog([init]) ->
    #img_info{prev_info=PreviewInfo,hmin=MinEm,hmax=MaxEm,w=W0,h=H0,bpp=Bpp0}=wings_pref:get_value(heightmap_img_info),
    {HiValue,{Hue_p1,Hue_p2}}=if Bpp0 > 2 ->
        {?MAX_HUE_VALUE,?HUE_PARAMS};
      true ->
        {?MAX_RGB_VALUE,?RGB_PARAMS}
    end,
    Cr=#cr{},
    build_color_ramp(Cr),
    TexSz=case nearest_power_two(max(W0,H0)) of
        TexSz0 when TexSz0 < 128 -> 128;
        TexSz0 -> TexSz0
    end,
    Density=init_density(W0,H0),
    Arg=[
    {prw,PreviewInfo},
    {mode,mesh}, % heightmap mode: mesh;block;
    {nb,false},
    {ar,true},
    {ws,10.0},
    {hs,H0*get_factor(W0,H0,10.0)},
    {es,1.0},
    {density,Density},
    {iw,trunc(W0*(Density/100.0))},
    {ih,trunc(H0*(Density/100.0))},
    {hi_value,HiValue},
    {hue_p1,Hue_p1},
    {hue_p2,Hue_p2},
    {bf,false},
    {sf,false},
    {he,true},
    {vc,false},
    {cr_preset,custom},
    {tcut_lo,MinEm*100.0},
    {tcut_hi,MaxEm*100.0},
    {scut_lo_c,?MAX_HUE_VALUE-MinEm*?MAX_HUE_VALUE},
    {scut_hi_c,?MAX_HUE_VALUE-MaxEm*?MAX_HUE_VALUE},
    {scut_lo_m,MinEm},
    {scut_hi_m,MaxEm},
    {mt,false},
    {texsz,TexSz},
    {cr,Cr}],
    heightmap_dialog(Arg);

heightmap_dialog(Arg) ->
    ArgDict = dict:from_list(Arg),
    PreviewInfo = dict:fetch(prw,ArgDict),
    Mode = dict:fetch(mode,ArgDict),
    Nb = dict:fetch(nb,ArgDict),
    Ar = dict:fetch(ar,ArgDict),
    Ws = dict:fetch(ws,ArgDict),
    Hs = dict:fetch(hs,ArgDict),
    Es = dict:fetch(es,ArgDict),
    Density = dict:fetch(density,ArgDict),
    IW = dict:fetch(iw,ArgDict),
    IH = dict:fetch(ih,ArgDict),
    HiValue = dict:fetch(hi_value,ArgDict),
    Hue_p1 = dict:fetch(hue_p1,ArgDict),
    Hue_p2 = dict:fetch(hue_p2,ArgDict),
    Bf = dict:fetch(bf,ArgDict),
    Sf = dict:fetch(sf,ArgDict),
    He = dict:fetch(he,ArgDict),
    Vc = dict:fetch(vc,ArgDict),
    Preset = dict:fetch(cr_preset,ArgDict),
    TextLo = dict:fetch(tcut_lo,ArgDict),
    TextHi = dict:fetch(tcut_hi,ArgDict),
    MinEc = dict:fetch(scut_lo_c,ArgDict),
    MaxEc = dict:fetch(scut_hi_c,ArgDict),
    MinEm = dict:fetch(scut_lo_m,ArgDict),
    MaxEm = dict:fetch(scut_hi_m,ArgDict),
    Mt = dict:fetch(mt,ArgDict),
    TexSz = dict:fetch(texsz,ArgDict),
    Cr = (dict:fetch(cr,ArgDict))#cr{name=Preset},

    LoSliderC={slider,[{color,{h,hue_p1,hue_p2}},{range,{0.0,?MAX_HUE_VALUE}},{key,scut_lo_c},{value,MinEc},{hook,fun dlg_default_hook/2}]},
    HiSliderC={slider,[{color,{h,hue_p1,hue_p2}},{range,{0.0,?MAX_HUE_VALUE}},{key,scut_hi_c},{value,MaxEc},{hook,fun dlg_default_hook/2}]},
    LoSliderM={slider,[{color,{v,hue_p1,hue_p2}},{range,{0.0,1.0}},{key,scut_lo_m},{value,MinEm},{hook,fun dlg_default_hook/2}]},
    HiSliderM={slider,[{color,{v,hue_p1,hue_p2}},{range,{0.0,1.0}},{key,scut_hi_m},{value,MaxEm},{hook,fun dlg_default_hook/2}]},
    [{vframe,[
        {hframe,[
           {vframe,[
              {dashboard,PreviewInfo,?PREVIEW_SIZE+2,?PREVIEW_SIZE+2,fun image_handle/2,
                [{info, ?__(26,"LL: Open the image dialog box")},{key,prw}]}]},
           {vframe,[
              {vframe,[
                {custom,?CHAR_WIDTH*18+2,?CHAR_HEIGHT*5+2,fun prw_info/5}]},
              {vframe,[
                {hframe,[
                  {label,?__(23,"Density:")},
                  {custom,?CHAR_WIDTH*15,?LINE_HEIGHT,fun density_preview/5}]},
                {hframe,[
                  {slider,[{range,{10.0,100.0}},{key,density},{value,Density},{hook,fun dlg_default_hook/2}]}]}
              ]}
           ]}],
           [
              {title,?__(13,"Image Info")} ]},
        {vframe,[
           {hframe,[
               {hradio, [{?__(31,"Mesh")++"  ",mesh},{?__(32,"Blocks")++"   ", block}], Mode, [{key, mode}, {hook,fun dlg_default_hook/2}]},
               {?__(33,"Build null block"),Nb,[{key,nb}]}
%               {?__(33,"Build null block"),Nb,[{key,nb},{hook,dlg_disable_hook(mode)}]}
               ]
           }],
        [
           {title,?__(34,"Contruction method")} ]},
        {vframe,[
           {hframe,[
               {?__(5,"Keep aspect ratio"),Ar,[{hook,fun dlg_default_hook/2},{key,ar}]} ]
           },
           {hframe,[
              {vframe,[
                {label,?__(6,"Width (X)")},
                {hframe,[
                {label,?__(7,"Length (Z)")}],[{hook,dlg_disable_hook(ar,true)}]},
                {label,?__(8,"Height (Y)")},
                {label,?__(15,"Lower cut (%)")},
                {label,?__(16,"High cut (%)")} ]
              },
              {vframe,[
                {vframe,[
                   {text,Ws,[{range,{0.01,infinity}},{width,6},{hook,fun dlg_default_hook/2},{key,ws}]},
                   {text,Hs,[{range,{0.01,infinity}},{width,6},{hook,dlg_disable_hook(ar,true)},{key,hs}]},
                   {text,Es,[{range,{0.01,infinity}},{width,6},{key,es}]} ]
                },
                {vframe,[
                   {value,false,[no_focus,{key,rebuild}]},
                   {value,IW,[no_focus,{key,iw}]},
                   {value,IH,[no_focus,{key,ih}]},
                   {value,HiValue,[no_focus,{key,hi_value}]},
                   {value,Hue_p1,[no_focus,{key,hue_p1}]},
                   {value,Hue_p2,[no_focus,{key,hue_p2}]},
                   {hframe, [
                       {text,TextLo,[{range,{0.0,100.0}},{width,5},{hook,fun dlg_default_hook/2},{key,tcut_lo}]},
                       {custom,?LINE_HEIGHT-2,?LINE_HEIGHT,fun lo_range_preview/5},
                       {hframe,[
                          LoSliderM],[{key,frm_lom},{hook,fun dlg_default_hook/2}]},
                       {hframe,[
                          LoSliderC],[{key,frm_loc},{hook,fun dlg_default_hook/2}]} ]
                   },
                   {hframe, [
                       {text,TextHi,[{range,{0.0,100.0}},{width,5},{hook,fun dlg_default_hook/2},{key,tcut_hi}]},
                       {custom,?LINE_HEIGHT-2,?LINE_HEIGHT,fun hi_range_preview/5},
                       {hframe,[
                          HiSliderM],[{key,frm_him},{hook,fun dlg_default_hook/2},layout]},
                       {hframe,[
                          HiSliderC],[{key,frm_hic},{hook,fun dlg_default_hook/2},layout]} ]
                   } ]
                } ]
              } ]
           },
           {hframe,[
              {?__(17,"Use minor height as ground value"),false,[{key,gv}]}]
           },
           separator,
           {hframe,[
              {?__(18,"Bottom faces"),Bf,[{key,bf}]},
              {?__(19,"Side faces"),Sf,[{key,sf}]},
              {?__(22,"Hard edges"),He,[{key,he}]}]},
           separator,
           {hframe,[
              {?__(24,"Set vertex color"),Vc,[{key,vc},{hook,fun dlg_default_hook/2}]}]},
           {vframe,[
             {hframe,[
                {label,?__(4,"Color ramp")},
                {menu,cr_build_menu(),Preset,[{key,cr_preset},{hook,fun dlg_default_hook/2},layout]},
                {hframe,[
                  {text,"",[{width,10},{key,sv_name}]},
                  {button,?__(27,"Save..."),done,[{key,sv},{hook,dlg_rebuild_hook()}]}
                  ],[{key,frm_sv},{hook,fun dlg_default_hook/2},layout]},
                {hframe,[
                  {button,?__(28,"Delete..."),done,[{key,del},{hook,dlg_rebuild_hook()}]}
                  ],[{key,frm_del},{hook,fun dlg_default_hook/2},layout]}],
               [{hook,dlg_disable_hook(vc,false)}]}]},
           {hframe,[
              {vframe,[
                {dashboard,Cr,?CR_WIDTH,?CR_HEIGHT,fun color_ramp_handle/2,
                    [{info, wings_msg:join([?__(29,"L: Set a color key"),?__(30,"L+[CTRL]: Moves the color key.")])},
                    {key,cr},layout]}]}],
             [{hook,dlg_disable_hook(vc,false)}]},
           {hframe,[
              {?__(25,"Make texture"),Mt,[{key,mt},{hook,dlg_disable_hook(vc,false)}]},
              {menu,gen_tx_sizes(),TexSz,[{key,texsz},{hook,dlg_disable_hook(mt,false)}]}],
             [{hook,dlg_disable_hook(vc,false)}]}
           ],
        [
           {title,?__(14,"Parameters")}]}]
    } ].

dlg_default_hook(Event, Params) ->
    case Event of
    update ->
        {Var,_,Val,Store}=Params,
        #img_info{w=W,h=H}=wings_pref:get_value(heightmap_img_info),
        Store1=case Var of
        vc ->
            %% It forces the color ramp be redrawn.
            %% I didn't discover why just this field doesn't produce the geom update.
            wings_wm:send(wings_wm:this(),redraw),
            gb_trees:update(Var,Val,Store);
        cr_preset ->
            wings_wm:send(wings_wm:this(),redraw),
            gb_trees:update(Var,Val,update_color_keys(Var,Val,Store));
        density ->
            update_range_density(Val,Store,W,H);
        ar ->
            gb_trees:update(Var,Val,calc_aspect_ratio(W,H,gb_trees:get(ws,Store),Val,Store));
        ws ->
            gb_trees:update(Var,Val,calc_aspect_ratio(W,H,Val,gb_trees:get(ar,Store),Store));
        tcut_lo ->
            update_range_slider(Var,Val,Store,gb_trees:get(hi_value,Store));
        tcut_hi ->
            update_range_slider(Var,Val,Store,gb_trees:get(hi_value,Store));
        scut_lo_m ->
            gb_trees:update(Var,Val,update_range_text(Var,Val,Store,mono));
        scut_hi_m ->
            gb_trees:update(Var,Val,update_range_text(Var,Val,Store,mono));
        scut_lo_c ->
            gb_trees:update(Var,Val,update_range_text(Var,Val,Store,color));
        scut_hi_c ->
            gb_trees:update(Var,Val,update_range_text(Var,Val,Store,color));
        _ ->
            gb_trees:update(Var,Val,Store)
        end,
        {store,Store1};
    is_minimized ->
        {Var,_I,Store}=Params,
        case Var of
            frm_sv -> gb_trees:get(cr_preset, Store) =/= custom;
            frm_del -> gb_trees:get(cr_preset, Store) =:= custom;
            frm_lom -> gb_trees:get(hi_value, Store) =:= ?MAX_HUE_VALUE;
            frm_loc -> gb_trees:get(hi_value, Store) =:= ?MAX_RGB_VALUE;
            frm_him -> gb_trees:get(hi_value, Store) =:= ?MAX_HUE_VALUE;
            frm_hic -> gb_trees:get(hi_value, Store) =:= ?MAX_RGB_VALUE;
            _ -> void
        end;
    _ ->
        void
    end.

%dlg_disable_hook(V) ->
%    fun (is_disabled, {_Var,_I,Store}) ->
%          case V of
%              mode -> gb_trees:get(mode, Store)=/=block;
%              _ ->
%                  false
%          end;
%      (_, _) ->
%          void
%    end.

dlg_disable_hook(V,Inverse) ->
    fun (is_disabled, {_Var,_I,Store}) ->
          case Inverse of
              true -> gb_trees:get(V, Store);
              _ ->
                  not gb_trees:get(V, Store)
          end;
      (_, _) ->
          void
    end.

dlg_rebuild_hook() ->
    fun(update, {Var,_,_,Store}) ->
        case Var of
            sv ->
                case cr_save(gb_trees:get(sv_name,Store),gb_trees:get(cr,Store)) of
                    none -> keep;
                    NewId ->
                        Store0=gb_trees:update(rebuild,true,Store),
                        {done,gb_trees:update(cr_preset,NewId,Store0)}
                end;
            del ->
                cr_delete(gb_trees:get(cr_preset,Store)),
                Store0=gb_trees:update(rebuild,true,Store),
                {done,gb_trees:update(cr_preset,custom,Store0)};
            _ ->
                keep
        end;
      (is_disable,_) -> false;
      (_, _) ->
          void
    end.

%%%
%%%  Color ramp menu and file management
%%%
cr_load() ->
    case file:consult(cr_file_name()) of
        {ok,CRs} -> CRs;
        _ -> []
    end.

cr_build_menu() ->
    Presets=cr_load(),
    lists:foldr(fun({Id,Name,_},Acc) ->
        Acc++[{Name,Id}]
    end, [], [cr_custom()]++Presets).

cr_save(Name0,#cr{color_keys=ColKeys}) ->
    {IdC,Custom,_}=cr_custom(),
    case string:strip(Name0) of
        "" -> none;
        Custom -> none;
        Name ->
            case list_to_atom(Name) of
                IdC -> none;
                NewId ->
                    Presets1=cr_load(),
                    Presets0=case lists:keytake(NewId,1,Presets1) of
                        {value,_,Lst} -> Lst;
                        _ -> Presets1
                    end,
                    New=[{NewId,Name,ColKeys}],
                    cr_save_1(lists:keysort(2, Presets0++New)),
                    NewId
            end
    end.

cr_save_1(Presets) ->
    Format=case os:type() of
        {win32,_} -> "~p.\r\n";
        _ -> "~p.\n"
    end,
    Str=lists:map(fun(S) ->
        io_lib:format(Format,[S])
    end,Presets),
    catch file:write_file(cr_file_name(), Str).

cr_delete(Id) ->
    Presets=cr_load(),
    case lists:keyfind(Id,1,Presets) of
        {Id,_,_} ->
            cr_save_1(lists:keydelete(Id,1,Presets));
        _ -> ok
    end,
    keep.

cr_read(Id) ->
    Presets=case file:consult(cr_file_name()) of
        {ok,CRs} -> CRs;
        _ -> [cr_custom()]
    end,
    case lists:keyfind(Id,1,Presets) of
        {Id,_,ColKeys} -> ColKeys;
        _ ->
            #cr{color_keys=ColKeys}=#cr{},
            ColKeys
    end.

cr_file_name() ->
    Dir=case new_pref_file() of
        none -> wings_prefs:get_value(current_directory);
        PrefFile ->	filename:dirname(PrefFile)
    end,
    Dir++"/color_ramp.txt".

cr_custom() ->
    #cr{color_keys=ColorKeys}=#cr{},
    {custom,?__(1,"Custom"),ColorKeys}.

%%%
%%%  functions for dialog support
%%%
getkpixel(W,H) ->
    Kp=W*H,
    case Kp>1000 of
    true -> integer_to_list(trunc(Kp/1000))++?__(1,"K pixels");
    false -> integer_to_list(Kp)++?__(2," pixels")
    end.

init_density(W,H) ->
    case trunc(?START_DENSITY/100.0 * min(W,H)) of  % check if is possible to init density to 50%
        MinDim when MinDim =< 3 -> 100.0;
        _  -> ?START_DENSITY
    end.

calc_aspect_ratio(W,H,Ws,Calc,Store) when Calc ->
    gb_trees:update(hs, H*(Ws/W), Store);
calc_aspect_ratio(_,_,_,_,Store) ->
    Store.

update_color_keys(_,Val,Store) ->
    ColKeys=cr_read(Val),
    Cr=gb_trees:get(cr,Store),
    gb_trees:update(cr,Cr#cr{name=Val,color_keys=ColKeys},Store).

update_range_density(Val,Store,W,H) ->
    Den=Val/100.0,
    MinDim=min(W,H),
    Val0=if trunc(Den * MinDim) =<3 ->
        3/MinDim*100.0;
    true -> Val
    end,
    Store1=gb_trees:update(iw,trunc(W*Val0/100),Store),
    Store0=gb_trees:update(ih,trunc(H*Val0/100),Store1),
    gb_trees:update(density,Val0,Store0).

update_range_slider(Var,Val,Store,MaxValue) ->
    Var0=if Var=:=tcut_lo ->
        case MaxValue of
            ?MAX_HUE_VALUE -> scut_lo_c;
            _ -> scut_lo_m
        end;
    true ->
        case MaxValue of
            ?MAX_HUE_VALUE -> scut_hi_c;
            _ -> scut_hi_m
        end
    end,
    if MaxValue=:=?MAX_HUE_VALUE ->  % hue slider
        Val0=(100.0-Val)/100.0*?MAX_HUE_VALUE;
    true ->  % val slider
        Val0=Val/100.0
    end,
    gb_trees:update(Var,Val,gb_trees:update(Var0,Val0,Store)).

update_range_text(Var,Val,Store,mono) ->
    Var0=if Var=:=scut_lo_m ->
        tcut_lo;
    true ->
        tcut_hi
    end,
    H=gb_trees:get(hue_p1,Store),
    S=gb_trees:get(hue_p2,Store),
    V=Val,
    Val0=Val*100.0,
    Color=wings_color:hsv_to_rgb(H,S,V),
    Store0=gb_trees:update(Var0,Val0,Store),
    gb_trees:update(Var,Color,Store0);
update_range_text(Var,Val,Store,color) ->
    Var0=if Var=:=scut_lo_c ->
        tcut_lo;
    true ->
        tcut_hi
    end,
    H=Val,
    S=gb_trees:get(hue_p1,Store),
    V=gb_trees:get(hue_p2,Store),
    Val0=((?MAX_HUE_VALUE-Val)/?MAX_HUE_VALUE)*100.0,
    Color=wings_color:hsv_to_rgb(H,S,V),
    Store0=gb_trees:update(Var0,Val0,Store),
    gb_trees:update(Var,Color,Store0).

lo_range_preview(X,Y,W,_,Store) ->
    case gb_trees:get(hi_value,Store) of
        ?MAX_HUE_VALUE -> range_preview(X,Y,W,Store,scut_lo_c);
        _ -> range_preview(X,Y,W,Store,scut_lo_m)
    end.

hi_range_preview(X,Y,W,_,Store) ->
    case gb_trees:get(hi_value,Store) of
        ?MAX_HUE_VALUE -> range_preview(X,Y,W,Store,scut_hi_c);
        _ -> range_preview(X,Y,W,Store,scut_hi_m)
    end.

range_preview(X,Y,W,Store,Type) ->
    Hue_p0=gb_trees:get(Type,Store),
    #img_info{bpp=Bpp}=wings_pref:get_value(heightmap_img_info),
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

density_preview(X,Y,_,_,Store) ->
    Iw=gb_trees:get(iw,Store),
    Ih=gb_trees:get(ih,Store),
    Dstr=io_lib:format("~wx~w (~s)", [Iw,Ih,getkpixel(Iw,Ih)]),
    gl:color3fv(wings_pref:get_value(dialog_text)),
    wings_io:text_at(X, Y+?LINE_HEIGHT-1, Dstr).

%% process the color_ramp field events
image_handle({doubleclick,_X,_Y}, {_Var,_I,_Val,_Store,_ClientRect}=_Params) ->
    Owner = wings_wm:this(),
    ImgDir = wings_pref:get_value(image_repository, wings_pref:get_value(current_directory)),
    Ps = [{extensions,wpa:image_formats()},{directory,ImgDir}],
    wpa:import_filename(Ps, fun(N) ->
        wings_wm:send(Owner,{load_image,{N}})
    end);
image_handle({load_image,{FileName}}, {_Var,_I,_Val,Store0,_ClientRect}=_Params) ->
    case read_image(FileName) of
    error -> keep;
    {SrcInfo,Image0} ->
        wings_wm:message(?__(1,"Resizing the image if needed...")),
        case maybe_resize(Image0) of
        {error,GlErr} ->
            wpa:error_msg(?__(2,"The image cannot be resized.\nFile:") ++ "\"~s\"\n" ++
                          ?__(3,"GLU Error:") ++ " ~p - ~s\n",
                  [FileName,GlErr, glu:errorString(GlErr)]);
        #e3d_image{width=W0,height=H0}=Image ->
            #img_info{prev_info={Id_Img_Preview,_,_}}=wings_pref:get_value(heightmap_img_info),
            wings_image:delete(Id_Img_Preview),
            Density=init_density(W0,H0),
            process_elevation(SrcInfo,Image,Density),
            #img_info{prev_info=PreviewInfo,w=W,h=H,hmin=MinE,hmax=MaxE,bpp=Bpp}=wings_pref:get_value(heightmap_img_info),
            {LoCut,HiCut}={?MAX_HUE_VALUE-MinE*?MAX_HUE_VALUE,?MAX_HUE_VALUE-MaxE*?MAX_HUE_VALUE},
            Fxz=get_factor(W,H,10.0),
            {HiValue,{Hue_p1,Hue_p2}}=if Bpp > 2 ->
                {?MAX_HUE_VALUE,?HUE_PARAMS};
              true ->
                {?MAX_RGB_VALUE,?RGB_PARAMS}
            end,
            TextLo=MinE*100.0,
            TextHi=MaxE*100.0,
            Fields=[{ws,W*Fxz},{hs,H*Fxz},{es,1.0},{prw,PreviewInfo},{scut_lo_c,LoCut},
                    {scut_hi_c,HiCut},{scut_lo_m,MinE},{scut_hi_m,MaxE},{hue_p1,Hue_p1},
                    {hue_p2,Hue_p2},{tcut_lo,TextLo},{tcut_hi,TextHi},{hi_value,HiValue}],
            Store=lists:foldr(fun({Var0,Value0}, Sto) ->
                gb_trees:update(Var0,Value0,Sto)
            end, update_range_density(Density,Store0,W,H), Fields),
            {layout,Store}
        end
    end;
image_handle(redraw, {Val,Rect,_Active,_DisEnabled}=_Params) ->
    img_preview(Rect,Val);
image_handle(_Ev,_) -> void.

img_preview({_,_,_,_},{none,_,_}) -> ok;
img_preview({X,Y,W,H},{Id_Img_Preview,W0,H0}) ->
    Dx=(W-W0)div 2,
    Dy=(H-H0)div 2,
    gl:enable(?GL_TEXTURE_2D),
    wings_image:draw_preview(X+Dx-1,Y-H+Dy+1,W0,H0,Id_Img_Preview),
    gl:disable(?GL_TEXTURE_2D).

prw_info(X,Y,_,_,_Store) ->
    #img_info{src_info=SrcInfo,w=W0,h=H0,bpp=Bpp,hmin=MinE,hmax=MaxE}=wings_pref:get_value(heightmap_img_info),
    {_,WSrc,HSrc,BppSrc}=SrcInfo,
    case {WSrc,HSrc} of
    {W0,H0} ->
        Wstr=" ",
        Hstr=" ",
        Bstr=" ";
    {_,_} ->
        Wstr=io_lib:format(" (~w)",[WSrc]),
        Hstr=io_lib:format(" (~w)",[HSrc]),
        Bstr=io_lib:format(" (~w)",[BppSrc])
    end,
    gl:color3fv(wings_pref:get_value(dialog_text)),
    wings_io:text_at(X, Y+?CHAR_HEIGHT, io_lib:format(?__(1,"Width:")++" ~w~s",[W0,Wstr])),
    wings_io:text_at(X, Y+?CHAR_HEIGHT*2, io_lib:format(?__(2,"Height:")++" ~w~s",[H0,Hstr])),
    wings_io:text_at(X, Y+?CHAR_HEIGHT*3, io_lib:format(?__(3,"Channels:")++" ~w~s",[Bpp,Bstr])),
    wings_io:text_at(X, Y+?CHAR_HEIGHT*4, io_lib:format(?__(20,"Min Value:")++" ~.3f%",[MinE*100.0])),
    wings_io:text_at(X, Y+?CHAR_HEIGHT*5, io_lib:format(?__(21,"Max Value:")++" ~.3f%",[MaxE*100.0])).

%% process the color_ramp field events
color_ramp_handle(#mousemotion{x=X,y=Y,state=Bst}, {Var,_I,Val,Store0,_ClientRect}=_Params) ->
    #cr{color_keys=ColKeys0,sel_key=SelKey0}=Val,
    case SelKey0 of
        none -> void;
        _ ->
            Key=X-?CR_START,
            case hit_color_ramp(X,Y) of
                true when 0 < Key, Key < ?CR_W ->
                    if (Bst band ?SDL_BUTTON_LMASK) =/= 0 ->
                        RGB=orddict:fetch(SelKey0,ColKeys0),
                        ColKeys1=orddict:erase(SelKey0,ColKeys0),
                        ColKeys=orddict:store(Key,RGB,ColKeys1),
                        Store=gb_trees:update(Var,Val#cr{name=custom,sel_key=Key,color_keys=ColKeys},Store0),
                        {store,gb_trees:update(cr_preset,custom,Store)};
                      true -> void
                    end;
                _ -> void
            end
    end;
color_ramp_handle(#mousebutton{x=X,y=Y,button=1,state=?SDL_PRESSED}, {Var,_I,Val,Store0,_ClientRect}=_Params) ->
    case hit_color_ramp(X,Y) of
    true ->
        Key=X-?CR_START,
        case wings_io:is_modkey_pressed(?CTRL_BITS) of
            false ->
                ColRamp0=wings_pref:get_value(heightmap_color_ramp),
                RGB0=array:get(Key,ColRamp0),
                Owner = wings_wm:this(),
                wings_color:choose(RGB0, fun(RGB) ->
                    wings_wm:send(Owner,{set_color,{Key,RGB}})
                end);
            _ when 0 < Key, Key < ?CR_W ->
                #cr{color_keys=ColKeys}=Val,
                SelKey=case orddict:find(Key, ColKeys) of
                    {ok,_} -> Key;
                    _ -> none
                end,
                Store=gb_trees:update(Var,Val#cr{name=custom,sel_key=SelKey},Store0),
                {store,gb_trees:update(cr_preset,custom,Store)};
            _ -> void
        end;
    _ -> void
    end;
color_ramp_handle(#mousebutton{button=1,state=?SDL_RELEASED}, {Var,_I,Val,Store,_ClientRect}=_Params) ->
    {store,gb_trees:update(Var,Val#cr{sel_key=none},Store)};
color_ramp_handle({set_color,{Idx,RGB}}, {Var,_I,Val,Store0,_ClientRect}=_Params) ->
    #cr{color_keys=ColKeys0}=Val,
    Val0=Val#cr{name=custom,color_keys=orddict:store(Idx,RGB,ColKeys0)},
    build_color_ramp(Val0),
    Store=gb_trees:update(Var,Val0,Store0),
    wings_wm:send(wings_wm:this(),redraw),
    {store,gb_trees:update(cr_preset,custom,Store)};
color_ramp_handle(redraw, {Val,Rect,_Active,_DisEnabled}=_Params) ->
    #cr{sel_key=SelKey,color_keys=ColKeys}=Val,
    {Xo,Yo,_,_}=Rect,
    Top=Yo-(?CR_START+?CR_H),
    Left=Xo+?CR_START,
    ColRamp=wings_pref:get_value(heightmap_color_ramp),
    array:foldr(fun(Key, Color, _Acc) ->
        gl:'begin'(?GL_LINES),
        gl:color3fv(Color),
        gl:vertex2f(Left+Key, Yo-?CR_START),
        gl:vertex2f(Left+Key, Top+1),
        gl:'end'(),
        case orddict:is_key(Key,ColKeys) of
            true ->
                gl:'begin'(?GL_LINES),
                gl:color3fv({0.0,0.0,0.0}),
                gl:vertex2f(Left+Key, Top),
                gl:vertex2f(Left+Key, Top-2),
                gl:'end'();
            _ -> ok
        end
    end, [], ColRamp),
    if SelKey=/=none ->
        gl:'begin'(?GL_LINE_LOOP),
        gl:color3fv({0.0,0.0,0.0}),
        gl:vertex2f(Left+SelKey-1, Yo-?CR_START+1),
        gl:vertex2f(Left+SelKey, Top),
        gl:vertex2f(Left+SelKey+1, Yo-?CR_START+1),
        gl:vertex2f(Left+SelKey+1, Top-1),
        gl:'end'(),
        gl:'begin'(?GL_LINES),
        gl:color3fv(orddict:fetch(SelKey,ColKeys)),
        gl:vertex2f(Left+SelKey-1, Top-1),
        gl:vertex2f(Left+SelKey, Yo-?CR_START),
        gl:'end'();
    true -> void
    end;
color_ramp_handle(_,_) ->
    void.

hit_color_ramp(X,Y) when (?CR_START) =< X,
                        X =< (?CR_START+?CR_W),
                        (?CR_START) =< Y,
                        Y =< (?CR_START+?CR_H) -> true;
hit_color_ramp(_,_) -> false.

build_color_ramp(#cr{color_keys=ColKeys1}) ->
    [Col0|Cols]=orddict:to_list(ColKeys1),
    {_,KeyCols0}=lists:foldl(fun({Key1,_C1}=Item1, {{Key0,_C0}=Item0,Acc}) ->
        Acc0=[build_color_ramp_1(Key,Item0,Item1) || Key <- lists:seq(Key0,Key1)],
        {Item1,Acc++Acc0}
    end, {Col0,[]}, Cols),
    KeyCols=lists:foldl(fun({Key,Value}, Acc) ->
        orddict:store(Key,Value,Acc)
    end, orddict:new(), KeyCols0),
    wings_pref:set_value(heightmap_color_ramp, array:from_orddict(KeyCols));
build_color_ramp(Arg) ->
    ArgDict = dict:from_list(Arg),
    build_color_ramp(dict:fetch(cr,ArgDict)).

build_color_ramp_1(Key, {Key0,C0}, {Key1,C1}) ->
    Range=Key1-Key0,
    Cr=e3d_vec:sub(C1,C0),
    Rf=(Key-Key0)/Range,
    Cr0=e3d_vec:mul(Cr,Rf),
    {Key,e3d_vec:add(C0,Cr0)}.

create_img_preview(Image) ->
    case maybe_scale(Image) of
    {error,_}=Error ->
        Error;
    Image0 ->
        #e3d_image{width=W0,height=H0}=Image1=maybe_resize(Image0,?PREVIEW_SIZE),
        Id=case wings_image:new_hidden("heightmap_preview",Image1) of
          {error,_} -> none;
          Id0 -> Id0
        end,
        {Id,W0,H0}
    end.

check_is_grayscale(#e3d_image{width=W,height=H,bytes_pp=Bpp,image=Pixels}=Image)->
    case Bpp > 2 of
    true ->
        Bpp0=Bpp-3,
        Ca = [ R || << R:1/binary, G:1/binary, B:1/binary, _:Bpp0/binary >> <= Pixels, R=:=G, G=:=B ],
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

%%%
%%%  Process the global [temporary] data
%%%

%% it creates a very small cube that will carry "pst" information
make_preview() ->
    VL=[{1.0,1.0,1.0} || _ <- lists:seq(1,2), _ <- lists:seq(1,2)],
    {VsTop,VsBot}=create_normalized_vertices(mesh,2,2,0.0,1.0,false,false,false,VL),
    Nv0=[scale_vertices(Vertice,0.00005,0.00005,0.0001,false) || Vertice <- VsTop++VsBot],
    {Fs,_}=create_faces(2,2,false,false),
    {Nv0,Fs}.

%% removes the global [temporary] data from the preference settings
cleanup() ->
    #img_info{prev_info={Id_Img_Preview,_,_}}=wings_pref:get_value(heightmap_img_info),
    wings_pref:delete_value(heightmap_elevation),
    wings_pref:delete_value(heightmap_color_ramp),
    wings_pref:delete_value(heightmap_img_info),
    wings_image:delete(Id_Img_Preview).

process_elevation(SrcInfo,#e3d_image{width=W0,height=H0}=Image0,Density) ->
    PreviewInfo=create_img_preview(Image0),
    Image=case Density of
        ?START_DENSITY -> resize_image(Image0,trunc(W0*?START_DENSITY/100.0),trunc(H0*?START_DENSITY/100.0));
        _ -> Image0
    end,
    #e3d_image{image=Pixels,width=W,height=H,bytes_pp=Bpp}=check_is_grayscale(Image),
    Len=Bpp*W*H, % required because resize sometimes pad the buffer with zeros
    Els=pixel_to_height(<<Pixels:Len/binary>>, Bpp),
    MinE=lists:min(Els),
    MaxE=lists:max(Els),
    ImgInfo=#img_info{work_image=Image0,prev_info=PreviewInfo,src_info=SrcInfo,hmin=MinE,hmax=MaxE,w=W0,h=H0,bpp=Bpp},
    wings_pref:set_value(heightmap_elevation, {W,H,Els}),
    wings_pref:set_value(heightmap_img_info, ImgInfo).

process_density(Arg) ->
    ArgDict = dict:from_list(Arg),
    Iw = dict:fetch(iw,ArgDict),
    Ih = dict:fetch(ih,ArgDict),
    #img_info{work_image=Image}=wings_pref:get_value(heightmap_img_info),
    Image0=resize_image(Image,Iw,Ih),
    #e3d_image{image=Pixels0,bytes_pp=Bpp0,width=W0,height=H0}=check_is_grayscale(Image0),
    Len=Bpp0*W0*H0, % required because resize sometimes pad the buffer with zeros
    Els=pixel_to_height(<<Pixels0:Len/binary>>, Bpp0),
    wings_pref:set_value(heightmap_elevation, {W0,H0,Els}).

%%%
%%%  Computing the data for build the mesh
%%%

%%  process_surface/1 computes the data for preview
process_surface(Arg) ->
    ArgDict = dict:from_list(Arg),
    ?SLOW(process_surface(dict:fetch(mode,ArgDict),Arg)).
process_surface(mesh, Arg) ->
    ArgDict = dict:from_list(Arg),
    Wm = dict:fetch(ws,ArgDict),
    Hm = dict:fetch(hs,ArgDict),
    Em = dict:fetch(es,ArgDict),
    CLo = dict:fetch(tcut_lo,ArgDict),
    CHi = dict:fetch(tcut_hi,ArgDict),
    GroundValue = dict:fetch(gv,ArgDict),
    BottomFace = dict:fetch(bf,ArgDict),
    VertexColor = dict:fetch(vc,ArgDict),
    SideFace = dict:fetch(sf,ArgDict),
    HardEdges = dict:fetch(he,ArgDict),

    wings_wm:message(?__(1,"Computing mesh... (this can take a few minutes)")),
    {W,H,Els}=wings_pref:get_value(heightmap_elevation),
    {VsTop,VsBot}=create_normalized_vertices(mesh,W,H,CLo/100.0,CHi/100.0,GroundValue,BottomFace,SideFace,Els),
    Vs=[scale_vertices(Vertice,Wm,Em,Hm,VertexColor) || Vertice <- VsTop++VsBot],
    {Fs,He0}=create_faces(W,H,BottomFace,SideFace),
    He=if HardEdges=:=true -> He0;
        true -> []
    end,
    {Vs,Fs,He,VertexColor};
process_surface(block, Arg) ->
    ArgDict = dict:from_list(Arg),
    Wm = dict:fetch(ws,ArgDict),
    Hm = dict:fetch(hs,ArgDict),
    Em = dict:fetch(es,ArgDict),
    Nb = dict:fetch(nb,ArgDict),
    CLo = dict:fetch(tcut_lo,ArgDict),
    CHi = dict:fetch(tcut_hi,ArgDict),
    GroundValue = dict:fetch(gv,ArgDict),
    VertexColor = dict:fetch(vc,ArgDict),

    wings_wm:message(?__(2,"Computing blocks... (this can take a several minutes)")),
    {W,H,Els}=wings_pref:get_value(heightmap_elevation),
    Bw=(Wm/W)/2,
    Bh=(Hm/H)/2,
    {VsTop1,_}=create_normalized_vertices(block,W,H,CLo/100.0,CHi/100.0,GroundValue,false,false,Els),
    if Nb=/=true ->
        VsTop=[scale_vertices(V,Wm,Em,Hm,VertexColor) || {_,Y,_}=V <- VsTop1, Y > 0.0],
        {_,Blks}=lists:foldr(fun(Vi, {Idx, BlAcc0}) ->
            case Vi of
                {_, Y0, _} -> Y=Y0;
                {{_, Y0, _},_} -> Y=Y0
            end,
            if (Y > 0.0) -> {Idx+1,BlAcc0++[Idx]};
              true -> {Idx,BlAcc0}
            end
        end, {0,[]},VsTop),
        Fs=create_blocks(Blks);
      true ->
        VsTop=[scale_vertices(V,Wm,Em,Hm,VertexColor) || V <- VsTop1],
        Fs=create_blocks(W,H)
    end,
    VsBlks=[block_vertices(V,Bw,Bh) || V <- VsTop],
    {lists:flatten(VsBlks),Fs,[],VertexColor};
%%  process_surface/2 computes the data for build the final mesh
process_surface(Arg, St) ->
    Params=process_surface(Arg),
    create_surface(Params,Arg,St).
%%  process_surface/3 computes the data for build the final mesh by using pre-calculated data (optimization)
process_surface(Params, Arg, St) ->
    ArgDict=dict:from_list(Arg),
    Mode=dict:fetch(mode,ArgDict),
    W=dict:fetch(iw,ArgDict),
    H=dict:fetch(ih,ArgDict),
    Tx=dict:fetch(mt,ArgDict),
    TxSize=dict:fetch(texsz,ArgDict),

    {Vs,Fs,He,Col}=Params,
    Op=case Col of
    true ->
        if (Mode=:=mesh)and(Tx=:=true) -> {uv,TxSize};
          true -> Col
        end;
    _ -> Col
    end,
    create_surface({W,H},{Vs,Fs,He,Op},St).

%% create plain wings3d object - no properties is set
create_surface(_,{Vs,Fs,He,false},St0) ->
    wings_wm:message(?__(1,"Building Wings object... (this can take a few minutes)")),
    {_,St}=?SLOW(build_shape(?SHAPE_NAME,Fs,Vs,He,St0)),
    St;
%% create wings3d object and set the color vertex property
create_surface(_,{Vs0,Fs,He,true},St0) ->
    wings_wm:message(?__(1,"Building Wings object... (this can take a few minutes)")),
    {Vs,Cs}=lists:foldl(fun({V,H}, {VAcc,CAcc}) ->
        C=trunc(H* (?CR_W*1.0)),
        {VAcc++[V], CAcc++[C]}
    end, {[],[]}, Vs0),
    {Id,#st{shapes=Shapes}=St}=?SLOW(build_shape(?SHAPE_NAME,Fs,Vs,He,St0)),
    wings_wm:message(?__(2,"Setting vertex colors...")),
    We0=?SLOW(set_vertex_color(Cs, gb_trees:get(Id,Shapes))),
    St#st{shapes=gb_trees:update(Id,We0,Shapes)};
%% create wings3d object, material, texture and set them to it
create_surface(DstDim,{Vs0,Fs,He,{uv,TxSize}},St0) ->
    wings_wm:message(?__(1,"Building Wings object... (this can take a few minutes)")),
    {Vs,_}=lists:foldl(fun({V,H}, {VAcc,CAcc}) ->
        C=trunc(H* (?CR_W*1.0)),
        {VAcc++[V], CAcc++[C]}
    end, {[],[]}, Vs0),
    {Id,#st{shapes=Shapes,mat=Mtab0}=St}=?SLOW(build_shape(?SHAPE_NAME,Fs,Vs,He,St0)),
    We0=gb_trees:get(Id,Shapes),
    {We,Mtab}=?SLOW(set_texture(We0,DstDim,TxSize,Mtab0)),
    St#st{shapes=gb_trees:update(Id,We,Shapes),mat=Mtab}.

set_vertex_color(Cs0, #we{es=Edges}=We) ->
    ColRamp=wings_pref:get_value(heightmap_color_ramp),
    Cs=list_to_tuple(Cs0),
    {Lva,Rva}=array:sparse_foldl(fun(Idx, #edge{vs=Vs,ve=Ve}, {LAcc,RAcc}) ->
        Ids=element(Vs+1,Cs),
        Ide=element(Ve+1,Cs),
        ColS=array:get(Ids,ColRamp),
        ColE=array:get(Ide,ColRamp),
        {array:set(Idx,[ColS|none],LAcc),
         array:set(Idx,[ColE|none],RAcc)}
    end, {array:new({default,none}),array:new({default,none})}, Edges),
    We#we{lv=Lva,rv=Rva}.

set_texture(#we{name=Name0}=We0,{W,H}=DstDim,TxSize,Mtab0) ->
    wings_wm:message(?__(1,"Setting texture: creating and painting... (this can take a few minutes)")),
    Name=Name0++"_auv",
    MatName=list_to_atom(Name),
    {Diffmap,MaxUV}=create_texture(Name,TxSize),

    Mat0=create_material(),
    Maps0 = proplists:get_value(maps, Mat0),
    Maps=proplists:delete(diffuse, Maps0)++[{diffuse,Diffmap}],
    Mat=proplists:delete(maps, Mat0)++[{maps,Maps}],
    Mtab=gb_trees:insert(MatName, Mat, Mtab0),

    FaceMat=[{F,MatName} || F <- lists:seq(0,(W-1)*(H-1)-1)],
    We1=wings_facemat:assign(FaceMat,We0),
    wings_wm:message(?__(2,"Setting texture: uv mapping... (this can take a few minutes)")),
    We=set_vertex_uv(MaxUV,DstDim,We1),
    {We,Mtab}.

set_vertex_uv({MaxU,MaxV},{W0,H0},#we{es=Edges}=We) ->
    H=H0-1,
    W=W0-1,
    TopFaces=W*H,
    {Lva,Rva}=array:sparse_foldl(fun(Idx, #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf}, {LAcc0,RAcc0}) ->
        case min(Lf,Rf) of
            F when F < TopFaces ->
                RAcc=if Rf<TopFaces ->
                    Ruv={((Ve rem W0)/W)*MaxU,((H-(Ve div W0))/H)*MaxV},
                    array:set(Idx,[none|Ruv],RAcc0);
                true -> RAcc0
                end,
                LAcc=if Lf<TopFaces ->
                    Luv={((Vs rem W0)/W)*MaxU,((H-(Vs div W0))/H)*MaxV},
                    array:set(Idx,[none|Luv],LAcc0);
                true -> LAcc0
                end,
                {LAcc,RAcc};
            _ ->
                {array:set(Idx,none,LAcc0),
                 array:set(Idx,none,RAcc0)}
        end
    end, {array:new({default,none}),array:new({default,none})}, Edges),
    We#we{lv=Lva,rv=Rva}.

create_texture(Name,TxSize) ->
    #img_info{src_info=SrcInfo,work_image=#e3d_image{width=W,height=H}=Image0,bpp=Bpp}=wings_pref:get_value(heightmap_img_info),
    {FileMame,WSrc,HSrc,_BppSrc}=SrcInfo,
    Image=case nearest_power_two(max(W,H)) of
        TxSize -> Image0;  % reuse the work image
        _ ->
            case read_image(FileMame) of  % load original image
                error -> Image0;
                {_,Image1} ->
                    Fct=TxSize/max(WSrc,HSrc),
                    case resize_image(Image1,trunc(WSrc*Fct),trunc(HSrc*Fct)) of
                        {error,_} -> Image0;
                        Image2 -> Image2
                    end
            end
    end,
    create_texture_1(Image,Name,Bpp=<2).

create_texture_1(#e3d_image{width=W,height=H}=Image0,Name,Gray) ->
    #e3d_image{image=Pixels}=Image1=e3d_image:convert(Image0, r8g8b8, 1, lower_left),
    ColRamp=wings_pref:get_value(heightmap_color_ramp),
    Calc_Color=fun(Col,Range) ->
        Idx=Col*Range,
        Idx0=trunc(Idx),
        Idx1=min(Idx0+1,array:size(ColRamp)-1),
        Col0=array:get(Idx0,ColRamp),
        Col1=array:get(Idx1,ColRamp),
        calc_color_1(Idx-Idx0,Col0,Col1)
    end,
    HUE_Range=(?CR_W*1.0)/?MAX_HUE_VALUE,
    Gray_Range=(?CR_W*1.0)/?MAX_RGB_VALUE,
    Fun_Gray=fun(R2) ->
            [R0]=binary_to_list(R2),
            Col=Calc_Color(R0,Gray_Range),
            {R1,G1,B1}=e3d_vec:mul(Col,255.0),
            list_to_binary([trunc(R1),trunc(G1),trunc(B1)])
        end,
    Fun_HUE=fun(R2,G2,B2) ->
            [R0]=binary_to_list(R2),
            [G0]=binary_to_list(G2),
            [B0]=binary_to_list(B2),
            Col=Calc_Color(rgb_to_height(R0,G0,B0),HUE_Range),
            {R1,G1,B1}=e3d_vec:mul(Col,255.0),
            list_to_binary([trunc(R1),trunc(G1),trunc(B1)])
        end,

    NewPix=if Gray=:=true ->
        [Fun_Gray(R0) || <<R0:1/binary,_G0:1/binary,_B0:1/binary>> <= Pixels];
    true ->
        [Fun_HUE(R0,G0,B0) || <<R0:1/binary,G0:1/binary,B0:1/binary>> <= Pixels]
    end,
    #e3d_image{width=W0,height=H0}=Image=pad_image(Image1#e3d_image{image=list_to_binary(NewPix)}),
    MaxU = W/W0,
    MaxV = H/H0,
    %% We must make sure that the new image is saved - so force
    %% it to be internal by setting filename to 'none'.
    {wings_image:new(Name,Image#e3d_image{filename=none}),{MaxU,MaxV}}.

calc_color_1(Pos,C0,C1) ->
    Cr=e3d_vec:sub(C1,C0),
    Cr0=e3d_vec:mul(Cr,Pos),
    e3d_vec:add(C0,Cr0).

create_material() ->
    Mat=wings_material:default(),
    Props0=gb_trees:get(default,Mat),
    OpenGL0 = proplists:delete(specular, proplists:get_value(opengl, Props0)),
    OpenGL=proplists:delete(vertex_colors, OpenGL0)++[{specular,{0.1,0.1,0.1,1.0}},{vertex_colors,ignore}],
    [{opengl,OpenGL}|lists:keydelete(opengl, 1, Props0)].

%%%
%%% Computing the normalized data for each vertice that defines the object
%%%
create_normalized_vertices(Mode,W,H,CLo,CHi,GroundValue,BottomFace,SideFace,Els) ->
    W0=(W-1),
    H0=(H-1),
	Dx=W0/2.0,
	Dz=H0/2.0,
    if GroundValue=:=true -> Dy=CLo;
      true -> Dy=0.0
    end,
    %  Computing the normalized heigth for each vertice that defines the grid
    ?SLOW(VeticeList=gen_vertices(W,H,0,CLo,CHi,Els)),
    %  Normalizing the X and Z coordenates that defines the grid
    VsTop=case Mode of
        mesh ->
            [{(X-Dx)/W0,Y-Dy,(Z-Dz)/H0} || {X,Y,Z} <- VeticeList];
        _ ->
            [{(X-Dx)/W,Y-Dy,(Z-Dz)/H} || {X,Y,Z} <- VeticeList]
    end,
    %  Computing the vertices for the Bottom
    if BottomFace=:=true ->
        VsBot = [{X,?BOTTOM_Y,Z} || {X,_,Z} <- VsTop];
    true ->
        if SideFace=:=true ->
            VNd = [{(X-Dx)/W0,?BOTTOM_Y,(0.0-Dz)/H0}		|| X <-lists:seq(0,W-1)],
            VEd = [{(W-1-Dx)/W0,?BOTTOM_Y,(Z-Dz)/H0}		|| Z <-lists:seq(1,H-2)],
            VSd = [{(W-X-Dx)/W0,?BOTTOM_Y,(0.0+Dz)/H0}	|| X <-lists:seq(1,W)],
            VWd = [{(0.0-Dx)/W0,?BOTTOM_Y,(H-1-Z -Dz)/H0}	|| Z <-lists:seq(1,H-2)],
            VB1 = lists:append(VNd,VEd),
            VB2 = lists:append(VSd,VWd),
            VsBot = lists:append(VB1,VB2);
        true ->
            VNd = [{-Dx/W0,?BOTTOM_Y,-Dz/H0},{+Dx/W0,?BOTTOM_Y,-Dz/H0}],
            VSd = [{+Dx/W0,?BOTTOM_Y,+Dz/H0},{-Dx/W0,?BOTTOM_Y,+Dz/H0}],
            VsBot = lists:append(VNd,VSd)
        end
    end,
    {VsTop,VsBot}.

%%%
%%%  Computing the face indexes for object's faces
%%%
create_faces(W,H,BottomFace,SideFace) ->
    %  Defining the faces of the surface's top
    Fs0 = [gen_face(W,X,Z,0) ||
            Z <- lists:seq(0,W-2),
            X <- lists:seq(0,H-2)],
    %  Defining the faces of the surface's bottom
    VsDx=W*H,
    if BottomFace=:=true ->
        Fs1 = [gen_face(W,X,Z,VsDx) ||
                Z <- lists:seq(0,W-2),
                X <- lists:seq(0,H-2)];
    true ->
        if SideFace=:=true ->
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
    if (BottomFace=:=true) andalso (SideFace=:=true) ->
        FNd = [VsDx+I		|| I<-lists:seq(0,W-1)],		% North down
        FSd = [VsDx*2-1-I	|| I<-lists:seq(0,W-1)],		% South down
        FWd = [VsDx*2-W-I	|| I<-lists:seq(0,(H-1)*W,W)],	% West down
        FEd = [VsDx+W-1+I	|| I<-lists:seq(0,(H-1)*W,W)];	% Est down
    true ->
        if SideFace=:=true ->
            FNd = [VsDx+I			|| I<-lists:seq(0,W-1)],
            FSd = [VsDx+W+H-2+I		|| I<-lists:seq(0,W-1)],
            FWd = lists:append([VsDx+W*2+H-3+I	|| I<-lists:seq(0,H-2)],[VsDx]),
            FEd = [VsDx+W-1+I		|| I<-lists:seq(0,H-1)];
        true ->
            if BottomFace=:=true ->
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
    if SideFace=:=true ->
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
    Fsa = lists:append(Fs1,Fs2),
    Fsb = lists:append(Fs3,Fs4),
    Fsc = lists:append(Fs5,Fsa),
    Fsd = lists:append(Fsb,Fsc),
    Fs=lists:append(Fs0,Fsd),   % the top faces will be at the begin of the list
    %  Defining the hard edges of the surface
    if (SideFace=:=true) ->
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

%% Scale the normatized heightmap data
scale_vertices({_,Y,_}=Cood,Sx,_,Sz,VertexColor) when Y==?BOTTOM_Y ->
    scale_vertices_0(Cood,Sx,1.0,Sz,VertexColor);
scale_vertices({_,_,_}=Cood,Sx,Sy,Sz,VertexColor) ->
    scale_vertices_0(Cood,Sx,Sy,Sz,VertexColor).

scale_vertices_0({Xn,Yn,Zn},Sx,Sy,Sz,false) ->
    {Xn*Sx,Yn*Sy,Zn*Sz};
scale_vertices_0({Xn,Yn,Zn},Sx,Sy,Sz,true) ->
    {{Xn*Sx,Yn*Sy,Zn*Sz},Yn}.

%% Set the elevation value for each vertex of the object top (E values). W is the width of the image
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
    [{X*1.0,Y*1.0,Z*1.0}|gen_vertices(W,H,Idx+1,CutLo,CutHi,T)].

%%%
%%%  Computing the face indexes for object's faces
%%%
create_blocks(W,H) ->
    create_blocks_0(W*H-1,block_vs_idx(),[]).
create_blocks(Blks) ->
    create_blocks_1(Blks,block_vs_idx(),[]).

create_blocks_0(0, Blk0, Acc0) -> Blk0++Acc0;
create_blocks_0(Idx0, Blk0, Acc0) ->
    Blk=block_vs_idx(Idx0*8, Blk0),
    create_blocks_0(Idx0-1,Blk0,Blk++Acc0).

create_blocks_1([], _, Acc0) -> Acc0;
create_blocks_1([Idx|Blks], Blk0, Acc0) ->
    Blk=block_vs_idx(Idx*8, Blk0),
    create_blocks_1(Blks, Blk0, Blk++Acc0).

%% Computes the vertice's values for the top and bottom faces of the blocks
block_vertices({{Vx,Vy,Vz},Yn}, Wdx, Hdx) ->
    Vs=[{{Vx-Wdx,Vy,Vz-Hdx},Yn}, {{Vx-Wdx,Vy,Vz+Hdx},Yn}, {{Vx+Wdx,Vy,Vz+Hdx},Yn}, {{Vx+Wdx,Vy,Vz-Hdx},Yn}],
    Vs ++[{{Vx0,0.0,Vz0},Yn0} || {{Vx0,_,Vz0},Yn0} <- lists:reverse(Vs)];
block_vertices({Vx,Vy,Vz}, Wdx, Hdx) ->
    Vs=[{Vx-Wdx,Vy,Vz-Hdx}, {Vx-Wdx,Vy,Vz+Hdx}, {Vx+Wdx,Vy,Vz+Hdx}, {Vx+Wdx,Vy,Vz-Hdx}],
    Vs ++[{Vx0,0.0,Vz0} || {Vx0,_,Vz0} <- lists:reverse(Vs)].

block_vs_idx() -> [[0,1,2,3],[4,5,6,7],[0,7,6,1],[1,6,5,2],[2,5,4,3],[3,4,7,0]].
block_vs_idx(Idx, Blk) ->
    [[V0+Idx,V1+Idx,V2+Idx,V3+Idx] || [V0,V1,V2,V3] <- Blk].

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

%% Computes the vertex indexes that define the hard edges
build_hard_edge([],[]) -> [];
build_hard_edge([Vu|Tu],Vld) ->
    Vld0=lists:append(Vld,[Vu]),
    Vl=lists:append(Tu,Vld0),
    build_hard_edge_1(Vu,Vl).

build_hard_edge_1(_,[]) -> [];
build_hard_edge_1(V0,[V1|T1]) ->
    [{V0,V1}|build_hard_edge_1(V1,T1)].

%%%
%%%  functions of general purpose
%%%

%% Build the final wings object #we{}
build_shape(Prefix, Fs, Vs, He, #st{onext=Oid}=St) ->
    We=case He of
    [] ->
        wings_we:build(Fs,Vs);
    He0 ->
        wings_we_build:we(Fs,Vs,He0)
    end,
    Name = Prefix++integer_to_list(Oid),
    {Oid,wings_shape:new(Name, We, St)}.

get_factor(W0,H0,Dmax) when W0>H0 ->
    Dmax/W0;
get_factor(_,H0,Dmax) ->
    Dmax/H0.

maybe_resize(#e3d_image{width=W,height=H}=Image) ->
%% resize by area - max is about 147Kpx
    Weigth=(W*H),
    if Weigth > ?MAX_SOURCE_WEIGTH ->
        Fct=math:sqrt(Weigth/(?MAX_SOURCE_WEIGTH)),
        resize_image(Image,round(W/Fct),round(H/Fct));
    true ->
        Image
    end.
maybe_resize(#e3d_image{width=W,height=H}=Image, MaxDim) ->
%% resize by max dimension required
    Max=max(W,H),
    if Max>MaxDim ->
        Fct=MaxDim/Max,
        resize_image(Image,trunc(W*Fct),trunc(H*Fct));
    true ->
        Image
    end.

%% as preview is called for each field that has been changed this function will always
%% return only one field/value. Also, if there was no field value changed it returns none.
%% it can happen if the field just got the focus - preview will fire an event
changed_field(Old_Res, Res) ->
    Arg0 = dict:from_list(Old_Res),
    Arg1 = dict:from_list(Res),
    dict:fold(fun(Key, Value1, Acc) ->
        Value0=dict:fetch(Key,Arg0),
        case Value0=/=Value1 of
            true -> {Key, Value1};
            _ -> Acc
        end
    end, none, Arg1).

get_current_state() ->
    DispLists = wings_wm:get_prop(geom, display_lists),
    get({wm_current_state,DispLists}).

%%%
%%%  Processing the binary data from image
%%%

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


%%% ==============================================
%%% Copied from wings_image.erl
%%% begin
maybe_scale(#e3d_image{width=W0,height=H0}=Image) ->
%%  case wings_gl:is_ext({2,0}, 'GL_ARB_texture_non_power_of_two') of
%%  Aarg ATI doesn't support ARB_NPOT textures, though it report GL_VER >= 2.0
    case wings_image:maybe_exceds_opengl_caps(Image) of
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

texture_format(#e3d_image{type=r8g8b8}) -> ?GL_RGB;
texture_format(#e3d_image{type=r8g8b8a8}) -> ?GL_RGBA;
texture_format(#e3d_image{type=b8g8r8}) -> ?GL_BGR;
texture_format(#e3d_image{type=b8g8r8a8}) -> ?GL_BGRA;
texture_format(#e3d_image{type=g8}) -> ?GL_LUMINANCE;
texture_format(#e3d_image{type=a8}) -> ?GL_ALPHA.
%%% Copied from wings_image.erl
%%% end

%%% ==============================================
%%% copied from wpc_image.erl
%%% begin
pad_image(#e3d_image{width=W0,image=Pixels0,bytes_pp=PP}=Image) ->
    case nearest_power_two(W0) of
    W0 ->
        pad_image_1(Image);
    W ->
        Pad = zeroes(PP*(W-W0)),
        Pixels = pad_rows(Pixels0, PP*W0, Pad, []),
        pad_image_1(Image#e3d_image{width=W,image=Pixels})
    end.

pad_image_1(#e3d_image{width=W,height=H0,image=Pixels0,bytes_pp=PP}=Image) ->
    case nearest_power_two(H0) of
    H0 ->
        pad_image_2(Image);
    H ->
        Pad = zeroes(PP*W*(H-H0)),
        Pixels = [Pixels0|Pad],
        pad_image_2(Image#e3d_image{height=H,image=Pixels})
    end.

pad_image_2(#e3d_image{image=Pixels}=Image) when is_list(Pixels) ->
    Image#e3d_image{image=list_to_binary(Pixels)};
pad_image_2(Image) -> Image.

pad_rows(Bin, W, Pad, Acc) ->
    case Bin of
    <<>> -> lists:reverse(Acc);
    <<Row:W/binary,T/binary>> ->
        pad_rows(T, W, Pad, [[Row|Pad]|Acc])
    end.

zeroes(0) -> [];
zeroes(1) -> [0];
zeroes(N) when N rem 2 =:= 0 ->
    Z = zeroes(N div 2),
    [Z|Z];
zeroes(N) ->
    Z = zeroes(N div 2),
    [0,Z|Z].

nearest_power_two(N) ->
    nearest_power_two(N, 1).

nearest_power_two(N, B) when N =< B -> B;
nearest_power_two(N, B) -> nearest_power_two(N, B bsl 1).
%%% Copied from wpc_image.erl
%%% end

%%% ==============================================
%%% copied from wings_prefs.erl
%%% begin
-define(MAC_PREFS, "Library/Preferences/Wings3D/Preferences.txt").
-define(MAC_OLD_PREFS, "Library/Preferences/Wings 3D Preferences.txt").
-define(WIN32_PREFS, "Wings3D/Preferences.txt").
-define(UNIX_PREFS, ".wings3d/preferences.txt").
-define(OLD_UNIX_PREFS, ".wings").

new_pref_file() ->
    case os:type() of
    {unix,darwin} ->
        filename:join(os:getenv("HOME"), ?MAC_PREFS);
    {unix,_} ->
        filename:join(os:getenv("HOME"), ?UNIX_PREFS);
    {win32,_} ->
        win32_new_pref()
    end.

win32_new_pref() ->
    case win32_appdata() of
    none ->
        none;
    AppData ->
        filename:join(AppData, ?WIN32_PREFS)
    end.

win32_appdata() ->
    case init:get_plain_arguments() of
    [AppData] ->
        AppData;
    [] ->
        %% No AppData location provided. Probably because
        %% a developer started Wings using a script that
        %% does not provide the location.
        none
    end.
%%% Copied from wings_pref.erl
%%% end


gen_tx_sizes() ->
    [MaxTxs0|_] = gl:getIntegerv(?GL_MAX_TEXTURE_SIZE),
    MaxTxs = max(min(4096, MaxTxs0), 256),
    gen_tx_sizes(MaxTxs, []).
%%% ==============================================
%%% copied from auv_texture.erl
%%% begin
gen_tx_sizes(Sz, Acc) when Sz < 128 -> Acc;
gen_tx_sizes(Sz, Acc) ->
    Bytes = Sz*Sz*3,
    Mb = 1024*1024,
    SzStr = if
        Bytes < 1024*1024 ->
            io_lib:format("(~pKb)",[Bytes div 1024]);
        true ->
            io_lib:format("(~pMb)",[Bytes div Mb])
        end,
    Str0 = io_lib:format("~px~p ", [Sz,Sz]),
    Str = lists:flatten([Str0|SzStr]),
    gen_tx_sizes(Sz div 2, [{Str,Sz}|Acc]).
%%% Copied from auv_texture.erl
%%% end


%%%
%%%  owner draw processing
%%%
% update_dlist/3 compiles the gl list, draw/4 is called from wings_render.erl.
% update_dlist({_,_}=Data, #dlo=D, #st=St)
update_dlist({fs,{FsList,VsList,HeList,Colored}}, #dlo{plugins=Pdl}=D, _St) ->
    Key = ?MODULE,
    VsTuple=list_to_tuple(VsList),
    List0 = gl:genLists(1),
    gl:newList(List0,?GL_COMPILE),
    pump_faces(FsList,VsTuple),
    gl:endList(),

    List1 = gl:genLists(1),
    gl:newList(List1,?GL_COMPILE),
    pump_edges(FsList,VsTuple),
    gl:endList(),

    List2=case HeList of
    [] -> none;
    _ ->
        List3 = gl:genLists(1),
        gl:newList(List3,?GL_COMPILE),
        pump_hard_edges(HeList,VsTuple),
        gl:endList(),
        List3
    end,
    D#dlo{plugins=[{Key,{fs,[List0,List1,List2,Colored]}}|Pdl]}.

pump_faces([],_) -> ok;
pump_faces([[[_,_,_,_]|_]|_]=Fs,VsList) ->
    lists:foreach(fun(F0) ->
        pump_faces(F0,VsList)
    end, Fs);
pump_faces(Fs,VsList) ->
    ColRamp=wings_pref:get_value(heightmap_color_ramp),
    lists:foreach(
      fun(F) ->
          {Vs,VsCol}=lists:foldl(fun(Vi,{VAcc,HAcc}) ->
              case element(Vi+1,VsList) of
                  {{_,_,_}=V,H} ->
                      Idx=trunc(H* (?CR_W*1.0)),
                      Vc=array:get(Idx,ColRamp),
                      {VAcc++[V],HAcc++[Vc]};
                  {_,_,_}=V ->
                      {VAcc++[V],HAcc}
              end
          end, {[],[]}, F),
          gl:'begin'(?GL_POLYGON),
          gl:normal3fv(e3d_vec:normal(Vs)),
          pump_vertex(Vs,VsCol),
          gl:'end'()
      end, Fs).

pump_edges([],_) -> ok;
pump_edges([[[_,_,_,_]|_]|_]=Fs,VsList) ->
    lists:foreach(fun(F0) ->
        pump_edges(F0,VsList)
    end, Fs);
pump_edges(Fs,VsList) ->
    lists:foreach(
      fun(F) ->
          Vs=lists:foldl(fun(Vi,VAcc) ->
              VAcc++case element(Vi+1,VsList) of
                  {{_,_,_}=V,_} -> [V];
                  {_,_,_}=V -> [V]
              end
          end, [], F),
          gl:'begin'(?GL_POLYGON),
          pump_vertex(Vs,[]),
          gl:'end'()
      end, Fs).

pump_hard_edges([],_) -> ok;
pump_hard_edges(Es,VsList) ->
    lists:foldl(fun({I0,I1}, _) ->
          gl:'begin'(?GL_LINES),
          case element(I0+1,VsList) of
            {{_,_,_}=V1,_} -> {{_,_,_}=V2,_}=element(I1+1,VsList);
            {_,_,_}=V1 -> V2=element(I1+1,VsList)
          end,
          gl:vertex3fv(V1),
          gl:vertex3fv(V2),
          gl:'end'()
      end, [], Es).

pump_vertex([],[]) -> ok;
pump_vertex([V|Vs],[]=Vsc) ->
    gl:vertex3fv(V),
    pump_vertex(Vs,Vsc);
pump_vertex([V|Vs],[Vc|Vsc]) ->
    gl:color3fv(Vc),
    gl:vertex3fv(V),
    pump_vertex(Vs,Vsc).

% draw(plain/smooth=Flag, #dlo=D, body/face/edge/vertex=SelMode)
draw(_, {fs,[LstFs,LstEs,LstHe,Colored]}, _, SelMode) ->
    gl:disable(?GL_CULL_FACE),
    gl:disable(?GL_POLYGON_SMOOTH),

    if Colored=:= false ->
        wings_render:enable_lighting(wings_view:load_matrices(true));
      true -> ok
    end,
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    wings_render:polygonOffset(2),
    gl:polygonMode(?GL_FRONT, ?GL_FILL),
    wings_dl:call(LstFs),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    if Colored=:= false ->
        wings_render:disable_lighting();
      true -> ok
    end,

    gl:enable(?GL_POLYGON_OFFSET_LINE),
    wings_render:polygonOffset(1),
    gl:color3fv(wings_pref:get_value(edge_color)),
    gl:polygonMode(?GL_FRONT, ?GL_LINE),
    wings_dl:call(LstEs),
    gl:disable(?GL_POLYGON_OFFSET_LINE),
    draw_hard_edges(LstHe,SelMode);
draw(_,_,_,_) ->
    ok.

draw_hard_edges(none, _) -> ok;
draw_hard_edges(Hard, SelMode) ->
    gl:lineWidth(hard_edge_width(SelMode)),
    gl:color3fv(wings_pref:get_value(hard_edge_color)),
    wings_dl:call(Hard).

hard_edge_width(edge) -> wings_pref:get_value(hard_edge_width);
hard_edge_width(_) -> max(wings_pref:get_value(hard_edge_width) - 1, 1).


% get_data/3 There are currently only two atoms/tags/flags that are looked for when wings
% goes to get the stored data to draw stuff from plugins ('update_dlist' and 'save').
% Because the #pst could be used to store all sorts of data, 'update_dlist' or 'save' in
% the first argument of get_data is there to allow you to specify what data to retrieve
% from the pst. And specifically, what to redraw/update when usings the 'update_dlist' tag,
% and what to save with in my_model.wings when using the 'save' tag.
% get_data/3 is called from wings_plugin.erl as ?MODULE:get_data/3
%
% get_data(update_dlist/save=Flag, plugin data=PData, Acc) should result {ok, Result}
get_data(update_dlist, {{Vs,Fs,He,Colored},_Res}=_Data, Acc) ->
    {ok, [{plugin, {?MODULE, {fs, {Fs,Vs,He,Colored}}}}|Acc]};
get_data(save, _, _) ->
    {none, []}.

% merge_we/1 is used to transfer draw data to the proper We when merging/combining/bridging/separating objects.
% If an object is added to another object, there may be similarly id numbered verts or other elements.
% So Wings has to renumber all the elements so that each is unique. merge_we/1 ensures that the elements
% you are storing for your draw elements are renumbered accordingly.
merge_we(_We) ->
    io:format("merge_we called.",[]),
    _We.

