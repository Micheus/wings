%%
%%  wpc_mesh_bump.erl --
%%
%%     A mesh modifier that uses an applied texture as modeler
%%
%%  Copyright (c) 2011 Micheus (update: 2025)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_mesh_bump).

-define(NEED_OPENGL, 1).
-define(MAX_HUE_VALUE, 360.0).
-define(MAX_RGB_VALUE, 255).

-include_lib("wings/src/wings.hrl").

-export([init/0,menu/2,command/2]).
-type vertex_num() :: non_neg_integer().

init() -> true.

menu({vertex}, Menu) ->
    init_menu(Menu);
%%menu({face}, Menu) ->
%%    init_menu(Menu);
menu(_,Menu) -> 
    Menu.

init_menu(Menu) ->
    case parse_micheus(Menu,[]) of
    [] -> [micheus_heading(),separator]++Menu;
    Menu0 -> Menu0
    end.

parse_micheus([],_) -> [];
parse_micheus([{"Micheus add on..."=MId0,{MId1,SubMenu}}|Rest],Acc) ->
    T0={MId0,{MId1,SubMenu++[mesh_bump_heading()]}},
    [T0,Rest]++Acc;
parse_micheus([Elem|Rest],Acc) ->
    parse_micheus(Rest, [Acc|Elem]).
    
micheus_heading() ->	
    {?__(1,"Micheus add on..."),
      {bymicheus, [mesh_bump_heading()]}}.

mesh_bump_heading() ->	
    {?__(1,"Mesh Bump"),mesh_bump,
          ?__(2,"Modify a mesh using un existent UV map applied to it.")}.

command({vertex,{bymicheus,mesh_bump}}, St) ->
    mesh_bump_vertex(St);
command(_, _) ->
    next.

mesh_bump_vertex(St) ->
    F = fun(Vs0, We) ->
            case wings_va:any_uvs(We) of
                true ->
                    % applying temporary texture color to vertices
                    We1 = wings_we:uv_to_color(We, St),
                    % getting the height information for each vertex => [{V,Height}|_]
                    VsColor = lists:foldl(fun(V,Acc) ->
                                            Va = wings_va:vtx_attrs(V,We1),
                                            Value = add_col_info(V,wings_va:attr(color,Va)),
                                            [Value|Acc]
                                          end,[],gb_sets:to_list(Vs0)),
                    % preparing vertex information for custom function [{V,Pos,N,Height}|_]
                    {Vs,VsInfo} = make_vertex_info(We1,VsColor),
                    {Vs,mesh_bump_fun(VsInfo)};
                false ->
                    []
            end
        end,
    wings_drag:fold(F, [distance], [{initial,[1.0]}], St).

-spec add_col_info(vertex_num(), {float(),float(),float()} | 'none') -> {vertex_num(),float()} | 'none'.
add_col_info(V,none) -> {V,0.0};
add_col_info(V, Col) ->
    Height=rgb_to_height(Col),
    {V,Height}.

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
% | 255 |  0  |     | Red  => Hue scale = 0� (the max height)
% |  :  |  :  |     | yellow
% |  0  | 255 |  0  | green
% |     |  :  |  :  | cyan
% |  0  |  0  | 255 | blue
% |  :  |     |  :  | violet
% | 254 |     |  0  | pink  => Hue scale = 359.999�  (the min height)
% +-----+-----+-----+
% After get the HUE value, it should be inverted
rgb_to_height(R,G,B) ->
    {Hue,_,_}=wings_color:rgb_to_hsv(R,G,B),
    ?MAX_HUE_VALUE-Hue.
rgb_to_height({V,V,V}) ->
    rgb_to_height(V,V,V);
rgb_to_height({R,G,B}) ->
    rgb_to_height(R,G,B).

make_vertex_info(We,VsColor) ->
    lists:foldl(fun({V,Height}, {VAcc,IAcc}) ->
              NVec = wings_vertex:normal(V,We),
              #we{vp=Vtab}=We,
              Pos = wings_vertex:pos(V,Vtab),
              {[V|VAcc],[{V,Pos,NVec,Height}|IAcc]}
          end, {[],[]}, VsColor).

mesh_bump_fun(VsInfo) ->
    %% Move each element along its own normal.
    fun([Dist],Acc) ->
        lists:foldl(
                  fun({V,Pos0,N,Mul}, A) ->
                      Offset=Dist*Mul,
                      VOffset=e3d_vec:mul(N,Offset),
                      Pos = e3d_vec:add(Pos0,VOffset),
                      [{V,Pos}|A]
                  end, Acc, VsInfo)
    end.
