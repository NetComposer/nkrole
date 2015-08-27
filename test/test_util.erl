%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(test_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).
-include_lib("nklib/include/nklib.hrl").
-include_lib("eunit/include/eunit.hrl").

insert(Set) ->
    ?debugFmt("start insert for ~p", [Set]),
    ets:delete_all_objects(nkrole_backend),
	nkrole_proxy:stop_all(),
    wait_for_stop(600),
	lists:foreach(
		fun({ObjId, Roles}) -> ok = nkrole_backend:put_rolemap(ObjId, Roles) end,
		objs(Set)).


objs(set1) -> [
	{root, #{member => [#{member=>orgA}, #{member=>orgB}]}},

	% OrgA
	{orgA, #{member => [#{member=>depA1}, #{member=>depA2}, #{member=>depA3}], head=>[root]}},
	{depA1, #{member => [u01, u02], head=>[orgA, #{head=>orgA}]}},
	{depA2, #{member => [depA21, depA22, #{member=>depA21}, #{member=>depA22}], head=>[orgA, #{head=>orgA}]}},
	{depA21, #{member => [u03, u04], head=>[depA2, #{head=>depA2}]}},
	{depA22, #{member => [u05, u06], head=>[depA2, #{head=>depA2}]}},
	{depA3, #{member => [u07, u08], head=>[orgA, #{head=>orgA}]}},
	% Dom2
	{orgB, #{member => [u10, #{member=>depB1}], head=>[root]}},
	{depB1, #{member => [u11, u12], head=>[orgB, #{head=>orgB}]}},

	% Users
	{u01, #{head=>[depA1, #{head=>depA1}]}},
	{u02, #{head=>[depA1, #{head=>depA1}]}},
	{u03, #{head=>[depA21, #{head=>depA21}]}},
	{u04, #{head=>[depA21, #{head=>depA21}]}},
	{u05, #{head=>[depA22, #{head=>depA22}]}},
	{u06, #{head=>[depA22, #{head=>depA22}]}},
	{u07, #{head=>[depA3, #{head=>depA3}]}},
	{u08, #{head=>[depA3, #{head=>depA3}]}},
	{u09, #{}},
	{u10, #{head=>[orgB, #{head=>orgB}]}},
	{u11, #{head=>[depB1, #{head=>depB1}]}},
	{u12, #{head=>[depB1, #{head=>depB1}]}}
];


objs(set2) -> [
    {obj_1, #{member=>[obj_1, obj_2, #{member=>obj_1}, #{member=>obj_2}]}},
    {obj_2, #{member=>[obj_1, obj_2, #{member=>obj_1}, #{member=>obj_2}]}},
    {obj_3, #{member=>[#{member=>obj_3}]}},
    {obj_4, #{member=>[#{member=>obj_4}]}},
    {obj_5, #{member=>[#{member=>obj_5}]}},
    {obj_6, #{member=>[#{member=>obj_6}]}},
    {obj_7, #{member=>[#{member=>obj_7}]}}
].



populate(Levels) when Levels >= 3 ->
    ?debugMsg("Populating..."),
    true = ets:delete_all_objects(nkrole_backend),
    nkrole_proxy:stop_all(),
    wait_for_stop(600),
    nkrole_backend:put_rolemap(<<"root">>, #{member => make_childs(<<"R">>)}),
    ?debugFmt("Creating ~p objects", [level_objs(Levels-1)]),
    [populate_level(Pos, <<"R">>, Levels-1) || Pos <- lists:seq(0,9)],
    ok.


populate_level(Pos, Base, 1) ->
    ObjId = make_pos(Pos, Base),
    nkrole_backend:put_rolemap(ObjId, #{});

populate_level(Pos, Base, 2) ->
    ObjId = make_pos(Pos, Base),
    nkrole_backend:put_rolemap(ObjId, #{member => make_childs_final(ObjId)}),
    [populate_level(SubPos, ObjId, 1) || SubPos <- lists:seq(0,9)];

populate_level(Pos, Base, Level) ->
    ObjId = make_pos(Pos, Base),
    nkrole_backend:put_rolemap(ObjId, #{member => make_childs(ObjId)}),
    [populate_level(SubPos, ObjId, Level-1) || SubPos <- lists:seq(0,9)].


make_pos(Pos, Base) ->
    <<(integer_to_binary(Pos))/binary, $_, Base/binary>>.

make_childs(Base) ->
    [#{member=>Pos} || Pos <- make_childs_final(Base)].

make_childs_final(Base) ->
    [make_pos(Pos, Base) || Pos <- lists:seq(0, 9)].


level_objs(Level) ->
    round(math:pow(10, Level)).


wait_for_stop(0) ->
    error(?LINE);
wait_for_stop(N) ->
    case nkrole_cache:get_all() of
        [] -> 
            ok;
        L ->
            nkrole_proxy:stop_all(),
            ?debugFmt("...wait for ~p caches to stop)", [length(L)]),
            timer:sleep(100), wait_for_stop(N-1)
    end.

