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

-module(loop_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).
-include_lib("nklib/include/nklib.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(nkrole, [find_role_objs/3]).


loop_test_() ->
  	{setup, 
    	fun() -> 
    		ok = nkrole_app:start()
		end,
		fun(_) -> 
			ok 
		end,
	    fun(_) ->
		    [
		    	fun() -> loop() end
			]
		end
  	}.




loop() ->
    test_util:insert(set2),
    lager:notice("The following messages about looped errors are expected"),
    {ok, [obj_2, obj_1, obj_1, obj_2]} = find_role_objs(member, obj_1, #{}),
    {ok, [obj_2, obj_1]} = find_role_objs(member, obj_2, #{}),
    {ok, []} = find_role_objs(member, obj_3, #{}),
    {ok, []} = find_role_objs(member, obj_4, #{}),
    {ok, []} = find_role_objs(member, obj_5, #{}),
    {ok, []} = find_role_objs(member, obj_6, #{}),
    {ok, []} = find_role_objs(member, obj_7, #{}),
    ok.

	
	