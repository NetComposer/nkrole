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

-module(large_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).
-include_lib("nklib/include/nklib.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(nkrole_proxy, [get_obj/1, get_roles/1, get_role_objs/2, find_role_objs/2,
					   add_role/3, add_subrole/4, del_role/3, del_subrole/4,
					   get_proxy/1, stop_all/0, stop_obj/1]).


-define(LEVELS, 4).


large_test_() ->
  	{setup, 
    	fun() -> 
    		ok = nkrole_app:start(),
            timer:sleep(500),
            test_util:populate(?LEVELS)
		end,
		fun(_) -> 
			ok 
		end,
	    fun(_) ->
		    [
                {timeout, 60, fun() -> large() end}
			]
		end
  	}.


large() ->
    ?debugMsg("Starting large test"),
    {Time1, {ok, LMax}} =
        timer:tc(fun() -> nkrole_proxy:find_role_objs(member, <<"root">>, #{timeout=>60000}) end),
    ?debugFmt("First time: ~pmsec", [Time1 div 1000]),
    {Time2, {ok, LMax}} =
        timer:tc(fun() -> nkrole_proxy:find_role_objs(member, <<"root">>, #{}) end),
    ?debugFmt("Second time: ~pmsec", [Time2 div 1000]),



    N1 = test_util:level_objs(?LEVELS-1),
    N1 = length(LMax),
  
    {Time3, {ok, false}} =
        timer:tc(fun() -> nkrole_proxy:has_role(<<"none">>, member, <<"root">>, #{timeout=>60000}) end),
    ?debugFmt("First search time: ~pmsec", [Time3 div 1000]),
    {Time4, {ok, false}} =
        timer:tc(fun() -> nkrole_proxy:has_role(<<"none">>, member, <<"root">>, #{}) end),
    ?debugFmt("Seconds search time: ~pmsec", [Time4 div 1000]),

    [Last|_] = lists:reverse(LMax),
    {ok, true} = nkrole_proxy:has_role(Last, member, <<"root">>, #{}),

    [O1|_] = LMax,
    {ok, []} = nkrole_proxy:find_role_objs(member, O1, #{}),
    <<"0_", O2/binary>> = O1,
    {ok, L2} = nkrole_proxy:find_role_objs(member, O2, #{}),
    10 = length(L2),
    <<"0_", O3/binary>> = O2,
    {ok, L3} = nkrole_proxy:find_role_objs(member, O3, #{}),
    100 = length(L3),
    ok.
  







