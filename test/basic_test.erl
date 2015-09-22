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

-module(basic_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).
-include_lib("nklib/include/nklib.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(nkrole, [get_roles/2, get_role_objs/3, find_role_objs/3,
					       add_role/4, add_subrole/5, del_role/4, del_subrole/5,
					       get_proxy/2, stop/1]).


basic_test_() ->
  	{setup, 
    	fun() -> 
    		ok = nkrole_app:start()
		end,
		fun(_) -> 
            ok
		end,
	    fun(_) ->
		    [
		    	fun() -> roles() end,
                fun() -> find() end,
		    	fun() -> caches() end
			]
		end
  	}.




roles() ->
    ?debugMsg("Starting Basic test"),
    test_util:insert(set1),
    timer:sleep(100),

    {ok, [member]} = get_roles(root, #{}),
    {ok, [#{member:=orgA}, #{member:=orgB}]} = get_role_objs(member, root, #{}),

    ok = add_role(member, root, u01, #{}),
    {ok,[u01, #{member:=orgA}, #{member:=orgB}]} = get_role_objs(member, root, #{}),

    ok = add_role(other, root, u02, #{}),
    {ok, [member, other]} = get_roles(root, #{}),
    {ok, [u02]} = get_role_objs(other, root, #{}),

    ok = add_subrole(member, root, other, u03, #{}),
    {ok, [#{other:=u03}, u01, #{member:=orgA}, #{member:=orgB}]} = get_role_objs(member, root, #{}),

    ok = add_subrole(other, root, member, u03, #{}),
    {ok, [#{other:=u03}, u01, #{member:=orgA}, #{member:=orgB}]} = get_role_objs(member, root, #{}),
    {ok, [#{member:= u03}, u02]} = get_role_objs(other, root, #{}),

    ok = add_role(other, root, u03, #{}),
    {ok, [u03, #{member:=u03}, u02]} = get_role_objs(other, root, #{}),

    ok = add_subrole(other, root, member, u03, #{}),
    {ok, [u03, #{member:=u03}, u02]} = get_role_objs(other, root, #{}),

    ok = add_role(other, root, u03, #{}),
    {ok, [u03, #{member:=u03}, u02]} = get_role_objs(other, root, #{}),

    ok = del_role(member, root, u01, #{}),
    ok = del_role(member, root, u03, #{}),
    {ok, [#{other:=u03}, #{member:=orgA}, #{member:=orgB}]} = get_role_objs(member, root, #{}),

    ok = del_subrole(member, root, member, u03, #{}),
    {ok, [#{other:=u03}, #{member:=orgA}, #{member:=orgB}]} = get_role_objs(member, root, #{}),

    ok = del_subrole(member, root, other, u03, #{}),
    {ok, [#{member:=orgA}, #{member:=orgB}]} = get_role_objs(member, root, #{}),
    ok.


find() ->
    ?debugMsg("Starting Find test"),
    test_util:insert(set1),
    timer:sleep(100),

    {ok, [u01,u02,depA21,depA22,u03,u04,u05,u06,u07,u08,u10,u11,u12]} =
        find_role_objs(member, root, #{}),
    {ok, []} = find_role_objs(other, root, #{}),

    {ok, [u01,u02,depA21,depA22,u03,u04,u05,u06,u07,u08]} = find_role_objs(member, orgA, #{}),
    {ok, [u02,u01]} = find_role_objs(member, depA1, #{}),
    {ok, [depA22,depA21,u03,u04,u05,u06]} = find_role_objs(member, depA2, #{}),
    {ok, [u04,u03]} = find_role_objs(member, depA21, #{}),
    {ok, [u06,u05]} = find_role_objs(member, depA22, #{}),
    {ok, [u08,u07]} = find_role_objs(member, depA3, #{}),
    {ok, [u10,u11,u12]} = find_role_objs(member, orgB, #{}),
    {ok, [u12,u11]} = find_role_objs(member, depB1, #{}),
    
    {ok, []} = find_role_objs(head, root, #{}),
    {ok, [root]} = find_role_objs(head, orgA, #{}),
    {ok, [orgA, root]} = find_role_objs(head, depA1, #{}),
    {ok, [orgA, root]} = find_role_objs(head, depA2, #{}),
    {ok, [depA2, orgA, root]} = find_role_objs(head, depA21, #{}),
    {ok, [depA2, orgA, root]} = find_role_objs(head, depA22, #{}),
    {ok, [orgA, root]} = find_role_objs(head, depA3, #{}),
    {ok, [root]} = find_role_objs(head, orgB, #{}),
    {ok, [orgB, root]} = find_role_objs(head, depB1, #{}),
    {ok, [depA1, orgA, root]} = find_role_objs(head, u01, #{}),
    ok.


caches() ->
    ?debugMsg("Starting Caches test"),
    % Must run after find to populate the cache
    % find(),

    M_root = get_cache(root, member),
    true = is_process_alive(M_root),
    {[], [M_orgA, M_orgB]} = get_cached(root, member),
    M_orgA = get_cache(orgA, member),
    {[], [M_depA1, M_depA2, M_depA3]} = get_cached(orgA, member),
    M_depA1 = get_cache(depA1, member),
    {[u01, u02], []} = get_cached(depA1, member),
    M_depA2 = get_cache(depA2, member),
    {[depA21, depA22], [M_depA21, M_depA22]} = get_cached(depA2, member),
    M_depA21 = get_cache(depA21, member),
    {[u03, u04], []} = get_cached(depA21, member),
    M_depA22 = get_cache(depA22, member),
    {[u05, u06], []} = get_cached(depA22, member),
    M_depA3 = get_cache(depA3, member),
    {[u07, u08], []} = get_cached(depA3, member),
    M_orgB = get_cache(orgB, member),
    {[u10], [M_depB1]} = get_cached(orgB, member),
    M_depB1 = get_cache(depB1, member),
    {[u11, u12], []} = get_cached(depB1, member),

    {[], []} = get_cached(root, head),
    {[root], []} = get_cached(orgA, head),
    {[orgA], [H_orgA]} = get_cached(depA1, head),
    H_orgA = get_cache(orgA, head),
    {[orgA], [H_orgA]} = get_cached(depA2, head),
    {[orgA], [H_orgA]} = get_cached(depA3, head),
    {[depA2], [H_depA2]} = get_cached(depA21, head),
    H_depA2 = get_cache(depA2, head),
    {[depA2], [H_depA2]} = get_cached(depA22, head),
    {[root], []} = get_cached(orgB, head),
    {[orgB], [H_orgB]} = get_cached(depB1, head),
    H_orgB = get_cache(orgB, head),
    H_depB1 = get_cache(depB1, head),
    {[depA1], [H_depA1]} = get_cached(u01, head),
    H_depA1 = get_cache(depA1, head),

    
   	% If we stop u06 proxy, 'member' caches are not affected (it is a direct role)
   	% (but, if the obj is removed, caches will contain a reference to a deleted object)
	stop(u06),
	timer:sleep(50),
    {[], [M_orgA, M_orgB]} = get_cached(root, member),
    {[], [M_depA1, M_depA2, M_depA3]} = get_cached(orgA, member),
    {[u11, u12], []} = get_cached(depB1, member),

    % However, if we stop depB1 proxy, the orgB branch caches are removed
	stop(depB1),
	timer:sleep(50),
    none = get_cached(root, member),
    false = is_process_alive(M_root),
    {[], [M_depA1, M_depA2, M_depA3]} = get_cached(orgA, member),
    {[u01, u02], []} = get_cached(depA1, member),
    {[depA21, depA22], [M_depA21, M_depA22]} = get_cached(depA2, member),
    {[u03, u04], []} = get_cached(depA21, member),
    {[u05, u06], []} = get_cached(depA22, member),
    {[u07, u08], []} = get_cached(depA3, member),
    none = get_cached(orgB, member),
    false = is_process_alive(M_orgB),

    % Other roles are also affected
    {[], []} = get_cached(root, head),
    {[root], []} = get_cached(orgA, head),
    {[orgA], [H_orgA]} = get_cached(depA1, head),
    {[orgA], [H_orgA]} = get_cached(depA2, head),
    {[orgA], [H_orgA]} = get_cached(depA3, head),
    {[depA2], [H_depA2]} = get_cached(depA21, head),
    {[depA2], [H_depA2]} = get_cached(depA22, head),
    {[root], []} = get_cached(orgB, head),
    none = get_cached(depB1, head),
    false = is_process_alive(H_depB1),


   	% We regenerate it
	{ok, _} = find_role_objs(member, root, #{}),
    {[], [M_orgA, _M_orgB_2]} = get_cached(root, member),
    M_root_2 = get_cache(root, member),
    true = is_process_alive(M_root_2),
    {[], [M_depA1, M_depA2, M_depA3]} = get_cached(orgA, member),
    {[u01, u02], []} = get_cached(depA1, member),
    {[depA21, depA22], [M_depA21, M_depA22]} = get_cached(depA2, member),
    {[u03, u04], []} = get_cached(depA21, member),
    {[u05, u06], []} = get_cached(depA22, member),
    {[u07, u08], []} = get_cached(depA3, member),
    {[u10], [M_depB1_2]} = get_cached(orgB, member),
    {[u11, u12], []} = get_cached(depB1, member),

	{ok, [orgB, root]} = find_role_objs(head, depB1, #{}),

   	% We add U09 on DepA3
   	% Caches at depA3, orgA and Root are affected
   	ok = add_role(member, depA3, u09, #{}),
   	timer:sleep(50),
    none = get_cached(root, member),
    false = is_process_alive(M_root_2),
    none = get_cached(orgA, member),
    false = is_process_alive(M_orgA),
    {[u01, u02], []} = get_cached(depA1, member),
    {[depA21, depA22], [M_depA21, M_depA22]} = get_cached(depA2, member),
    {[u03, u04], []} = get_cached(depA21, member),
    {[u05, u06], []} = get_cached(depA22, member),
    none = get_cached(depA3, member),
    {[u10], [M_depB1_2]} = get_cached(orgB, member),
    {[u11, u12], []} = get_cached(depB1, member),

   	% Other roles should not be affected
    {[], []} = get_cached(root, head),
    {[root], []} = get_cached(orgA, head),
    {[orgA], [H_orgA]} = get_cached(depA1, head),
    {[orgA], [H_orgA]} = get_cached(depA2, head),
    {[orgA], [H_orgA]} = get_cached(depA3, head),
    {[depA2], [H_depA2]} = get_cached(depA21, head),
    {[depA2], [H_depA2]} = get_cached(depA22, head),
    {[root], []} = get_cached(orgB, head),
    {[orgB], [H_orgB]} = get_cached(depB1, head),

   	% We update head at root, no one is affected (it is a direct role)
   	ok = add_role(head, root, u01, #{}),
   	timer:sleep(50),
    none = get_cached(root, head),
    {[root], []} = get_cached(orgA, head),
    {[orgA], [H_orgA]} = get_cached(depA1, head),
    {[orgA], [H_orgA]} = get_cached(depA2, head),
    {[orgA], [H_orgA]} = get_cached(depA3, head),
    {[depA2], [H_depA2]} = get_cached(depA21, head),
    {[depA2], [H_depA2]} = get_cached(depA22, head),
    {[root], []} = get_cached(orgB, head),
    {[orgB], [H_orgB]} = get_cached(depB1, head),

   	% However, if I modify orgA, its down branch is affected
   	ok = add_role(head, orgA, u01, #{}),
   	timer:sleep(50),

    none = get_cached(root, head),	% (from before)
    none = get_cached(orgA, head),
    none = get_cached(depA1, head),
    none = get_cached(depA2, head),
    none = get_cached(depA21, head),
    none = get_cached(depA22, head),
    none = get_cached(depA3, head),
    {[root], []} = get_cached(orgB, head),
    {[orgB], [H_orgB]} = get_cached(depB1, head),

   	{ok, [depA2, orgA, u01, root]} = find_role_objs(head, depA22, #{}),
   	ok.



%%%%%%%%%%%%%%%%%%%%%%  Util %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	


get_cached(ObjId, Role) ->
    case get_cache(ObjId, Role) of
        Pid when is_pid(Pid) ->
            {ok, Objs, Pids} = nkrole_cache:get_cached(Pid),
            {Objs, Pids};
        Other ->
            Other
    end.


get_cache(ObjId, Role) ->
	{ok, Pid} = nkrole_proxy:get_proxy(ObjId, #{}),
	Caches = gen_server:call(Pid, get_caches),
	nklib_util:get_value(Role, Caches, none).


