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

-import(nkrole_proxy, [get_obj/2, get_roles/2, get_role_objs/3, find_role_objs/3,
					   add_role/4, add_subrole/5, del_role/4, del_subrole/5,
					   get_proxy/2, stop_all/0, stop_obj/1]).


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
		    	fun() -> caches() end
			]
		end
  	}.




roles() ->
	test_util:insert(set1),
	
	{ok, [member]} = get_roles(root, #{}),
	{ok, [{member,orgA}, {member,orgB}]} = get_role_objs(member, root, #{}),

	ok = add_role(member, root, u01, #{}),
	{ok,[u01, {member,orgA}, {member,orgB}]} = get_role_objs(member, root, #{}),
	
	ok = add_role(other, root, u02, #{}),
	{ok, [member, other]} = get_roles(root, #{}),
	{ok, [u02]} = get_role_objs(other, root, #{}),

	ok = add_subrole(member, root, other, u03, #{}),
	{ok, [{other, u03}, u01, {member,orgA}, {member,orgB}]} = get_role_objs(member, root, #{}),

	ok = add_subrole(other, root, member, u03, #{}),
	{ok, [{other, u03}, u01, {member,orgA}, {member,orgB}]} = get_role_objs(member, root, #{}),
	{ok, [{member, u03}, u02]} = get_role_objs(other, root, #{}),

	ok = add_role(other, root, u03, #{}),
	{ok, [u03, {member,u03}, u02]} = get_role_objs(other, root, #{}),

	ok = add_subrole(other, root, member, u03, #{}),
	{ok, [u03, {member,u03}, u02]} = get_role_objs(other, root, #{}),
	
	ok = add_role(other, root, u03, #{}),
	{ok, [u03, {member,u03}, u02]} = get_role_objs(other, root, #{}),

	ok = del_role(member, root, u01, #{}),
	ok = del_role(member, root, u03, #{}),
	{ok, [{other, u03}, {member,orgA}, {member,orgB}]} = get_role_objs(member, root, #{}),

	ok = del_subrole(member, root, member, u03, #{}),
	{ok, [{other, u03}, {member,orgA}, {member,orgB}]} = get_role_objs(member, root, #{}),

	ok = del_subrole(member, root, other, u03, #{}),
	{ok, [{member,orgA}, {member,orgB}]} = get_role_objs(member, root, #{}),
	ok.


caches() ->
	test_util:insert(set1),

 	{ok, [u01,u02,depA21,depA22,u03,u04,u05,u06,u07,u08,u10,u11,u12] = RootMembers} =
		find_role_objs(member, root, #{}),
    {ok, []} = find_role_objs(other, root, #{}),

    check_cached(root, member, RootMembers),
    check_cached(orgA, member, [u01,u02,depA21,depA22,u03,u04,u05,u06,u07,u08]),
    check_cached(depA1, member, [u01, u02]),
    check_cached(depA2, member, [depA21, depA22, u03, u04, u05, u06]),
    check_cached(depA21, member, [u03, u04]),
    check_cached(depA22, member, [u05, u06]),
    check_cached(depA3, member, [u07, u08]),
   	check_cached(orgB, member, [u10, u11, u12]),
   	check_cached(depB1, member, [u11, u12]),
   	
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

    check_cached(root, head, []),
    check_cached(orgA, head, [root]),
    check_cached(depA1, head, [orgA, root]),
    check_cached(depA2, head, [orgA, root]),
    check_cached(depA21, head, [depA2, orgA, root]),
    check_cached(depA22, head, [depA2, orgA, root]),
    check_cached(depA3, head, [orgA, root]),
   	check_cached(orgB, head, [root]),
   	check_cached(depB1, head, [orgB, root]),
   	check_cached(u01, head, [depA1, orgA, root]),

   	% If we stop u06 proxy, 'member' caches are not affected (it is a direct role)
   	% (but, if the obj is removed, caches will contain a reference to a deleted object)
	stop_obj(u06),
	timer:sleep(50),
    check_cached(root, member, RootMembers),
    check_cached(orgA, member, [u01,u02,depA21,depA22,u03,u04,u05,u06,u07,u08]),
   	check_cached(depB1, member, [u11, u12]),

    % However, if we stop depB1 proxy, the orgB branch caches are removed
	stop_obj(depB1),
	timer:sleep(50),
    check_cached(root, member, none),
    check_cached(orgA, member, [u01,u02,depA21,depA22,u03,u04,u05,u06,u07,u08]),
    check_cached(depA1, member, [u01, u02]),
    check_cached(depA2, member, [depA21, depA22, u03, u04, u05, u06]),
    check_cached(depA21, member, [u03, u04]),
    check_cached(depA22, member, [u05, u06]),
    check_cached(depA3, member, [u07, u08]),
   	check_cached(orgB, member, none),

   	% Other roles should not be affected
    check_cached(root, head, []),
    check_cached(orgA, head, [root]),
    check_cached(depA1, head, [orgA, root]),
    check_cached(depA2, head, [orgA, root]),
    check_cached(depA21, head, [depA2, orgA, root]),
    check_cached(depA22, head, [depA2, orgA, root]),
    check_cached(depA3, head, [orgA, root]),
   	check_cached(orgB, head, [root]),

   	% We regenerate it
	{ok, RootMembers} = find_role_objs(member, root, #{}),
    check_cached(root, member, RootMembers),
    check_cached(orgA, member, [u01,u02,depA21,depA22,u03,u04,u05,u06,u07,u08]),
    check_cached(depA1, member, [u01, u02]),
    check_cached(depA2, member, [depA21, depA22, u03, u04, u05, u06]),
    check_cached(depA21, member, [u03, u04]),
    check_cached(depA22, member, [u05, u06]),
    check_cached(depA3, member, [u07, u08]),
   	check_cached(orgB, member, [u10, u11, u12]),
   	check_cached(depB1, member, [u11, u12]),
   	
	{ok, [orgB, root]} = find_role_objs(head, depB1, #{}),
   	check_cached(depB1, head, [orgB, root]),


   	% We add U09 on DepA3
   	% Caches at depA3, orgA and Root are affected
   	ok = add_role(member, depA3, u09, #{}),
   	timer:sleep(50),
    check_cached(root, member, none),
    check_cached(orgA, member, none),
    check_cached(depA1, member, [u01, u02]),
    check_cached(depA2, member, [depA21, depA22, u03, u04, u05, u06]),
    check_cached(depA21, member, [u03, u04]),
    check_cached(depA22, member, [u05, u06]),
    check_cached(depA3, member, none),
   	check_cached(orgB, member, [u10, u11, u12]),
   	check_cached(depB1, member, [u11, u12]),

   	% Other roles should not be affected
    check_cached(root, head, []),
    check_cached(orgA, head, [root]),
    check_cached(depA1, head, [orgA, root]),
    check_cached(depA2, head, [orgA, root]),
    check_cached(depA21, head, [depA2, orgA, root]),
    check_cached(depA22, head, [depA2, orgA, root]),
    check_cached(depA3, head, [orgA, root]),
   	check_cached(orgB, head, [root]),
   	check_cached(depB1, head, [orgB, root]),

   	% We update head at root, no one is affected (it is a direct role)
   	ok = add_role(head, root, u01, #{}),
   	timer:sleep(50),
    check_cached(root, head, none),
    check_cached(orgA, head, [root]),
    check_cached(depA1, head, [orgA, root]),
    check_cached(depA2, head, [orgA, root]),
    check_cached(depA21, head, [depA2, orgA, root]),
    check_cached(depA22, head, [depA2, orgA, root]),
    check_cached(depA3, head, [orgA, root]),
   	check_cached(orgB, head, [root]),
   	check_cached(depB1, head, [orgB, root]),
   
   	% However, if I modify orgA, its down branch is affected
   	ok = add_role(head, orgA, u01, #{}),
   	timer:sleep(50),
    check_cached(root, head, none),	% (from before)
    check_cached(orgA, head, none),
    check_cached(depA1, head, none),
    check_cached(depA2, head, none),
    check_cached(depA21, head, none),
    check_cached(depA22, head, none),
    check_cached(depA3, head, none),
   	check_cached(orgB, head, [root]),
   	check_cached(depB1, head, [orgB, root]),

   	{ok, [depA2, orgA, u01, root]} = find_role_objs(head, depA22, #{}),
   	ok.



%%%%%%%%%%%%%%%%%%%%%%  Util %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	


check_cached(ObjId, Role, none) ->
	none = get_cache(ObjId, Role);

check_cached(ObjId, Role, List) ->
	Pid = get_cache(ObjId, Role),
	case nkrole_role_cache:get_obj_ids(Pid, #{timeout=>5000}) of
		{ok, List} -> 
			ok;
		{ok, Other} -> 
			lager:warning("\nList: ~p\nOthe: ~p", [List, Other]),
			error(?LINE)
	end.


get_cache(ObjId, Role) ->
	{ok, Pid} = get_proxy(ObjId, #{}),
	Caches = gen_server:call(Pid, get_caches),
	maps:get(Role, Caches, none).


