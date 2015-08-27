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

%% @doc NkROLE User Functions
-module(nkrole).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_roles/2, get_role_objs/3]).
-export([find_role_objs/3, has_role/4]).
-export([add_role/4, add_subrole/5, del_role/4, del_subrole/5]).
-export([stop/1]).

-export_type([obj_id/0, role/0, role_spec/0, role_map/0]).
-export_type([opts/0, get_rolemap_fun/0]).


%% ===================================================================
%% Types
%% ===================================================================

-type obj_id() :: term().

-type role() :: term().

-type role_spec() :: obj_id() | #{role() => obj_id()}.

-type role_map() :: #{role() => [role_spec()]}.

-type get_rolemap_fun() :: fun((obj_id()) -> {ok, role_map()} | {error, term()}).

-type opts() ::
    #{
        timeout => pos_integer() | infinity,
        proxy_timeout => pos_integer() | infinity,
        cache_timeout => pos_integer() | infinity,
        get_rolemap_fun => get_rolemap_fun()
    }.



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Gets an object's roles
-spec get_roles(obj_id(), opts()) ->
    {ok, [role()]} | {error, term()}.

get_roles(ObjId, Opts) ->
    nkrole_proxy:do_call(ObjId, get_roles, Opts).


%% @doc Gets an object's direct roles
-spec get_role_objs(role(), obj_id(), opts()) ->
    {ok, [role_spec()]} | {error, term()}.

get_role_objs(Role, ObjId, Opts) ->
    nkrole_proxy:do_call(ObjId, {get_role_objs, Role}, Opts).


%% @doc Gets an object's roles iterating at all levels
-spec find_role_objs(role(), obj_id(), opts()) ->
    {ok, [obj_id()]} | {error, term()}.

find_role_objs(Role, ObjId, Opts) ->
    case nkrole_proxy:get_cache(Role, ObjId, Opts) of
        {ok, Pid} ->
            nkrole_cache:get_obj_ids(Pid, Opts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Gets an object's roles iterating at all levels
-spec has_role(obj_id(), role(), obj_id(), opts()) ->
    {ok, [obj_id()]} | {error, term()}.

has_role(ObjId, Role, Target, Opts) ->
    case nkrole_proxy:get_cache(Role, Target, Opts) of
        {ok, Pid} ->
            nkrole_cache:has_obj_id(Pid, ObjId, Opts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Adds a role to an object
-spec add_role(role(), obj_id(), obj_id(), opts()) ->
    ok | {error, term()}.

add_role(Role, Base, ObjId, Opts) ->
    nkrole_proxy:do_call(Base, {add_role, Role, ObjId}, Opts).


%% @doc Adds a role to an object
-spec add_subrole(role(), obj_id(), role(), 
                  obj_id(), opts()) ->
    ok | {error, term()}.

add_subrole(Role, Base, SubRole, ObjId, Opts) ->
    Spec = maps:put(SubRole, ObjId, #{}),
    nkrole_proxy:do_call(Base, {add_role, Role, Spec}, Opts).


%% @doc Adds a role to an object
-spec del_role(role(), obj_id(), obj_id(), opts()) ->
    ok | {error, term()}.

del_role(Role, Base, ObjId, Opts) ->
    nkrole_proxy:do_call(Base, {del_role, Role, ObjId}, Opts).


%% @doc Adds a role to an object
-spec del_subrole(role(), obj_id(), role(), 
                  obj_id(), opts()) ->
    ok | {error, term()}.

del_subrole(Role, Base, SubRole, ObjId, Opts) ->
    Spec = maps:put(SubRole, ObjId, #{}),
    nkrole_proxy:do_call(Base, {del_role, Role, Spec}, Opts).


%% @doc
-spec stop(nkrole:obj_id()) ->
    ok | {error, term()}.

stop(ObjId) ->
    nkrole_proxy:stop(ObjId).


