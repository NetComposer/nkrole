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
-export([proxy_op/3]).

-export_type([obj_id/0, role/0, role_spec/0, role_map/0]).
-export_type([opts/0, get_rolemap_fun/0]).


%% ===================================================================
%% Types
%% ===================================================================

-type obj_id() :: term().

-type role() :: term().

-type role_spec() :: obj_id() | #{role() => obj_id()}.

-type role_map() :: #{role() => [role_spec()]}.

-type get_rolemap_fun() :: fun((obj_id()) -> {ok, role_map()} | {error, not_found|term()}).

-type opts() ::
    #{
        timeout => pos_integer() | infinity,
        proxy_timeout => pos_integer() | infinity,
        get_rolemap_fun => get_rolemap_fun()
        % proxy_pid => pid()
    }.



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Gets an object's roles
-spec get_roles(obj_id(), opts()) ->
    {ok, [role()]} | {error, term()}.

get_roles(ObjId, Opts) ->
    proxy_op(ObjId, get_roles, Opts).


%% @doc Gets an object's direct roles
-spec get_role_objs(role(), obj_id(), opts()) ->
    {ok, [role_spec()]} | {error, term()}.

get_role_objs(Role, ObjId, Opts) ->
    proxy_op(ObjId, {get_role_objs, Role}, Opts).


%% @doc Check if ObjId has Role over Target
-spec has_role(obj_id(), role(), obj_id(), opts()) ->
    {ok, [obj_id()]} | {error, term()}.

has_role(ObjId, Role, Target, Opts) ->
    proxy_op(Target, {has_role, ObjId, Role}, Opts).


%% @doc Gets all nested objects having a role over and object
-spec find_role_objs(role(), obj_id(), opts()) ->
    {ok, [obj_id()]} | {error, term()}.

find_role_objs(Role, ObjId, Opts) ->
    case proxy_op(ObjId, {get_cached, Role}, Opts) of
        {ok, {_CachePid, ObjIds, Pids}} ->
            lager:debug("FIRST: ~p, ~p", [ObjIds, Pids]),
            case find_role_objs(Role, ObjId, Opts, Pids, ObjIds) of
                {ok, List} -> 
                    {ok, lists:flatten(lists:reverse(List))};
                {error, Error} -> 
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
-spec find_role_objs(role(), obj_id(), opts(), [pid()], [obj_id()]) ->
    {ok, [obj_id()]} | {error, term()}.

find_role_objs(_Role, _ObjId, _Opts, [], Acc) ->
    {ok, Acc};

find_role_objs(Role, ObjId, Opts, [Pid|Rest], Acc) ->
    case nkrole_cache:get_cached(Pid) of
        {ok, SubObjIds, SubPids} ->
            lager:debug("CALL FOR ~p: ~p, ~p", [Pid, SubObjIds, SubPids]),
            case find_role_objs(Role, ObjId, Opts, SubPids, [SubObjIds|Acc]) of
                {ok, Acc2} ->
                    find_role_objs(Role, ObjId, Opts, Rest, Acc2);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Adds a role to an object
-spec add_role(role(), obj_id(), obj_id(), opts()) ->
    ok | {error, term()}.

add_role(Role, Base, ObjId, Opts) ->
    proxy_op(Base, {add_role, Role, ObjId}, Opts).


%% @doc Objects having SubRole over ObjId have Role over Base
-spec add_subrole(role(), obj_id(), role(), obj_id(), opts()) ->
    ok | {error, term()}.

add_subrole(Role, Base, SubRole, ObjId, Opts) ->
    Spec = maps:put(SubRole, ObjId, #{}),
    proxy_op(Base, {add_role, Role, Spec}, Opts).


%% @doc Adds a role to an object
-spec del_role(role(), obj_id(), obj_id(), opts()) ->
    ok | {error, term()}.

del_role(Role, Base, ObjId, Opts) ->
    proxy_op(Base, {del_role, Role, ObjId}, Opts).


%% @doc Adds a role to an object
-spec del_subrole(role(), obj_id(), role(), obj_id(), opts()) ->
    ok | {error, term()}.

del_subrole(Role, Base, SubRole, ObjId, Opts) ->
    Spec = maps:put(SubRole, ObjId, #{}),
    proxy_op(Base, {del_role, Role, Spec}, Opts).


%% @doc
-spec stop(nkrole:obj_id()) ->
    ok | {error, term()}.

stop(ObjId) ->
    nkrole_proxy:stop(ObjId).


%% ===================================================================
%% Public
%% ===================================================================


%% @private
-spec proxy_op(obj_id(), nkrole_proxy:op(), opts()) ->
    ok | {ok, term()} | {error, term()}.

proxy_op(ObjId, Op, Opts) ->
    case nkrole_proxy:proxy_op(ObjId, Op, Opts) of
        {ok, _ProxyPid} -> ok;
        {ok, Reply, _ProxyPid} -> {ok, Reply};
        {error, Error} -> {error, Error}
    end.



