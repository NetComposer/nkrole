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

%% @doc Obj Proxy
-module(nkrole_proxy).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(nkdist_proc).
-behaviour(gen_server).

-export([get_obj/2, get_roles/2, get_role_objs/3]).
-export([find_role_objs/3, has_role/4]).
-export([add_role/4, add_subrole/5, del_role/4, del_subrole/5]).
-export([get_cache/3, get_proxy/2, stop_obj/1, stop_all/0]).
-export([start/2, start_and_join/2, join/2]).
-export([do_init/2, init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-include("nkrole.hrl").

-type call_opts() ::
    #{
        timeout => pos_integer() | infinity,
        proxy_timeout => pos_integer() | infinity,
        cache_timeout => pos_integer() | infinity
    }.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Gets an object, starting a proxy
-spec get_obj(nkrole:obj_id(), call_opts()) ->
    {ok, nkrole:obj()} | {error, term()}.

get_obj(ObjId, Opts) ->
    do_call(ObjId, get_obj, Opts).


%% @doc Gets an object's roles
-spec get_roles(nkrole:obj_id(), call_opts()) ->
    {ok, [nkrole:role()]} | {error, term()}.

get_roles(ObjId, Opts) ->
    do_call(ObjId, get_roles, Opts).


%% @doc Gets an object's direct roles
-spec get_role_objs(nkrole:role(), nkrole:obj_id(), call_opts()) ->
    {ok, [nkrole:role_spec()]} | {error, term()}.

get_role_objs(Role, ObjId, Opts) ->
    do_call(ObjId, {get_role_objs, Role}, Opts).


%% @doc Gets an object's roles iterating at all levels
-spec find_role_objs(nkrole:role(), nkrole:obj_id(), call_opts()) ->
    {ok, [nkrole:obj_id()]} | {error, term()}.

find_role_objs(Role, ObjId, Opts) ->
    case get_cache(Role, ObjId, Opts) of
        {ok, Pid} ->
            nkrole_role_cache:get_obj_ids(Pid, Opts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Gets an object's roles iterating at all levels
-spec has_role(nkrole:obj_id(), nkrole:role(), nkrole:obj_id(), call_opts()) ->
    {ok, [nkrole:obj_id()]} | {error, term()}.

has_role(ObjId, Role, Target, Opts) ->
    case get_cache(Role, Target, Opts) of
        {ok, Pid} ->
            nkrole_role_cache:has_obj_id(Pid, ObjId, Opts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Adds a role to an object
-spec add_role(nkrole:role(), nkrole:obj_id(), nkrole:obj_id(), call_opts()) ->
    ok | {error, term()}.

add_role(Role, Base, ObjId, Opts) ->
    do_call(Base, {add_role, Role, ObjId}, Opts).


%% @doc Adds a role to an object
-spec add_subrole(nkrole:role(), nkrole:obj_id(), nkrole:role(), 
                  nkrole:obj_id(), call_opts()) ->
    ok | {error, term()}.

add_subrole(Role, Base, SubRole, ObjId, Opts) ->
    do_call(Base, {add_role, Role, {SubRole, ObjId}}, Opts).


%% @doc Adds a role to an object
-spec del_role(nkrole:role(), nkrole:obj_id(), nkrole:obj_id(), call_opts()) ->
    ok | {error, term()}.

del_role(Role, Base, ObjId, Opts) ->
    do_call(Base, {del_role, Role, ObjId}, Opts).


%% @doc Adds a role to an object
-spec del_subrole(nkrole:role(), nkrole:obj_id(), nkrole:role(), 
                  nkrole:obj_id(), call_opts()) ->
    ok | {error, term()}.

del_subrole(Role, Base, SubRole, ObjId, Opts) ->
    do_call(Base, {del_role, Role, {SubRole, ObjId}}, Opts).


%% @doc
-spec stop_obj(nkrole:obj_id()) ->
    ok | {error, term()}.

stop_obj(ObjId) ->
    do_cast(ObjId, stop, #{}).


%% @doc
-spec stop(pid()) ->
    ok.

stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).


%% @doc
-spec stop_all() ->
    ok.

stop_all() ->
    {ok, List} = nkdist:get_procs(?MODULE),
    lists:foreach(fun({_ObjId, Pid}) -> stop(Pid) end, List).



% ===================================================================
%% nkdist_proc behaviour
%% ===================================================================


%% @doc Start a new process
-spec start(nkrole:node_id(), Args::term()) ->
    {ok, pid()} | {error, term()}.

start(ObjId, Opts) ->
    proc_lib:start_link(?MODULE, do_init, [ObjId, Opts]).


%% @doc Starts a new clone process
-spec start_and_join(nkrole:node_id(), pid()) ->
    {ok, pid()} | {error, term()}.

start_and_join(ObjId, Pid) ->
    stop(Pid),
    proc_lib:start_link(?MODULE, do_init, [ObjId, #{}]).


%% @doc Joins two existing processes
-spec join(Current::pid(), Old::pid()) ->
    ok | {error, term()}.

join(_Pid, OldPid) ->
    stop(OldPid).



% ===================================================================
%% gen_server behaviour
%% ===================================================================

-record(state, {
    obj_id :: nkrole:obj_id(),
    roles :: nkrole:role_map(),
    obj1 :: nkrole:obj(),
    proxy_timeout :: pos_integer() | infinity,
    cache_timeout :: pos_integer() | infinity,
    caches = #{} :: #{nkrole:role() => pid()}
}).


%% Not used
init([]) ->
    ok.


%% @private 
do_init(ObjId, Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    {Roles, Obj} = case nkrole_store:get_obj(ObjId) of
        {ok, Roles0, Obj0} -> {Roles0, Obj0};
        {error, not_found} -> {#{}, not_found}
    end,
    ProxyTimeout = case maps:find(proxy_timeout, Opts) of
        {ok, Timeout1} -> Timeout1;
        error -> nkrole_app:get(proxy_timeout)
    end,
    CacheTimeout = case maps:find(cache_timeout, Opts) of
        {ok, Timeout2} -> Timeout2;
        error -> nkrole_app:get(cache_timeout)
    end,
    State = #state{
        obj_id = ObjId, 
        roles = Roles,
        obj1 = Obj, 
        proxy_timeout = ProxyTimeout, 
        cache_timeout = CacheTimeout
    },
    lager:debug("Started proxy for ~p: ~p (~p)", [ObjId, Obj, self()]),
    gen_server:enter_loop(?MODULE, [], State, ProxyTimeout).


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}}.

handle_call(_, _From, #state{obj1=not_found}=State) ->
    reply({error, not_found}, State);

handle_call(get_obj, _From, #state{obj1=Obj}=State) ->
    reply({ok, Obj}, State);

handle_call(get_roles, _From, #state{roles=Roles}=State) ->
    reply({ok, maps:keys(Roles)}, State);

handle_call({get_role_objs, Role}, _From, #state{roles=Roles}=State) ->
    reply({ok, maps:get(Role, Roles, [])}, State);

handle_call({get_cache, Role, BasePids}, {CallerPid, _}, State) ->
    #state{obj_id=ObjId, roles=Roles, caches=Caches, cache_timeout=Timeout} = State,
    case maps:find(Role, Caches) of
        {ok, CallerPid} ->
            reply({error, looped1}, State);
        {ok, Pid} ->
            case gb_sets:is_element(Pid, BasePids) of
                false ->
                    reply({ok, Pid}, State);
                true ->
                    reply({error, looped2}, State)
            end;
        error ->
            RoleList = maps:get(Role, Roles, []),
            Opts = #{cache_timeout=>Timeout, base_pids=>BasePids},
            {ok, Pid} = nkrole_role_cache:start_link(ObjId, Role, RoleList, Opts),
            monitor(process, Pid),
            Caches1 = maps:put(Role, Pid, Caches),
            reply({ok, Pid}, State#state{caches=Caches1})
    end;

handle_call({add_role, Role, Spec}, _From, State) ->
    #state{obj_id=ObjId, roles=Roles, obj1=Obj} = State,
    RoleObjs = maps:get(Role, Roles, []),
    case lists:member(Spec, RoleObjs) of
        true ->
            reply(ok, State);
        false ->
            RoleObjs1 = [Spec|RoleObjs],
            Roles1 = maps:put(Role, RoleObjs1, Roles),
            ok = nkrole_store:put_obj(ObjId, Roles1, Obj),
            State1 = invalidate_role(Role, State#state{roles=Roles1}),
            reply(ok, State1)
    end;

handle_call({del_role, Role, Spec}, _From, State) ->
    #state{obj_id=ObjId, roles=Roles, obj1=Obj} = State,
    RoleObjs = maps:get(Role, Roles, []),
    case lists:member(Spec, RoleObjs) of
        true ->
            RoleObjs1 = RoleObjs -- [Spec],
            Roles1 = maps:put(Role, RoleObjs1, Roles),
            ok = nkrole_store:put_obj(ObjId, Roles1, Obj),
            State1 = invalidate_role(Role, State#state{roles=Roles1}),
            reply(ok, State1);
        false ->
            reply(ok, State)
    end;

handle_call(get_caches, _From, #state{caches=Caches}=State) ->
    reply(Caches, State);

handle_call(Msg, _From, State) -> 
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    noreply(State).


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Msg, State) -> 
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    noreply(State).


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{caches=Caches}=State) ->
    List = [{Role, RolePid} || {Role, RolePid} <- maps:to_list(Caches), RolePid/=Pid],
    noreply(State#state{caches=maps:from_list(List)});

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info(Info, State) -> 
    lager:warning("Module ~p received unexpected info: ~p (~p)", [?MODULE, Info, State]),
    noreply(State).


%% @private
-spec code_change(term(), #state{}, term()) ->
    {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
    ok.

terminate(_Reason, #state{obj_id=ObjId, caches=Caches}) ->  
    lists:foreach(
        fun({_Role, Pid}) -> gen_server:cast(Pid, stop) end,
        maps:to_list(Caches)),
    lager:debug("Stopped proxy for ~s", [ObjId]),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
invalidate_role(Role, #state{obj_id=ObjId, caches=Caches}=State) ->
    lager:debug("Invalidating role ~p at ~p (~p)", [Role, ObjId, Caches]),
    case maps:find(Role, Caches) of
        {ok, Pid} ->
            nkrole_role_cache:stop(Pid),
            State#state{caches=maps:remove(Role, Caches)};
        error -> 
            State
    end.


%% @private
reply(Reply, #state{proxy_timeout=Timeout}=State) ->
    {reply, Reply, State, Timeout}.


%% @private
noreply(#state{proxy_timeout=Timeout}=State) ->
    {noreply, State, Timeout}.


%% @doc Gets a role cache
-spec get_cache(nkrole:role(), nkrole:obj_id(), call_opts()) ->
    {ok, pid()} | {error, term()}.

get_cache(Role, ObjId, Opts) ->
    BasePids = maps:get(base_pids, Opts, gb_sets:new()),
    do_call(ObjId, {get_cache, Role, BasePids}, Opts).


%% @private
-spec do_call(nkrole:obj_id(), term(), call_opts()) ->
    {ok, term()} | {error, term()}.

do_call(ObjId, Op, Opts) ->
    case get_proxy(ObjId, Opts) of
        {ok, Pid} ->
            Timeout = maps:get(timeout, Opts, 5000),
            nklib_util:call(Pid, Op, Timeout);
        {error, Error} ->
            {error, Error}
    end.


%% @private
-spec do_cast(nkrole:obj_id(), term(), call_opts()) ->
    {ok, term()} | {error, term()}.

do_cast(ObjId, Op, Opts) ->
    case get_proxy(ObjId, Opts) of
        {ok, Pid} ->
            gen_server:cast(Pid, Op);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Returns current proxy for object or start a new one
-spec get_proxy(nkrole:obj_id(), call_opts()) ->
    {ok, pid()} | {error, term()}.

get_proxy(ObjId, Opts) ->
    case nkdist:find_proc(nkrole_proxy, ObjId) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_found} ->
            case nkdist:start_proc(nkrole_proxy, ObjId, Opts) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, {already_stated, Pid}} ->
                    {ok, Pid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.










