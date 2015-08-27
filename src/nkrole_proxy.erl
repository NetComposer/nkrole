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
-behaviour(gen_server).

-export([start_link/3, start/3]).
-export([get_cache/3, get_proxy/2, do_call/3, do_cast/3, stop/1, stop_all/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-include("nkrole.hrl").

-define(CALL_TRIES, 5).


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Starts a new proxy
-spec start_link(nkrole:obj_id(), nkrole:role_map(), nkrole:opts()) ->
    {ok, pid()} | {error, term()}.

start_link(ObjId, RoleMap, Opts) ->
    gen_server:start_link(?MODULE, {ObjId, RoleMap, Opts}, []).


%% @doc Starts a new proxy
-spec start(nkrole:obj_id(), nkrole:role_map(), nkrole:opts()) ->
    {ok, pid()} | {error, term()}.

start(ObjId, RoleMap, Opts) ->
    gen_server:start_link(?MODULE, {ObjId, RoleMap, Opts}, []).


%% @doc Gets a role cache
-spec get_cache(nkrole:role(), nkrole:obj_id(), nkrole:opts()) ->
    {ok, pid()} | {error, term()}.

get_cache(Role, ObjId, Opts) ->
    BasePids = maps:get(base_pids, Opts, gb_sets:new()),
    do_call(ObjId, {get_cache, Role, BasePids}, Opts).


%% @private
-spec do_call(nkrole:obj_id(), term(), nkrole:opts()) ->
    {ok, term()} | {error, term()}.

do_call(ObjId, Op, Opts) ->
    do_call(ObjId, Op, Opts, ?CALL_TRIES).


%% @private
-spec do_call(nkrole:obj_id(), term(), nkrole:opts(), pos_integer()) ->
    {ok, term()} | {error, term()}.

do_call(ObjId, Op, Opts, Tries) ->
    case get_proxy(ObjId, Opts) of
        {ok, Pid} ->
            % If the proxy stops just after getting its pid
            case nklib_util:call(Pid, Op, Opts) of
                {error, {exit, _}} when Tries > 1 ->
                    lager:notice("NkROLE Proxy call exit (~p), retrying", [Op]),
                    timer:sleep(100),
                    do_call(ObjId, Op, Opts, Tries-1);
                Other ->
                    Other
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
-spec do_cast(nkrole:obj_id(), term(), nkrole:opts()) ->
    {ok, term()} | {error, term()}.

do_cast(ObjId, Op, Opts) ->
    case get_proxy(ObjId, Opts) of
        {ok, Pid} ->
            gen_server:cast(Pid, Op);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Returns current proxy for object or start a new one
-spec get_proxy(nkrole:obj_id(), nkrole:opts()) ->
    {ok, pid()} | {error, term()}.

get_proxy(Pid, _Opts) when is_pid(Pid) ->
    {ok, Pid};

get_proxy(ObjId, Opts) ->
    case nklib_proc:values({?MODULE, ObjId}) of
        [{_, Pid}|_] -> 
            {ok, Pid};
        [] -> 
            Fun = nkrole_backend:get_rolemap_fun(Opts),
            case Fun(ObjId) of
                {ok, RoleMap} ->
                    start_link(ObjId, RoleMap, Opts);
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc
-spec stop(nkrole:obj_id()) ->
    ok | {error, term()}.

stop(ObjId) ->
    do_cast(ObjId, stop, #{}).


%% @private Stops all proxies, only for ets backend
-spec stop_all() ->
    ok.

stop_all() ->
    lists:foreach(
        fun({_ObjId, Pid}) -> stop(Pid) end, 
        nklib_proc:values(?MODULE)).



%% ===================================================================
%% gen_server behaviour
%% ===================================================================


-record(state, {
    obj_id :: nkrole:obj_id(),
    rolemap :: nkrole:role_map(),
    proxy_timeout :: pos_integer() | infinity,
    cache_timeout :: pos_integer() | infinity,
    get_fun :: nkrole:get_rolemap_fun(),
    caches = #{} :: #{nkrole:role() => pid()}
}).


%% @private 
init({ObjId, RoleMap, Opts}) ->
    nklib_proc:put(?MODULE, ObjId),
    nklib_proc:put({?MODULE, ObjId}),
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
        rolemap = RoleMap,
        proxy_timeout = ProxyTimeout, 
        cache_timeout = CacheTimeout,
        get_fun = nkrole_backend:get_rolemap_fun(Opts)
    },
    lager:debug("Started proxy for ~p (~p)", [ObjId, self()]),
    {ok, State, ProxyTimeout}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}}.

handle_call(get_roles, _From, #state{rolemap=RoleMap}=State) ->
    reply({ok, maps:keys(RoleMap)}, State);

handle_call({get_role_objs, Role}, _From, #state{rolemap=RoleMap}=State) ->
    reply({ok, maps:get(Role, RoleMap, [])}, State);

handle_call({get_cache, Role, BasePids}, {CallerPid, _}, State) ->
    #state{
        obj_id = ObjId, 
        rolemap = RoleMap, 
        caches = Caches, 
        cache_timeout = Timeout,
        get_fun = GetFun
    } = State,
    case maps:find(Role, Caches) of
        {ok, CallerPid} ->
            reply({error, looped_call}, State);
        {ok, Pid} ->
            case gb_sets:is_element(Pid, BasePids) of
                false ->
                    reply({ok, Pid}, State);
                true ->
                    reply({error, looped_path}, State)
            end;
        error ->
            % lager:notice("Staring proxy for ~p (~p)", [ObjId, self()]),
            % lager:notice("Base pids: ~p", [gb_sets:to_list(BasePids)]),
            RoleList = maps:get(Role, RoleMap, []),
            Opts = #{
                cache_timeout => Timeout, 
                base_pids => BasePids,
                get_rolemap_fun => GetFun
            },
            {ok, Pid} = nkrole_cache:start_link(ObjId, Role, RoleList, Opts),
            monitor(process, Pid),
            Caches1 = maps:put(Role, Pid, Caches),
            reply({ok, Pid}, State#state{caches=Caches1})
    end;

handle_call({add_role, Role, Spec}, _From, #state{rolemap=RoleMap}=State) ->
    RoleObjs = maps:get(Role, RoleMap, []),
    case lists:member(Spec, RoleObjs) of
        true ->
            reply(ok, State);
        false ->
            RoleObjs1 = [Spec|RoleObjs],
            RoleMap1 = maps:put(Role, RoleObjs1, RoleMap),
            State1 = invalidate_role(Role, State#state{rolemap=RoleMap1}),
            reply(ok, State1)
    end;

handle_call({del_role, Role, Spec}, _From, #state{rolemap=RoleMap}=State) ->
    RoleObjs = maps:get(Role, RoleMap, []),
    case lists:member(Spec, RoleObjs) of
        true ->
            RoleObjs1 = RoleObjs -- [Spec],
            RoleMap1 = maps:put(Role, RoleObjs1, RoleMap),
            State1 = invalidate_role(Role, State#state{rolemap=RoleMap1}),
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
    lager:debug("Stopped proxy for ~s", [ObjId]).



%% ===================================================================
%% gen_server behaviour
%% ===================================================================


%% @private
invalidate_role(Role, #state{obj_id=ObjId, caches=Caches}=State) ->
    lager:debug("Invalidating role ~p at ~p (~p)", [Role, ObjId, Caches]),
    case maps:find(Role, Caches) of
        {ok, Pid} ->
            nkrole_cache:stop(Pid),
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











