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

-export([get_proxy/2, proxy_op/3, stop/1, get_all/0, get_all_caches/0, stop_all/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export_type([op/0]).


-include("nkrole.hrl").

-define(CALL_TRIES, 3).

-type op() ::
    get_roles | {get_role_objs, nkrole:role()} |  
    {has_role, nkrole:obj_id(), nkrole:role()} |
    {has_elements, nkrole:role(), sets:set(pid())} |
    {get_cached, nkrole:role()} | 
    {add_role, nkrole:role(), nkrole:role_spec()} |
    {del_role, nkrole:role(), nkrole:role_spec()} |
    {set_role, nkrole:role(), [nkrole:role_spec()]}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Returns current proxy for object or start a new one
-spec get_proxy(nkrole:obj_id(), nkrole:opts()) ->
    {ok, pid()} | {error, term()}.

get_proxy(Pid, _Opts) when is_pid(Pid) ->
    {ok, Pid};

% get_proxy(_, #{proxy_pid:=Pid}) ->
%     {ok, Pid};

get_proxy(ObjId, Opts) ->
    case nklib_proc:values({?MODULE, ObjId}) of
        [{_, Pid}|_] -> 
            {ok, Pid};
        [] -> 
            start_link(ObjId, Opts)
    end.


%% @private
-spec proxy_op(nkrole:obj_id(), op(), nkrole:opts()) ->
    {ok, pid()} | {ok, term(), pid()} | {error, term()}.

proxy_op(ObjId, Op, Opts) ->
    proxy_op(ObjId, Op, Opts, ?CALL_TRIES).


%% @private
-spec proxy_op(nkrole:obj_id(), op(), nkrole:opts(), pos_integer()) ->
    {ok, pid()} | {ok, term(), pid()} | {error, term()}.

proxy_op(ObjId, Op, Opts, Tries) ->
    case get_proxy(ObjId, Opts) of
        {ok, ProxyPid} ->
            % Check if the proxy has stopped just after getting its pid
            case nklib_util:call(ProxyPid, Op, Opts) of
                ok ->
                    {ok, ProxyPid};
                {ok, Reply} ->
                    {ok, Reply, ProxyPid};
                {error, {exit, _}} when Tries > 1 ->
                    lager:notice("NkROLE Proxy call exit (~p), retrying", [Op]),
                    timer:sleep(100),
                    proxy_op(ObjId, Op, Opts, Tries-1);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
-spec get_all() ->
    [{nkrole:obj_id(), pid()}].

get_all() ->
    nklib_proc:values(?MODULE).


%% @private
-spec get_all_caches() ->
    [{nkrole:obj_id(), pid()}].

get_all_caches() ->
    lists:map(
        fun({ObjId, Pid}) -> {ObjId, Pid, gen_server:call(Pid, get_caches)} end,
        get_all()).


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


%% @doc Starts a new proxy
-spec start_link(nkrole:obj_id(), nkrole:opts()) ->
    {ok, pid()} | {error, term()}.

start_link(ObjId, Opts) ->
    case gen_server:start_link(?MODULE, {ObjId, Opts}, []) of
        {ok, Pid} -> {ok, Pid};
        ignore -> {error, not_found};
        {error, Error} -> {error, Error}
    end.


-record(state, {
    obj_id :: nkrole:obj_id(),
    rolemap :: nkrole:role_map(),
    proxy_timeout :: pos_integer() | infinity,
    get_fun :: nkrole:get_rolemap_fun(),
    caches = [] :: [{nkrole:role(), pid()}]
}).


%% @private 
init({ObjId, Opts}) ->
    nklib_proc:put(?MODULE, ObjId),
    nklib_proc:put({?MODULE, ObjId}),
    ProxyTimeout = case maps:find(proxy_timeout, Opts) of
        {ok, Timeout1} -> Timeout1;
        error -> nkrole_app:get(proxy_timeout)
    end,
    GetFun = nkrole_backend:get_rolemap_fun(Opts),
    case GetFun(ObjId) of
        {ok, RoleMap} ->
            State = #state{
                obj_id = ObjId, 
                rolemap = RoleMap,
                proxy_timeout = ProxyTimeout, 
                get_fun = GetFun,
                caches = []
            },
            lager:debug("NkROLE started proxy for ~p (~p)", [ObjId, self()]),
            {ok, State, ProxyTimeout};
        {error, not_found} ->
            ignore;
        {error, Error} ->
            {stop, {obj_get_error, {Error, ObjId}}}
    end.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}}.

handle_call(get_roles, _From, #state{rolemap=RoleMap}=State) ->
    reply({ok, maps:keys(RoleMap)}, State);

handle_call({get_role_objs, Role}, _From, #state{rolemap=RoleMap}=State) ->
    reply({ok, maps:get(Role, RoleMap, [])}, State);

handle_call({has_role, ObjId, Role}, From, State) ->
    {ok, Pid, State1} = get_cache(Role, State),
    spawn_link(
        fun() ->
            Reply = nkrole_cache:has_obj_id(Pid, ObjId),
            gen_server:reply(From, Reply)
        end),
    noreply(State1);

handle_call({has_elements, Role, Path}, {CallerPid, _}=From, State) ->
    case get_cache(Role, Path, CallerPid, State) of
        {ok, Pid, State1} ->
            spawn_link(
                fun() ->
                    Reply = case nkrole_cache:has_elements(Pid) of
                        {ok, true} -> {ok, {true, Pid}};
                        {ok, false} -> {ok, {false, Pid}};
                        {error, Error} -> {error, Error}
                    end,
                    gen_server:reply(From, Reply)
                end),
            noreply(State1);
        {error, Error} ->
            reply({error, Error}, State)
    end;

handle_call({get_cached, Role}, From, State) ->
    {ok, Pid, State1} = get_cache(Role, State),
    spawn_link(
        fun() ->
            Reply = case nkrole_cache:get_cached(Pid) of
                {ok, ObjIds, Pids} -> {ok, {Pid, ObjIds, Pids}};
                {error, Error} -> {error, Error}
            end,
            gen_server:reply(From, Reply)
        end),
    noreply(State1);

handle_call({set_role, Role, Specs}, _From, #state{rolemap=RoleMap}=State) ->
    State1 = case maps:get(Role, RoleMap, []) of
        Specs ->
            lager:warning("ROLE PROXY1"),
            State;
        _ ->
            lager:warning("ROLE PROXY2"),
            RoleMap1 = maps:put(Role, Specs, RoleMap),
            invalidate_role(Role, State#state{rolemap=RoleMap1})
    end,
    reply(ok, State1);

handle_call({add_role, Role, Spec}, _From, #state{rolemap=RoleMap}=State) ->
    RoleObjs = maps:get(Role, RoleMap, []),
    State1 = case lists:member(Spec, RoleObjs) of
        true ->
            State;
        false ->
            RoleObjs1 = [Spec|RoleObjs],
            RoleMap1 = maps:put(Role, RoleObjs1, RoleMap),
            invalidate_role(Role, State#state{rolemap=RoleMap1})
    end,
    reply(ok, State1);

handle_call({del_role, Role, Spec}, _From, #state{rolemap=RoleMap}=State) ->
    RoleObjs = maps:get(Role, RoleMap, []),
    State1 = case lists:member(Spec, RoleObjs) of
        true ->
            RoleObjs1 = RoleObjs -- [Spec],
            RoleMap1 = maps:put(Role, RoleObjs1, RoleMap),
            invalidate_role(Role, State#state{rolemap=RoleMap1});
        false ->
            State
    end,
    reply(ok, State1);

handle_call(get_caches, _From, #state{caches=Caches}=State) ->
    reply(Caches, State);

handle_call(Msg, _From, State) -> 
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    noreply(State).


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.

handle_cast({stop_cache, Pid}, State) ->
    noreply(stop_cache(Pid, State));

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Msg, State) -> 
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    noreply(State).


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    noreply(stop_cache(Pid, State));

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

terminate(_Reason, #state{obj_id=ObjId}) ->  
    lager:debug("NkROLE stopped proxy for ~p", [ObjId]).



%% ===================================================================
%% gen_server behaviour
%% ===================================================================

%% @private
-spec get_cache(nkrole:role(), #state{}) ->
    {ok, pid(), #state{}}.

%% @private
get_cache(Role, State) ->
    get_cache(Role, sets:new(), none, State).
    

%% @private
-spec get_cache(nkrole:role(), sets:set(pid()), pid()|none, #state{}) ->
    {ok, pid(), #state{}} | {error, term()}.

get_cache(Role, Path, CallerPid, State) ->
    #state{
        obj_id = ObjId,
        rolemap = RoleMap, 
        caches = Caches, 
        get_fun = GetFun
    } = State,
    case lists:keyfind(Role, 1, Caches) of
        {Role, CallerPid} ->
            {error, looped_call};
        {Role, Pid} ->
            case sets:is_element(Pid, Path) of
                false ->
                    {ok, Pid, State};
                true ->
                    {error, looped_path}
            end;
        false ->
            % lager:notice("Base pids: ~p", [sets:to_list(Path)]),
            RoleList = maps:get(Role, RoleMap, []),
            {ok, Pid} = nkrole_cache:start_link(RoleList, self(), GetFun, Path),
            lager:debug("NkROLE started cache for ~p ~p (~p)", [ObjId, Role, Pid]),
            monitor(process, Pid),
            {ok, Pid, State#state{caches=[{Role, Pid}|Caches]}}
    end.


%% @private
-spec stop_cache(pid(), #state{}) ->
    #state{}.

stop_cache(Pid, #state{obj_id=ObjId, caches=Caches}=State) ->
    case lists:keytake(Pid, 2, Caches) of
        {value, {Role, Pid}, Caches1} ->
            lager:debug("NkROLE proxy ~p stop cache for ~p (~p)", [ObjId, Role, Pid]),
            nkrole_cache:stop(Pid),
            Caches1 = lists:keydelete(Pid, 2, Caches),
            State#state{caches=Caches1};
        false ->
            State
    end.


%% @private
-spec invalidate_role(nkrole:role(), #state{}) ->
    #state{}.

invalidate_role(Role, #state{obj_id=ObjId, caches=Caches}=State) ->
    case lists:keyfind(Role, 1, Caches) of
        {Role, Pid} ->
            lager:debug("NkROLE invalidating role ~p at ~p (~p)", [Role, ObjId, Caches]),
            stop_cache(Pid, State);
        false ->
            State
    end.


%% @private
reply(Reply, #state{proxy_timeout=Timeout}=State) ->
    {reply, Reply, State, Timeout}.


%% @private
noreply(#state{proxy_timeout=Timeout}=State) ->
    {noreply, State, Timeout}.



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









