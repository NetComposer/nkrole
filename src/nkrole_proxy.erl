%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc Every time an object is referred, a proxy process is started, with a timeout value
-module(nkrole_proxy).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([get_proxy/2, proxy_op/3, stop/1, get_all/0, get_all_caches/0, stop_all/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export_type([op/0]).


-include("nkrole.hrl").

-define(CALL_TRIES, 3).

%% See nkrole.erl
-type op() ::
    get_roles |
    {get_role_objs, nkrole:role()} |
    {has_role, nkrole:obj_id(), nkrole:role()} |
    {has_elements, nkrole:role(), sets:set(pid())} |
    {get_cached, nkrole:role()} | 
    {add_role, nkrole:role(), nkrole:role_spec()} |
    {del_role, nkrole:role(), nkrole:role_spec()} |
    {set_role, nkrole:role(), [nkrole:role_spec()]}.



-define(DEBUG(Txt, Args, State),
    case State#state.debug of
        true -> ?LLOG(debug, Txt, Args, State);
        _ -> ok
    end).


-define(LLOG(Type, Txt, Args, State),
    lager:Type(
        [
            {obj_id, State#state.obj_id}
        ],
        "NkROLE proxy (~p): "++Txt, [State#state.obj_id | Args])).


-define(LLOG(Type, Txt, Args),
    lager:Type("NkROLE proxy: "++Txt, Args)).




%% ===================================================================
%% Public
%% ===================================================================


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
            start_link(ObjId, Opts)
    end.


%% @private
-spec proxy_op(nkrole:obj_id(), op(), nkrole:opts()) ->
    {ok, pid()} | {ok, term(), pid()} | {error, term()}.

proxy_op(ObjId, Op, Opts) ->
    do_call(ObjId, Op, Opts).


%% @private
-spec get_all() ->
    [{nkrole:obj_id(), pid()}].

get_all() ->
    nklib_proc:values(?MODULE).


%% @private
-spec get_all_caches() ->
    [{nkrole:obj_id(), pid(), [{nkrole:role(), pid()}]}].

get_all_caches() ->
    lists:map(
        fun({ObjId, Pid}) -> {ObjId, Pid, gen_server:call(Pid, get_caches)} end,
        get_all()).


%% @doc Stops a proxy
-spec stop(nkrole:obj_id()) ->
    ok | {error, term()}.

stop(ObjId) ->
    do_cast(ObjId, stop, #{}).


%% @private Stops all proxies
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
    caches = [] :: [{nkrole:role(), pid()}],
    debug :: boolean()
}).


%% @private 
init({ObjId, Opts}) ->
    nklib_proc:put(?MODULE, ObjId),
    nklib_proc:put({?MODULE, ObjId}),
    ProxyTimeout = case maps:find(proxy_timeout, Opts) of
        {ok, Time} ->
            Time;
        error ->
            nkrole_app:get(proxy_timeout)
    end,
    GetFun = nkrole_backend:get_rolemap_fun(Opts),
    case GetFun(ObjId) of
        {ok, RoleMap} ->
            State = #state{
                obj_id = ObjId, 
                rolemap = RoleMap,
                proxy_timeout = ProxyTimeout, 
                get_fun = GetFun,
                caches = [],
                debug = maps:get(debug, Opts, false) == true
            },
            ?DEBUG("started (~p)", [self()], State),
            {ok, State, ProxyTimeout};
        {error, not_found} ->
            ignore;
        {error, Error} ->
            {stop, {obj_get_error, {Error, ObjId}}}
    end.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}, non_neg_integer()} | {reply, term(), #state{}, non_neg_integer()}.

handle_call(get_roles, _From, #state{rolemap=RoleMap}=State) ->
    reply({ok, maps:keys(RoleMap)}, State);

handle_call({get_role_objs, Role}, _From, #state{rolemap=RoleMap}=State) ->
    reply({ok, maps:get(Role, RoleMap, [])}, State);

handle_call({has_role, ObjId, Role}, From, State) ->
    {ok, Pid, State2} = get_cache(Role, State),
    spawn_link(
        fun() ->
            Reply = nkrole_cache:has_obj_id(Pid, ObjId),
            gen_server:reply(From, Reply)
        end),
    noreply(State2);

handle_call({has_elements, Role, Path}, {CallerPid, _}=From, State) ->
    case get_cache(Role, Path, CallerPid, State) of
        {ok, Pid, State2} ->
            spawn_link(
                fun() ->
                    Reply = case nkrole_cache:has_elements(Pid) of
                        {ok, true} -> {ok, {true, Pid}};
                        {ok, false} -> {ok, {false, Pid}};
                        {error, Error} -> {error, Error}
                    end,
                    gen_server:reply(From, Reply)
                end),
            noreply(State2);
        {error, Error} ->
            reply({error, Error}, State)
    end;

handle_call({get_cached, Role}, From, State) ->
    {ok, Pid, State2} = get_cache(Role, State),
    spawn_link(
        fun() ->
            Reply = case nkrole_cache:get_cached(Pid) of
                {ok, ObjIds, Pids} -> {ok, {Pid, ObjIds, Pids}};
                {error, Error} -> {error, Error}
            end,
            gen_server:reply(From, Reply)
        end),
    noreply(State2);

handle_call({set_role, Role, Specs}, _From, #state{rolemap=RoleMap}=State) ->
    State2 = case maps:get(Role, RoleMap, []) of
        Specs ->
            State;
        _Old ->
            RoleMap2 = RoleMap#{Role=>Specs},
            invalidate_role(Role, State#state{rolemap=RoleMap2})
    end,
    reply(ok, State2);

handle_call({add_role, Role, Spec}, _From, #state{rolemap=RoleMap}=State) ->
    RoleObjs = maps:get(Role, RoleMap, []),
    State2 = case lists:member(Spec, RoleObjs) of
        true ->
            State;
        false ->
            RoleObjs2 = [Spec|RoleObjs],
            RoleMap2 = RoleMap#{Role=>RoleObjs2},
            invalidate_role(Role, State#state{rolemap=RoleMap2})
    end,
    reply(ok, State2);

handle_call({del_role, Role, Spec}, _From, #state{rolemap=RoleMap}=State) ->
    RoleObjs = maps:get(Role, RoleMap, []),
    State2 = case lists:member(Spec, RoleObjs) of
        true ->
            RoleObjs2 = RoleObjs -- [Spec],
            RoleMap2 = RoleMap#{Role=>RoleObjs2},
            invalidate_role(Role, State#state{rolemap=RoleMap2});
        false ->
            State
    end,
    reply(ok, State2);

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

terminate(_Reason, State) ->
    ?DEBUG("stopped proxy (~p)", [self()], State).



%% ===================================================================
%% Internal
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
        rolemap = RoleMap,
        caches = Caches, 
        get_fun = GetFun,
        proxy_timeout = ProxyTimeout
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
            Opts = #{
                proxy_timeout => ProxyTimeout,
                get_rolemap_fun => GetFun
            },
            {ok, Pid} = nkrole_cache:start_link(RoleList, self(), Path, Opts),
            ?DEBUG("started cache for ~p (~p)", [xRole, Pid], State),
            monitor(process, Pid),
            {ok, Pid, State#state{caches=[{Role, Pid}|Caches]}}
    end.


%% @private
-spec stop_cache(pid(), #state{}) ->
    #state{}.

stop_cache(Pid, #state{caches=Caches}=State) ->
    case lists:keytake(Pid, 2, Caches) of
        {value, {Role, Pid}, Caches2} ->
            ?DEBUG("stop cache for ~p (~p)", [Role, Pid], State),
            nkrole_cache:stop(Pid),
            State#state{caches=Caches2};
        false ->
            State
    end.


%% @private
-spec invalidate_role(nkrole:role(), #state{}) ->
    #state{}.

invalidate_role(Role, #state{caches=Caches}=State) ->
    case lists:keyfind(Role, 1, Caches) of
        {Role, Pid} ->
            ?DEBUG("invalidating role ~p (~p)", [Role, Caches], State),
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
-spec do_call(nkrole:obj_id(), op(), nkrole:opts()) ->
    {ok, pid()} | {ok, term(), pid()} | {error, term()}.

do_call(ObjId, Op, Opts) ->
    do_call(ObjId, Op, Opts, ?CALL_TRIES).


%% @private
-spec do_call(nkrole:obj_id(), op(), nkrole:opts(), pos_integer()) ->
    {ok, pid()} | {ok, term(), pid()} | {error, term()}.

do_call(ObjId, Op, Opts, Tries) ->
    case get_proxy(ObjId, Opts) of
        {ok, ProxyPid} ->
            % Check if the proxy has stopped just after getting its pid
            Timeout = maps:get(op_timeout, Opts, 5000),
            case nklib_util:call(ProxyPid, Op, Timeout) of
                ok ->
                    {ok, ProxyPid};
                {ok, Reply} ->
                    {ok, Reply, ProxyPid};
                {error, {exit, {{timeout, _}, _}}} ->
                    {error, timeout};
                {error, {exit, _}} when Tries > 1 ->
                    ?LLOG(info, "call exit (~p), retrying", [Op]),
                    timer:sleep(100),
                    do_call(ObjId, Op, Opts, Tries-1);
                {error, Error} ->
                    {error, Error}
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









