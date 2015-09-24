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
%% Each role cache is stored at the process dictionary

-module(nkrole_cache).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([start_link/4, get_cached/1, has_elements/1, has_obj_id/2, stop/1, get_all/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-include("nkrole.hrl").


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Starts a new cache
-spec start_link([nkrole:role_spec()], pid(), sets:set(pid()), nkrole:opts()) ->
    {ok, pid()}.

start_link(RoleSpecs, ProxyPid, Path, Opts) ->
    proc_lib:start_link(?MODULE, init, [{RoleSpecs, ProxyPid, Path, Opts}]).


%% @private
%% We need big timeouts for everything, because we can call right after
%% a creation start, that can last for a long time.
-spec get_cached(pid()) ->
    {ok, [nkrole:obj_id()], [pid()]} | {error, term()}.

get_cached(Pid) ->
    nklib_util:call(Pid, get_cached, #{timeout=>180000}).


%% @private
-spec has_elements(pid()) ->
    {ok, boolean()} | {error, term()}.

has_elements(Pid) ->
    nklib_util:call(Pid, has_elements, #{timeout=>180000}).


%% @private
-spec has_obj_id(pid(), nkrole:obj_id()) ->
    {ok, boolean()} | {error, term()}.

has_obj_id(Pid, ObjId) ->
    nklib_util:call(Pid, {has_obj_id, ObjId}, #{timeout=>180000}).


%% @doc Stops a cache
-spec stop(pid()) ->
    ok.

stop(Pid) ->
    gen_server:cast(Pid, stop).


%% @private
get_all() ->
    nklib_proc:values(?MODULE).



% ===================================================================
%% gen_server behaviour
%% ===================================================================

-record(state, {
    proxy :: pid(),
    obj_set :: sets:set(nkrole:obj_id()),
    cache_pids :: [pid()]
}).


%% @private
-spec init({[nkrole:role_spec()], pid(), sets:set(pid()), nkrole:opts()}) ->
    ok.

init({RoleSpecs, ProxyPid, Path, Opts}) ->
    ok = proc_lib:init_ack({ok, self()}),
    nklib_proc:put(?MODULE, ProxyPid),
    {ObjIdSet, CachePids} = find_objs(RoleSpecs, Path, Opts),
    monitor(process, ProxyPid),
    State = #state{
        proxy = ProxyPid,
        obj_set = ObjIdSet, 
        cache_pids = CachePids
    },
    gen_server:enter_loop(?MODULE, [], State).


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}}.

handle_call(get_cached, _From, #state{obj_set=ObjIdSet, cache_pids=Pids}=State) ->
    {reply, {ok, sets:to_list(ObjIdSet), Pids}, State};

handle_call(has_elements, _From, #state{obj_set=ObjIdSet, cache_pids=Pids}=State) ->
    Reply = Pids /= [] orelse sets:size(ObjIdSet) /= 0,
    {reply, {ok, Reply}, State};

handle_call({has_obj_id, ObjId}, From, #state{obj_set=ObjIdSet, cache_pids=Pids}=State) ->
    case sets:is_element(ObjId, ObjIdSet) of
        true ->
            {reply, {ok, true}, State};
        false ->
            spawn_link(fun() -> has_obj_id(ObjId, Pids, From) end),
            {noreply, State}
    end;

handle_call(Msg, _From, State) -> 
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Msg, State) -> 
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{proxy=Pid}=State) ->
    {stop, normal, State};

%% This way, if we have to stop, it enters in the proxy gen_server queue and 
%% no cache order to proxy is lost.
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, #state{proxy=ProxyPid}=State) ->
    gen_server:cast(ProxyPid, {stop_cache, self()}),
    {noreply, State};

handle_info(Info, State) -> 
    lager:warning("Module ~p received unexpected info: ~p (~p)", [?MODULE, Info, State]),
    {noreply, State}.


%% @private
-spec code_change(term(), #state{}, term()) ->
    {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
    ok.

terminate(_Reason, _State) ->  
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
find_objs(RoleSpecs, Path, Opts) ->
    Path2 = sets:add_element(self(), Path),
    find_objs(RoleSpecs, [], [], Path2, Opts).

    
%% @private
find_objs([], ObjIds, Pids, _Path, _Opts) ->
    {sets:from_list(ObjIds), lists:reverse(Pids)};

find_objs([Map|Rest], ObjIds, Pids, Path, Opts) when is_map(Map), map_size(Map)==1 ->
    [{SubRole, ObjId}] = maps:to_list(Map),
    case find_subrole(SubRole, ObjId, Path, Opts) of
        {ok, Bool, CachePid} ->
            monitor(process, CachePid),
            Path1 = sets:add_element(CachePid, Path),
            CachePids1 = case Bool of 
                true ->
                    lager:debug("Adding ~p (~p): ~p", 
                                 [{SubRole, ObjId}, self(), CachePid]),
                    [CachePid|Pids];
                false ->
                    lager:debug("Monitoring ~p (~p): ~p", 
                                 [{SubRole, ObjId}, self(), CachePid]),
                    Pids
            end,
            find_objs(Rest, ObjIds, CachePids1, Path1, Opts);
         error ->
            find_objs(Rest, ObjIds, Pids, Path, Opts)
    end;

find_objs([ObjId|Rest], ObjIds, Pids, Path, Opts) ->
    lager:debug("Adding ~p (~p)", [ObjId, self()]),
    find_objs(Rest, [ObjId|ObjIds], Pids, Path, Opts).


%% @private
find_subrole(SubRole, ObjId, Path, Opts) ->
    Opts1 = Opts#{timeout=>180000},
    case nkrole_proxy:proxy_op(ObjId, {has_elements, SubRole, Path}, Opts1) of
        {ok, {true, CachePid}, _ProxyPid} ->
            {ok, true, CachePid};
        {ok, {false, CachePid}, _ProxyPid} ->
            {ok, false, CachePid};
        {error, Error} ->
            lager:notice("Error getting obj ~p: ~p", [ObjId, Error]),
            error
    end.


%% @private 
has_obj_id(_ObjId, [], From) ->
    gen_server:reply(From, {ok, false});

has_obj_id(ObjId, [Pid|Rest], From) ->
    case catch gen_server:call(Pid, {has_obj_id, ObjId}, 180000) of
        {ok, true} -> 
            gen_server:reply(From, {ok, true});
        {ok, false} ->
            has_obj_id(ObjId, Rest, From);
        {'EXIT', Error} ->
            {error, Error}
    end.



