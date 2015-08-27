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

-export([start_link/4, stop/1, get_obj_ids/2, has_obj_id/3]).
-export([get_all/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-include("nkrole.hrl").

-define(MINIMUM_SET, 1000).

-type start_opts() ::
    #{
        cache_timeout => timeout(),
        base_pids => gb_sets:set()
    }.


-type call_opts() ::
    #{
        timeout => timeout()
    }.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Starts a new cache
-spec start_link(nkrole:obj_id(), nkrole:role(), [nkrole:role_spec()], start_opts()) ->
    {ok, pid()}.

start_link(ObjId, Role, RoleSpecs, Opts) ->
    proc_lib:start_link(?MODULE, init, [{ObjId, Role, RoleSpecs, Opts}]).


%% @doc Stops a cache
-spec stop(pid()) ->
    ok.

stop(Pid) ->
    gen_server:cast(Pid, stop).


%% @doc Gets the list of objects
-spec get_obj_ids(pid(), call_opts()) ->
    {ok, [nkrole:obj_id()]} | {error, term()}.

get_obj_ids(Pid, Opts) ->
    nklib_util:call(Pid, get_obj_ids, Opts).


%% @doc Gets the list of objects
-spec has_obj_id(pid(), nkrole:obj_id(), call_opts()) ->
    {ok, [nkrole:obj_id()]} | {error, term()}.

has_obj_id(Pid, ObjId, Opts) ->
    nklib_util:call(Pid, {has_obj_id, ObjId}, Opts).


%% @private
get_all() ->
    nklib_proc:values(?MODULE).



% ===================================================================
%% gen_server behaviour
%% ===================================================================

-record(state, {
    obj_ids :: [nkrole:obj_id()],
    length :: integer(),
    set :: gb_sets:set(),
    timeout :: pos_integer()
}).


%% @private
-spec init({nkrole:obj_id(), nkrole:role(), [nkrole:role_spec()], start_opts()}) ->
    ok.

init({ObjId, Role, RoleSpecs, Opts}) ->
    ok = proc_lib:init_ack({ok, self()}),
    BasePids1 = maps:get(base_pids, Opts, gb_sets:new()),
    BasePids2 = gb_sets:add_element(self(), BasePids1),
    lager:debug("Started cache for ~p ~p (~p)", [Role, ObjId, self()]),
    % lager:notice("Base pids: ~p", [gb_sets:to_list(BasePids2)]),
    ObjIds = find_objs(RoleSpecs, [], BasePids2),
    Length = length(ObjIds),
    nklib_proc:put(?MODULE, {ObjId, Role, Length}),
    Timeout = case maps:find(cache_timeout, Opts) of
        {ok, Timeout0} -> Timeout0;
        error -> nkrole_app:get(cache_timeout)
    end,
    State = #state{obj_ids=ObjIds, length=Length, timeout=Timeout},
    gen_server:enter_loop(?MODULE, [], State, Timeout).


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}}.

handle_call(get_obj_ids, From, #state{obj_ids=ObjIds}=State) ->
    gen_server:reply(From, {ok, ObjIds}),
    noreply(State);

handle_call({has_obj_id, ObjId}, From, #state{set=undefined, obj_ids=ObjIds}=State) ->
    #state{obj_ids=ObjIds, length=Length} = State,
    case Length > ?MINIMUM_SET of
        true ->
            Set = gb_sets:from_list(ObjIds),
            handle_call({has_obj_id, ObjId}, From, State#state{set=Set});
        false ->
            gen_server:reply(From, {ok, has_obj_id(ObjId, ObjIds)}),
            noreply(State)
    end;

handle_call({has_obj_id, ObjId}, From, #state{set=Set}=State) ->
    gen_server:reply(From, {ok, gb_sets:is_member(ObjId, Set)}),
    noreply(State);

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

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    {stop, normal, State};

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

terminate(_Reason, _State) ->  
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @private
find_objs([], ObjIds, _History) ->
    lists:flatten(lists:reverse(ObjIds));

find_objs([Map|Rest], ObjIds, PidsSet) when is_map(Map) ->
    case maps:to_list(Map) of
        [{SubRole, ObjId}] ->
            case gb_sets:is_element(ObjId, PidsSet) of
                true ->
                    lager:notice("Obj ~p is in path, skipping", [ObjId]),
                    find_objs(Rest, ObjIds, PidsSet);
                false ->
                    case find_subrole(SubRole, ObjId, PidsSet) of
                        {ok, SubObjIds, CachePid} ->
                            PidsSet1 = gb_sets:add_element(CachePid, PidsSet),
                            monitor(process, CachePid),
                            find_objs(Rest, [SubObjIds|ObjIds], PidsSet1);
                        error ->
                            find_objs(Rest, ObjIds, PidsSet)
                    end
            end;
        _ ->
            lager:warning("Invalid role spec: ~p", [Map]),
            find_objs(Rest, ObjIds, PidsSet)
    end;

find_objs([ObjId|Rest], ObjIds, PidsSet) ->
    find_objs(Rest, [ObjId|ObjIds], PidsSet).


%% @private
find_subrole(SubRole, ObjId, PidsSet) ->
    case catch nkrole_proxy:get_cache(SubRole, ObjId, #{base_pids=>PidsSet}) of
        {ok, CachePid} ->
            case get_obj_ids(CachePid, #{timeout=>180000}) of
                {ok, ObjIds} ->
                    {ok, ObjIds, CachePid};
                Error ->
                    lager:notice("Error getting obj ~p: ~p", [ObjId, Error]),
                    error
            end;
        Error ->
            lager:notice("Error getting obj ~p: ~p", [ObjId, Error]),
            error
    end.


%% @private 
has_obj_id(_ObjId, []) -> false;
has_obj_id(ObjId, [ObjId|_]) -> true;
has_obj_id(ObjId, [_|Rest]) -> has_obj_id(ObjId, Rest).


%% @private
noreply(#state{timeout=Timeout}=State) ->
    {noreply, State, Timeout}.

