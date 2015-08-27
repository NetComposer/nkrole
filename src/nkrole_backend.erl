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


-module(nkrole_backend).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([get_rolemap/1, put_rolemap/2, get_rolemap_fun/1]).
-export([start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).



%% ===================================================================
%% Public
%% ===================================================================


%% @doc
-spec start_link() ->
    {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc 
-spec get_rolemap(nkrole:obj_id()) ->
    {ok, nkrole:role_map()} | {error, not_found}.

get_rolemap(ObjId) ->
    case ets:lookup(?MODULE, ObjId) of
        [{_, Roles}] -> {ok, Roles};
        [] -> {error, not_found}
    end.


%% @doc 
-spec put_rolemap(nkrole:obj_id(), nkrole:rolemap()) ->
    {ok, nkrole:role_map()} | {error, not_found|term()}.

put_rolemap(ObjId, RoleMap) ->
    nkrole:stop(ObjId),
    ets:insert(?MODULE, {ObjId, RoleMap}),
    ok.


%% @private
-spec get_rolemap_fun(map()) ->
    nkrole:get_rolemap_fun().

get_rolemap_fun(#{get_rolemap_fun:=Fun}) -> Fun;
get_rolemap_fun(_) -> fun get_rolemap/1.



% ===================================================================
%% gen_server behaviour
%% ===================================================================


-record(state, {
}).


%% @private 
-spec init(term()) ->
    {ok, #state{}}.

init([]) ->
    ets:new(?MODULE, [named_table, public]),
    {ok, #state{}}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}}.

handle_call(Msg, _From, State) -> 
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.

handle_cast(Msg, State) -> 
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.

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

