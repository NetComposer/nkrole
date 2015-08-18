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


-module(nkrole_store).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([start_link/0]).
-export([get_obj/1, put_obj/3]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-callback get_obj(nkrole:obj_id()) ->
    {ok, nkrole:role_map(), nkrole:obj()} | {error, not_found|term()}.

-callback put_obj(nkrole:obj_id(), nkrole:role_map(), nkrole:obj()) ->
    ok | {error, term()}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
-spec start_link() ->
    {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc 
-spec get_obj(nkrole:obj_id()) ->
    {ok, nkrole:role_map(), nkrole:obj()} | {error, not_found|term()}.

get_obj(ObjId) ->
    case nkrole_app:get(backend) of
        ets ->
            case ets:lookup(?MODULE, ObjId) of
                [{_, Roles, Obj}] -> {ok, Roles, Obj};
                [] -> {error, not_found}
            end;
        riak_core ->
            case riak_core_metadata:get({?MODULE, none}, ObjId) of
                {Roles, Obj} -> {ok, Roles, Obj};
                undefined -> {error, not_found}
            end;
        Module ->
            case Module:get_obj(ObjId) of
                {ok, Roles, Data} -> {ok, Roles, Data};
                {error, Error} -> {error, Error}
            end
    end.


%% @doc
-spec put_obj(nkrole:obj_id(), nkrole:role_map(), nkrole:obj()) ->
    ok | {error, term()}.

put_obj(ObjId, Roles, Obj) ->
    case nkrole_app:get(backend) of
        ets ->
            ets:insert(?MODULE, {ObjId, Roles, Obj}),
            ok;
        riak_core ->
            ok = riak_core_metadata:put({?MODULE, none}, ObjId, {Roles, Obj});
        Module ->
            case Module:put_obj(ObjId, Roles, Obj) of
                ok -> ok;
                {error, Error} -> {error, Error}
            end
    end.



% ===================================================================
%% gen_server behaviour
%% ===================================================================


-record(state, {
    backend :: ets | riak_core | atom()
}).


%% @private 
-spec init(term()) ->
    {ok, #state{}}.

init([]) ->
    ets:new(?MODULE, [named_table, public]),
    {ok, #state{backend=nkrole_app:get(backend)}}.


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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


