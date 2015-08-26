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

-export([get_roles/1, get_roles/2, put_roles/2, put_roles/3, get_proxy/2]).
-export([start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).


-callback get_roles(nkrole:obj_id()) ->
    {ok, nkrole:role_map()} | {error, not_found|term()}.

-callback put_roles(nkrole:obj_id(), nkrole:role_map()) ->
    ok | {error, term()}.

% Optional callback
% -callback get_proxy(nkrole:obj_id(), map()) ->
%     {ok, pid()} | {error, not_found|term()}.



%% ===================================================================
%% Public
%% ===================================================================


%% @doc
-spec start_link() ->
    {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc 
-spec get_roles(nkrole:obj_id()) ->
    {ok, nkrole:role_map()} | {error, not_found|term()}.

get_roles(ObjId) ->
    get_roles(ObjId, #{}).


%% @doc 
-spec get_roles(nkrole:obj_id(), nkrole:call_opts()) ->
    {ok, nkrole:role_map()} | {error, not_found|term()}.

get_roles(ObjId, Opts) ->
    do_get_roles(get_backend(Opts), ObjId).



%% @doc
-spec put_roles(nkrole:obj_id(), nkrole:role_map()) ->
    ok | {error, term()}.

put_roles(ObjId, Roles) ->
    put_roles(ObjId, Roles, #{}).


%% @doc
-spec put_roles(nkrole:obj_id(), nkrole:role_map(), nkrole:call_opts()) ->
    ok | {error, term()}.

put_roles(ObjId, Roles, Opts) ->
    do_put_roles(get_backend(Opts), ObjId, Roles).



%% @doc Returns current proxy for object or start a new one
-spec get_proxy(nkrole:obj_id(), nkrole_proxy:call_opts()) ->
    {ok, pid()} | {error, term()}.

get_proxy(ObjId, Opts) ->
    do_get_proxy(get_backend(Opts), ObjId, Opts).





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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @private
-spec do_get_roles(nkrole:backend(), nkrole:obj_id()) ->
    {ok, nkrole:role_map()} | {error, not_found|term()}.

do_get_roles(ets, ObjId) ->
    case ets:lookup(?MODULE, ObjId) of
        [{_, Roles}] -> {ok, Roles};
        [] -> {error, not_found}
    end;

do_get_roles(Module, ObjId) ->
    case Module:get_roles(ObjId) of
        {ok, Roles} -> {ok, Roles};
        {error, Error} -> {error, Error}
    end.


%% @doc
-spec do_put_roles(nkrole:backend(), nkrole:obj_id(), nkrole:role_map()) ->
    ok | {error, term()}.

do_put_roles(ets, ObjId, Roles) ->
    nkrole:stop(ObjId),
    ets:insert(?MODULE, {ObjId, Roles}),
    ok;


do_put_roles(Module, ObjId, Roles) ->
    nkrole:stop(ObjId),
    case Module:put_roles(ObjId, Roles) of
        ok -> ok;
        {error, Error} -> {error, Error}
    end.


%% @private
-spec do_get_proxy(nkrole:backend(), nkrole:obj_id(), nkrole_proxy:call_opts()) ->
    {ok, pid()} | {error, term()}.

do_get_proxy(ets, ObjId, Opts) ->
    case nklib_proc:values({nkrole_proxy, ObjId}) of
        [{_, Pid}|_] -> 
            {ok, Pid};
        [] -> 
            nkrole_proxy:start_link(ObjId, Opts)
    end;

do_get_proxy(Module, ObjId, Opts) ->
    case erlang:function_exported(Module, get_proxy, 2) of
        true ->
            case Module:get_proxy(ObjId, Opts) of
                {ok, Pid} -> {ok, Pid};
                {error, Error} -> {error, Error}
            end;
        false ->
            do_get_proxy(ets, ObjId, Opts)
    end.


%% @private 
get_backend(Opts) -> 
    case Opts of
        #{backend:=Module} -> Module;
        _ -> nkrole_app:get(backend)
    end.
        