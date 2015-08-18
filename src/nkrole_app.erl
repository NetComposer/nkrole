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

%% @doc NkROLE OTP Application Module
-module(nkrole_app).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(application).

-export([start/0, start/2, stop/1]).
-export([get/1, put/2, del/1]).

-include("nkrole.hrl").
-include_lib("nklib/include/nklib.hrl").

-define(APP, nkrole).

-compile({no_auto_import, [get/1, put/2]}).

%% ===================================================================
%% Private
%% ===================================================================

%% @doc Starts stand alone.
-spec start() -> 
    ok | {error, Reason::term()}.

start() ->
    nkdist_util:ensure_dir(),
    case nklib_util:ensure_all_started(?APP, temporary) of
        {ok, _Started} -> ok;
        {error, Error} -> {error, Error}
    end.


%% @private OTP standard start callback
start(_Type, _Args) ->
    ConfigSpec = #{
        backend => atom,
        proxy_timeout => {integer, 1, none},
        cache_timeout => {integer, 1, none}
    },
    Defaults = #{
        backend => ets,
        proxy_timeout => 180000,
        cache_timeout => 180000
    },
    case nklib_config:load_env(?APP, ?APP, Defaults, ConfigSpec) of
        ok ->
            {ok, Vsn} = application:get_key(?APP, vsn),
            lager:notice("NkROLE v~s is starting", [Vsn]),
            nkrole_sup:start_link();
        {error, Error} ->
            lager:error("Error parsing config: ~p", [Error]),
            error(Error)
    end.


%% @private OTP standard stop callback
stop(_) ->
    ok.


%% @doc gets a configuration value
get(Key) ->
    get(Key, undefined).


%% @doc gets a configuration value
get(Key, Default) ->
    nklib_config:get(?APP, Key, Default).


%% @doc updates a configuration value
put(Key, Value) ->
    nklib_config:put(?APP, Key, Value).


%% @doc updates a configuration value
del(Key) ->
    nklib_config:del(?APP, Key).


