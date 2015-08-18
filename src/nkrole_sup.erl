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

%% @private NkROLE  main supervisor
-module(nkrole_sup).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(supervisor).

-export([init/1, start_link/0]).
% -export([start_tasks_sup/0]).

-include("nkrole.hrl").

%% @private
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    ChildsSpec = 
        [
            {nkrole_store,
                {nkrole_store, start_link, []},
                permanent,
                5000,
                worker,
                [nkrole_store]}
        ],
    supervisor:start_link({local, ?MODULE}, ?MODULE, {{one_for_one, 10, 60}, 
                          ChildsSpec}).


%% @private
init(ChildSpecs) ->
    {ok, ChildSpecs}.


% %% @private
% start_tasks_sup() ->
%     supervisor:start_link({local, nkrole_tasks_sup}, 
%                           ?MODULE, {{one_for_one, 10, 60}, []}).


