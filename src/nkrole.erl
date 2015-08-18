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

%% @doc NkROLE User Functions
-module(nkrole).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export_type([obj_id/0, obj_class/0, obj_ref/0, obj_data/0, obj/0]).
-export_type([role/0, role_spec/0, role_map/0]).
-export_type([backend/0]).

%% ===================================================================
%% Types
%% ===================================================================

-type obj_id() ::term().

-type obj_class() :: term().

-type obj_ref() :: term().

-type obj_data() :: term().

-type role() :: term().

-type role_spec() :: obj_id() | {role(), obj_id()}.

-type role_map() :: #{nkrole:role() => [nkrole:role_spec()]}.

-type obj() :: term().

-type backend() :: ets | riak_core | atom().


%% ===================================================================
%% Public
%% ===================================================================


