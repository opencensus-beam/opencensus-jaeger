%%%------------------------------------------------------------------------
%% Copyright 2019, OpenCensus BEAM Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc Jaeger exporter UDP transport
%% @end
%%%------------------------------------------------------------------------

-module(oc_reporter_jaeger_transport_udp).

%% API
-export([new/1]).

%% thrift_transport callbacks
-export([write/2, flush/1, close/1]).

new({Host, Port}) ->
  {gen_udp:open(0), Host, Port}.

write({Socket, Host, Port} = State, Data) ->
  ok = gen_udp:send(Socket, Host, Port, Data),
  {State, ok}.

flush(State) ->
    {State, ok}.

close({Socket, _, _}) ->
  ok = gen_udp:close(Socket),
  {[], ok}.
