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
%% @doc Exports spans to Jaeger
%% @end
%%%------------------------------------------------------------------------
%%%
-module(oc_reporter_jaeger).

-include_lib("opencensus/include/opencensus.hrl").

-include("gen/Jaeger.Thrift.Agent.hrl").

-behaviour(oc_reporter).

-ifdef(OTP_RELEASE).
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_INFO(Format, Args), error_logger:info_msg(Format, Args)).
-define(LOG_ERROR(Format, Args), error_logger:error_msg(Format, Args)).
-endif.

-define(DEFAULT_HOSTNAME, "localhost").
-define(DEFAULT_PORT, 6831).
-define(DEFAULT_PROTOCOL, compact).
-define(DEFAULT_TRANSPORT, oc_reporter_jaeger_transport_udp).
%% -define(DEFAULT_COLLECTOR, undefined). %% http://localhost:14268/api/traces
-define(DEFAULT_SERVICE_NAME, atom_to_binary(node(), utf8)).
-define(DEFAULT_TAGS, []).

-export([init/1,
         report/2]).

-record(state, {agent,
                process}).

init(Opts) ->
  Hostname = jaeger_hostname(Opts),
  Port = jaeger_port(Opts),
  ProtocolModule = jaeger_protocol(Opts),
  TransportModule = ?DEFAULT_TRANSPORT,

  {ok, Transport} = TransportModule:new(Hostname, Port),
  {ok, BufferedTransport} = thrift_buffered_transport:new(Transport),
  {ok, Protocol} = ProtocolModule:new(BufferedTransport),
  {ok, Agent} = thrift_client:new(Protocol, 'Jaeger.Thrift.Agent'),

  Process = #'Jaeger.Thrift.Process'{'serviceName' = ?DEFAULT_SERVICE_NAME,
                                     tags = ?DEFAULT_TAGS},

  #state{agent = Agent,
         process = Process}.

report(Spans, #state{agent = Agent,
                     process = Process}) ->
  _ = [send_jaeger_span(Span, Agent, Process) || Span <- Spans],
  ok.

send_jaeger_span(Span, Agent, Process) ->
  thrift_client:call(Agent, 'emitBatch', [#'Jaeger.Thrift.Batch'{process = Process,
                                                                 spans = make_spans(Span)}]).

make_spans(Span) ->
  %% (optional_fields(Span))#{
  %%                          <<"traceId">> => iolist_to_binary(io_lib:format("~32.16.0b", [Span#span.trace_id])),
  %%                          <<"name">> => iolist_to_binary(Span#span.name),
  %%                          <<"id">> => iolist_to_binary(io_lib:format("~16.16.0b", [Span#span.span_id])),
  %%                          <<"timestamp">> => wts:to_absolute(Span#span.start_time),
  %%                          <<"duration">> => wts:duration(Span#span.start_time, Span#span.end_time),
  %%                          <<"debug">> => false, %% TODO: get from attributes?
  %%                          <<"shared">> => false, %% TODO: get from attributes?
  %%                          <<"localEndpoint">> => LocalEndpoint,
  %%                          %% <<"remoteEndpoint">> =>  %% TODO: get from attributes?
  %%                          <<"annotations">> => to_annotations(Span#span.time_events),
  %%                          <<"tags">> => to_tags(Span#span.attributes) %% TODO: merge with oc_tags?
  %%                         }.

  TraceId = Span#span.trace_id,
  <<High:64, Low:64>> = <<TraceId:128/integer>>,
  
  [#'Jaeger.Thrift.Span'{'traceIdLow' = Low,
                         'traceIdHigh' = High,
                         'spanId' = Span#span.span_id,
                         'parentSpanId' = Span#span.parent_span_id,
                         'operationName' = Span#span.name,
                         'flags' = Span#span.trace_options,
                         'startTime' = wts:to_absolute(Span#span.start_time),
                         'duration' = wts:duration(Span#span.start_time, Span#span.end_time)}].
  

%% to_annotations(TimeEvents) ->
%%   to_annotations(TimeEvents, []).

%% to_annotations([], Annotations) ->
%%   Annotations;
%% to_annotations([{Timestamp, #annotation{description=Description,
%%                                         attributes=Attributes}} | Rest], Annotations) ->
%%   to_annotations(Rest, [#{<<"timestamp">> => wts:to_absolute(Timestamp),
%%                           <<"value">> => annotation_value(Description, Attributes)} | Annotations]);
%% to_annotations([{Timestamp, MessageEvent=#message_event{}} | Rest], Annotations) ->
%%   to_annotations(Rest, [#{<<"timestamp">> => wts:to_absolute(Timestamp),
%%                           <<"value">> => annotation_value(MessageEvent)} | Annotations]).

%% annotation_value(Description, Attributes) ->
%%   AttrString = lists:join(", ", [[Key, "=", to_string(Value)] ||
%%                                   {Key, Value} <- maps:to_list(Attributes)]),
%%   iolist_to_binary([Description, " Attributes:{", AttrString, "}"]).

%% annotation_value(#message_event{type=Type,
%%                                 id=Id,
%%                                 uncompressed_size=UncompressedSize,
%%                                 compressed_size=CompressedSize}) ->
%%   iolist_to_binary(["MessageEvent:{type=", atom_to_binary(Type, utf8),
%%                     ", id=", integer_to_binary(Id),
%%                     ", uncompressed_size=", integer_to_binary(UncompressedSize),
%%                     ", compressed_size=", integer_to_binary(CompressedSize), "}"]).

jaeger_hostname(Options) ->
  proplists:get_value(hostname, Options, ?DEFAULT_HOSTNAME).

jaeger_port(Options) ->
  proplists:get_value(port, Options, ?DEFAULT_PORT).

jaeger_protocol(Options) ->
  case proplists:get_value(protocol, Options, ?DEFAULT_PROTOCOL) of
    binary ->
      thrift_binary_protocol;
    compact ->
      thrift_compact_protocol;
    Protocol ->
      Protocol
  end.

%% to_string(Value) when is_function(Value) ->
%%   to_string(Value());
%% to_string(Value) when is_list(Value) ;
%%                       is_binary(Value) ->
%%   Value;
%% to_string(Value) ->
%%   io_lib:format("~p", [Value]).

%% to_tag(_Name, Value) when is_function(Value) ->
%%   Value();
%% to_tag(_Name, Value) when is_list(Value) ->
%%   list_to_binary(Value);
%% to_tag(_Name, Value) ->
%%   Value.

%% to_tags(Attributes) ->
%%   maps:map(fun(Name, Value) ->
%%                to_tag(Name, Value)
%%            end, Attributes).

%% optional_fields(Span) ->
%%   lists:foldl(fun(Field, Acc) ->
%%                   case span_field(Field, Span) of
%%                     undefined ->
%%                       Acc;
%%                     Value ->
%%                       maps:put(Field, Value, Acc)
%%                   end
%%               end, #{}, [<<"kind">>, <<"parentId">>]).

%% span_field(<<"parentId">>, #span{parent_span_id=undefined}) ->
%%   undefined;
%% span_field(<<"parentId">>, #span{parent_span_id=ParentId}) ->
%%   iolist_to_binary(io_lib:format("~16.16.0b", [ParentId]));
%% span_field(<<"kind">>, #span{kind=?SPAN_KIND_UNSPECIFIED}) ->
%%   undefined;
%% span_field(<<"kind">>, #span{kind=?SPAN_KIND_SERVER}) ->
%%   <<"SERVER">>;
%% span_field(<<"kind">>, #span{kind=?SPAN_KIND_CLIENT}) ->
%%   <<"CLIENT">>.
