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
-define(DEFAULT_SERVICE_TAGS, undefined).

-export([init/1,
         report/2]).

-record(state, {agent,
                process}).

init(Opts) ->
  Hostname = jaeger_hostname(Opts),
  Port = jaeger_port(Opts),
  ProtocolModule = jaeger_protocol(Opts),
  TransportModule = ?DEFAULT_TRANSPORT,

  ServiceName = jaeger_service_name(Opts),
  ServiceTags = jaeger_service_tags(Opts),

  {ok, Transport} = TransportModule:new(Hostname, Port),
  {ok, BufferedTransport} = thrift_buffered_transport:new(Transport),
  {ok, Protocol} = ProtocolModule:new(BufferedTransport),
  {ok, Agent} = thrift_client:new(Protocol, 'Jaeger.Thrift.Agent'),

  Process = #'Jaeger.Thrift.Process'{'serviceName' = ServiceName,
                                     'tags' = ServiceTags},

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

  TraceId = Span#span.trace_id,
  %% traceIdLow and traceIdHigh are _signed_ 64-bit integers while `High` and `Low` are unsigned, so they need to
  %% converted to signed to not be truncated in the thrift protocol
  %% See https://github.com/jaegertracing/jaeger/issues/1951
  <<SignedTraceIdHigh:64/signed-integer, SignedTraceIdLow:64/signed-integer>> = <<TraceId:128/unsigned-integer>>,

  %% Likewise, the span IDs, `parentSpanId` and `spanId` are signed 64, not unsigned 64
  SpanId = Span#span.span_id,
  <<SignedSpanId:64/signed-integer>> = <<SpanId:64/unsigned-integer>>,

  ParentSpanId = case Span#span.parent_span_id of
                   undefined -> 0;
                   PSI -> PSI
                 end,
  <<SignedParentSpanId:64/signed-integer>> = <<ParentSpanId:64/unsigned-integer>>,

  [#'Jaeger.Thrift.Span'{'traceIdLow' = SignedTraceIdLow,
                         'traceIdHigh' = SignedTraceIdHigh,
                         'spanId' = SignedSpanId,
                         'parentSpanId' = SignedParentSpanId,
                         'operationName' = Span#span.name,
                         'references' = to_references(Span#span.links),
                         'flags' = Span#span.trace_options,
                         'startTime' = wts:to_absolute(Span#span.start_time),
                         'duration' = wts:duration(Span#span.start_time, Span#span.end_time),
                         'tags' = to_tags(Span#span.attributes),
                         'logs' = to_logs(Span#span.time_events)}].

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

jaeger_service_name(Options) ->
  proplists:get_value(service_name, Options, ?DEFAULT_SERVICE_NAME).

jaeger_service_tags(Options) ->
  case proplists:get_value(service_tags, Options, ?DEFAULT_SERVICE_TAGS) of
    undefined ->
      undefined;
    Tags -> to_tags(Tags)
  end.

to_references([]) ->
  undefined;
to_references(Links) ->
  [to_reference(Link) || Link <- Links].

to_reference(#link{type = Type,
                   trace_id = TraceId,
                   span_id = SpanId}) ->

  <<High:64, Low:64>> = <<TraceId:128/integer>>,

  RefType = case Type of
              'TYPE_UNSPECIFIED' -> undefined;
              'CHILD_LINKED_SPAN' -> 0; %% 'CHILD_OF';
              'PARENT_LINKED_SPAN' -> 1 %%  'FOLLOWS_FROM'
            end,

  #'Jaeger.Thrift.SpanRef'{
     'refType' = RefType,
     'traceIdHigh' = High,
     'traceIdLow' = Low,
     'spanId' = SpanId}.

to_tag(Name, Value) when is_function(Value) ->
  to_tag(Name, Value());
to_tag(Name, Value) when is_list(Value) ->
  #'Jaeger.Thrift.Tag'{'key' = Name,
                       'vType' = 0, %% STRING
                       'vStr' = Value};
to_tag(Name, Value) when is_binary(Value) ->
  #'Jaeger.Thrift.Tag'{'key' = Name,
                       'vType' = 0, %% STRING
                       'vStr' = Value};
to_tag(Name, Value) when is_atom(Value) ->
  #'Jaeger.Thrift.Tag'{'key' = Name,
                       'vType' = 0, %% STRING
                       'vStr' = atom_to_binary(Value, utf8)};
to_tag(Name, Value) when is_float(Value) ->
  #'Jaeger.Thrift.Tag'{'key' = Name,
                       'vType' = 1, %% DOUBLE
                       'vDouble' = Value};
to_tag(Name, true) ->
  #'Jaeger.Thrift.Tag'{'key' = Name,
                       'vType' = 2, %% BOOL
                       'vBool' = true};
to_tag(Name, false) ->
  #'Jaeger.Thrift.Tag'{'key' = Name,
                       'vType' = 2, %% BOOL
                       'vBool' = false};
to_tag(Name, Value) when is_integer(Value) ->
  #'Jaeger.Thrift.Tag'{'key' = Name,
                       'vType' = 3, %% LONG
                       'vLong' = Value}.

to_tags(Map) when map_size(Map) == 0 ->
  undefined;
to_tags(Attributes) ->
  maps:values(maps:map(fun(Name, Value) ->
                           to_tag(Name, Value)
                       end, Attributes)).

to_log({Timestamp, #annotation{description = Description,
                               attributes = Attributes}}) ->
  Fields0 = case map_size(Attributes) of
              0 ->
                [];
              _ -> to_tags(Attributes)
            end,
  Fields = [#'Jaeger.Thrift.Tag'{'key' = <<"message">>,
                                 'vType' = 0, %% STRING
                                 'vStr' = Description}] ++ Fields0,
  {true, #'Jaeger.Thrift.Log'{timestamp = wts:to_absolute(Timestamp),
                              fields = Fields}};
to_log({_Timestamp, _MessageEvent}) ->
  false.

to_logs([]) ->
  undefined;
to_logs(TimeEvents) ->
  lists:filtermap(fun to_log/1, TimeEvents).
