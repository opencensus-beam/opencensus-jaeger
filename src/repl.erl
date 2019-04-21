-module(repl).

-export([run/0]).

-export([write/2]).
-export([flush/1]).
-export([close/1]).

-include("gen/jaeger_types.hrl").

run() ->
  {ok, Socket} = gen_udp:open(0),
  {ok, Transport} = thrift_transport:new(?MODULE, [Socket]),
  {ok, BufferedTransport} = thrift_buffered_transport:new(Transport),
  {ok, Protocol} = thrift_binary_protocol:new(BufferedTransport),
  {ok, Client} = thrift_client:new(Protocol, agent_thrift),
  
  thrift_client:call(Client, 'emitBatch', [#'Jaeger_Thrift.Batch'{process = make_process(),
                                                                 spans = make_spans()}]).

make_process() ->
  #'Jaeger_Thrift.Process'{'serviceName' = "repl",
                           tags = []}.

make_spans() ->
  [#'Jaeger_Thrift.Span'{'traceIdLow' = 1,
                         'traceIdHigh' = 10,
                         'spanId' = 66,
                         'parentSpanId' = 6,
                         'operationName' = "test",
                         flags = 1,
                         'startTime' = timer:now_diff(erlang:timestamp(), {0, 0, 0}),
                         'duration' = 100}].

write([Socket], Data) ->
  ok = gen_udp:send(Socket, "localhost", 6832, Data),
  {[Socket], ok}.

flush(State) ->
    {State, ok}.

close([Socket]) ->
  ok = gen_udp:close(Socket),
  {[], ok}.

%% -export([run/0]).

%% run() ->
%%     read_eval_process().

%% read_eval_process() ->
%%     ocp:with_child_span("repl"),

%%     Line = read_line(),

%%     Annotation = oc_span:annotation( <<"Invoking process_line/1">>,
%%                                      #{<<"len">> => length(Line),
%%                                        <<"use">> => <<"repl">>}),
%%     ocp:add_time_event(Annotation),

%%     Out = process_line(Line),
%%     io:format("< ~s~n~n", [Out]),

%%     ocp:finish_span(),

%%     read_eval_process().

%% read_line() ->
%%     ocp:with_child_span("read_line"),
%%     try io:get_line("> ")
%%     after
%%         ocp:finish_span()
%%     end.

%% process_line(Line) ->
%%     ocp:with_child_span("process_line"),
%%     try string:uppercase(Line)
%%     after
%%         ocp:finish_span()
%%     end.
