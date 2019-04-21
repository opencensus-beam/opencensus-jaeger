-ifndef(_jaeger_types_included).
-define(_jaeger_types_included, yeah).

-define(JAEGER_TAGTYPE_STRING, 0).
-define(JAEGER_TAGTYPE_DOUBLE, 1).
-define(JAEGER_TAGTYPE_BOOL, 2).
-define(JAEGER_TAGTYPE_LONG, 3).
-define(JAEGER_TAGTYPE_BINARY, 4).

-define(JAEGER_SPANREFTYPE_CHILD_OF, 0).
-define(JAEGER_SPANREFTYPE_FOLLOWS_FROM, 1).

%% struct 'Jaeger_Thrift.Tag'

-record('Jaeger_Thrift.Tag', {'key' :: string() | binary(),
                              'vType' :: integer(),
                              'vStr' :: string() | binary() | 'undefined',
                              'vDouble' :: float() | 'undefined',
                              'vBool' :: boolean() | 'undefined',
                              'vLong' :: integer() | 'undefined',
                              'vBinary' :: string() | binary() | 'undefined'}).
-type 'Jaeger_Thrift.Tag'() :: #'Jaeger_Thrift.Tag'{}.

%% struct 'Jaeger_Thrift.Log'

-record('Jaeger_Thrift.Log', {'timestamp' :: integer(),
                              'fields' = [] :: list()}).
-type 'Jaeger_Thrift.Log'() :: #'Jaeger_Thrift.Log'{}.

%% struct 'Jaeger_Thrift.SpanRef'

-record('Jaeger_Thrift.SpanRef', {'refType' :: integer(),
                                  'traceIdLow' :: integer(),
                                  'traceIdHigh' :: integer(),
                                  'spanId' :: integer()}).
-type 'Jaeger_Thrift.SpanRef'() :: #'Jaeger_Thrift.SpanRef'{}.

%% struct 'Jaeger_Thrift.Span'

-record('Jaeger_Thrift.Span', {'traceIdLow' :: integer(),
                               'traceIdHigh' :: integer(),
                               'spanId' :: integer(),
                               'parentSpanId' :: integer(),
                               'operationName' :: string() | binary(),
                               'references' :: list() | 'undefined',
                               'flags' :: integer(),
                               'startTime' :: integer(),
                               'duration' :: integer(),
                               'tags' :: list() | 'undefined',
                               'logs' :: list() | 'undefined'}).
-type 'Jaeger_Thrift.Span'() :: #'Jaeger_Thrift.Span'{}.

%% struct 'Jaeger_Thrift.Process'

-record('Jaeger_Thrift.Process', {'serviceName' :: string() | binary(),
                                  'tags' :: list() | 'undefined'}).
-type 'Jaeger_Thrift.Process'() :: #'Jaeger_Thrift.Process'{}.

%% struct 'Jaeger_Thrift.Batch'

-record('Jaeger_Thrift.Batch', {'process' = #'Jaeger_Thrift.Process'{} :: 'Jaeger_Thrift.Process'(),
                                'spans' = [] :: list()}).
-type 'Jaeger_Thrift.Batch'() :: #'Jaeger_Thrift.Batch'{}.

%% struct 'Jaeger_Thrift.BatchSubmitResponse'

-record('Jaeger_Thrift.BatchSubmitResponse', {'ok' :: boolean()}).
-type 'Jaeger_Thrift.BatchSubmitResponse'() :: #'Jaeger_Thrift.BatchSubmitResponse'{}.

-endif.
