-ifndef(_jaeger__thrift__types_included).
-define(_jaeger__thrift__types_included, yeah).

-define(JAEGER_TAGTYPE_STRING, 0).
-define(JAEGER_TAGTYPE_DOUBLE, 1).
-define(JAEGER_TAGTYPE_BOOL, 2).
-define(JAEGER_TAGTYPE_LONG, 3).
-define(JAEGER_TAGTYPE_BINARY, 4).

-define(JAEGER_SPANREFTYPE_CHILD_OF, 0).
-define(JAEGER_SPANREFTYPE_FOLLOWS_FROM, 1).

%% struct 'Jaeger.Thrift.Tag'

-record('Jaeger.Thrift.Tag', {'key' :: string() | binary(),
                              'vType' :: integer(),
                              'vStr' :: string() | binary() | 'undefined',
                              'vDouble' :: float() | 'undefined',
                              'vBool' :: boolean() | 'undefined',
                              'vLong' :: integer() | 'undefined',
                              'vBinary' :: string() | binary() | 'undefined'}).
-type 'Jaeger.Thrift.Tag'() :: #'Jaeger.Thrift.Tag'{}.

%% struct 'Jaeger.Thrift.Log'

-record('Jaeger.Thrift.Log', {'timestamp' :: integer(),
                              'fields' = [] :: list()}).
-type 'Jaeger.Thrift.Log'() :: #'Jaeger.Thrift.Log'{}.

%% struct 'Jaeger.Thrift.SpanRef'

-record('Jaeger.Thrift.SpanRef', {'refType' :: integer(),
                                  'traceIdLow' :: integer(),
                                  'traceIdHigh' :: integer(),
                                  'spanId' :: integer()}).
-type 'Jaeger.Thrift.SpanRef'() :: #'Jaeger.Thrift.SpanRef'{}.

%% struct 'Jaeger.Thrift.Span'

-record('Jaeger.Thrift.Span', {'traceIdLow' :: integer(),
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
-type 'Jaeger.Thrift.Span'() :: #'Jaeger.Thrift.Span'{}.

%% struct 'Jaeger.Thrift.Process'

-record('Jaeger.Thrift.Process', {'serviceName' :: string() | binary(),
                                  'tags' :: list() | 'undefined'}).
-type 'Jaeger.Thrift.Process'() :: #'Jaeger.Thrift.Process'{}.

%% struct 'Jaeger.Thrift.Batch'

-record('Jaeger.Thrift.Batch', {'process' = #'Jaeger.Thrift.Process'{} :: 'Jaeger.Thrift.Process'(),
                                'spans' = [] :: list()}).
-type 'Jaeger.Thrift.Batch'() :: #'Jaeger.Thrift.Batch'{}.

%% struct 'Jaeger.Thrift.BatchSubmitResponse'

-record('Jaeger.Thrift.BatchSubmitResponse', {'ok' :: boolean()}).
-type 'Jaeger.Thrift.BatchSubmitResponse'() :: #'Jaeger.Thrift.BatchSubmitResponse'{}.

-endif.
