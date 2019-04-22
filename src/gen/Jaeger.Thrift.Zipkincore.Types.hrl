-ifndef(_jaeger__thrift__zipkincore__types_included).
-define(_jaeger__thrift__zipkincore__types_included, yeah).

-define(ZIPKINCORE_ANNOTATIONTYPE_BOOL, 0).
-define(ZIPKINCORE_ANNOTATIONTYPE_BYTES, 1).
-define(ZIPKINCORE_ANNOTATIONTYPE_I16, 2).
-define(ZIPKINCORE_ANNOTATIONTYPE_I32, 3).
-define(ZIPKINCORE_ANNOTATIONTYPE_I64, 4).
-define(ZIPKINCORE_ANNOTATIONTYPE_DOUBLE, 5).
-define(ZIPKINCORE_ANNOTATIONTYPE_STRING, 6).

%% struct 'Jaeger.Thrift.Zipkin.Endpoint'

-record('Jaeger.Thrift.Zipkin.Endpoint', {'ipv4' :: integer() | 'undefined',
                                          'port' :: integer() | 'undefined',
                                          'service_name' :: string() | binary() | 'undefined',
                                          'ipv6' :: string() | binary() | 'undefined'}).
-type 'Jaeger.Thrift.Zipkin.Endpoint'() :: #'Jaeger.Thrift.Zipkin.Endpoint'{}.

%% struct 'Jaeger.Thrift.Zipkin.Annotation'

-record('Jaeger.Thrift.Zipkin.Annotation', {'timestamp' :: integer() | 'undefined',
                                            'value' :: string() | binary() | 'undefined',
                                            'host' :: 'Jaeger.Thrift.Zipkin.Endpoint'() | 'undefined'}).
-type 'Jaeger.Thrift.Zipkin.Annotation'() :: #'Jaeger.Thrift.Zipkin.Annotation'{}.

%% struct 'Jaeger.Thrift.Zipkin.BinaryAnnotation'

-record('Jaeger.Thrift.Zipkin.BinaryAnnotation', {'key' :: string() | binary() | 'undefined',
                                                  'value' :: string() | binary() | 'undefined',
                                                  'annotation_type' :: integer() | 'undefined',
                                                  'host' :: 'Jaeger.Thrift.Zipkin.Endpoint'() | 'undefined'}).
-type 'Jaeger.Thrift.Zipkin.BinaryAnnotation'() :: #'Jaeger.Thrift.Zipkin.BinaryAnnotation'{}.

%% struct 'Jaeger.Thrift.Zipkin.Span'

-record('Jaeger.Thrift.Zipkin.Span', {'trace_id' :: integer() | 'undefined',
                                      'name' :: string() | binary() | 'undefined',
                                      'id' :: integer() | 'undefined',
                                      'parent_id' :: integer() | 'undefined',
                                      'annotations' :: list() | 'undefined',
                                      'binary_annotations' :: list() | 'undefined',
                                      'debug' = false :: boolean() | 'undefined',
                                      'timestamp' :: integer() | 'undefined',
                                      'duration' :: integer() | 'undefined',
                                      'trace_id_high' :: integer() | 'undefined'}).
-type 'Jaeger.Thrift.Zipkin.Span'() :: #'Jaeger.Thrift.Zipkin.Span'{}.

%% struct 'Jaeger.Thrift.Zipkin.Response'

-record('Jaeger.Thrift.Zipkin.Response', {'ok' :: boolean()}).
-type 'Jaeger.Thrift.Zipkin.Response'() :: #'Jaeger.Thrift.Zipkin.Response'{}.

-endif.
