-ifndef(_zipkincore_types_included).
-define(_zipkincore_types_included, yeah).

-define(ZIPKINCORE_ANNOTATIONTYPE_BOOL, 0).
-define(ZIPKINCORE_ANNOTATIONTYPE_BYTES, 1).
-define(ZIPKINCORE_ANNOTATIONTYPE_I16, 2).
-define(ZIPKINCORE_ANNOTATIONTYPE_I32, 3).
-define(ZIPKINCORE_ANNOTATIONTYPE_I64, 4).
-define(ZIPKINCORE_ANNOTATIONTYPE_DOUBLE, 5).
-define(ZIPKINCORE_ANNOTATIONTYPE_STRING, 6).

%% struct 'Jaeger_Thrift_Zipkin.Endpoint'

-record('Jaeger_Thrift_Zipkin.Endpoint', {'ipv4' :: integer() | 'undefined',
                                          'port' :: integer() | 'undefined',
                                          'service_name' :: string() | binary() | 'undefined',
                                          'ipv6' :: string() | binary() | 'undefined'}).
-type 'Jaeger_Thrift_Zipkin.Endpoint'() :: #'Jaeger_Thrift_Zipkin.Endpoint'{}.

%% struct 'Jaeger_Thrift_Zipkin.Annotation'

-record('Jaeger_Thrift_Zipkin.Annotation', {'timestamp' :: integer() | 'undefined',
                                            'value' :: string() | binary() | 'undefined',
                                            'host' :: 'Jaeger_Thrift_Zipkin.Endpoint'() | 'undefined'}).
-type 'Jaeger_Thrift_Zipkin.Annotation'() :: #'Jaeger_Thrift_Zipkin.Annotation'{}.

%% struct 'Jaeger_Thrift_Zipkin.BinaryAnnotation'

-record('Jaeger_Thrift_Zipkin.BinaryAnnotation', {'key' :: string() | binary() | 'undefined',
                                                  'value' :: string() | binary() | 'undefined',
                                                  'annotation_type' :: integer() | 'undefined',
                                                  'host' :: 'Jaeger_Thrift_Zipkin.Endpoint'() | 'undefined'}).
-type 'Jaeger_Thrift_Zipkin.BinaryAnnotation'() :: #'Jaeger_Thrift_Zipkin.BinaryAnnotation'{}.

%% struct 'Jaeger_Thrift_Zipkin.Span'

-record('Jaeger_Thrift_Zipkin.Span', {'trace_id' :: integer() | 'undefined',
                                      'name' :: string() | binary() | 'undefined',
                                      'id' :: integer() | 'undefined',
                                      'parent_id' :: integer() | 'undefined',
                                      'annotations' :: list() | 'undefined',
                                      'binary_annotations' :: list() | 'undefined',
                                      'debug' = false :: boolean() | 'undefined',
                                      'timestamp' :: integer() | 'undefined',
                                      'duration' :: integer() | 'undefined',
                                      'trace_id_high' :: integer() | 'undefined'}).
-type 'Jaeger_Thrift_Zipkin.Span'() :: #'Jaeger_Thrift_Zipkin.Span'{}.

%% struct 'Jaeger_Thrift_Zipkin.Response'

-record('Jaeger_Thrift_Zipkin.Response', {'ok' :: boolean()}).
-type 'Jaeger_Thrift_Zipkin.Response'() :: #'Jaeger_Thrift_Zipkin.Response'{}.

-endif.
