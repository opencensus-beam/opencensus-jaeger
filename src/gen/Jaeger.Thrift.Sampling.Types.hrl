-ifndef(_jaeger__thrift__sampling__types_included).
-define(_jaeger__thrift__sampling__types_included, yeah).

-define(SAMPLING_SAMPLINGSTRATEGYTYPE_PROBABILISTIC, 0).
-define(SAMPLING_SAMPLINGSTRATEGYTYPE_RATE_LIMITING, 1).

%% struct 'Jaeger.Thrift.ProbabilisticSamplingStrategy'

-record('Jaeger.Thrift.ProbabilisticSamplingStrategy', {'samplingRate' :: float()}).
-type 'Jaeger.Thrift.ProbabilisticSamplingStrategy'() :: #'Jaeger.Thrift.ProbabilisticSamplingStrategy'{}.

%% struct 'Jaeger.Thrift.RateLimitingSamplingStrategy'

-record('Jaeger.Thrift.RateLimitingSamplingStrategy', {'maxTracesPerSecond' :: integer()}).
-type 'Jaeger.Thrift.RateLimitingSamplingStrategy'() :: #'Jaeger.Thrift.RateLimitingSamplingStrategy'{}.

%% struct 'Jaeger.Thrift.OperationSamplingStrategy'

-record('Jaeger.Thrift.OperationSamplingStrategy', {'operation' :: string() | binary(),
                                                    'probabilisticSampling' = #'Jaeger.Thrift.ProbabilisticSamplingStrategy'{} :: 'Jaeger.Thrift.ProbabilisticSamplingStrategy'()}).
-type 'Jaeger.Thrift.OperationSamplingStrategy'() :: #'Jaeger.Thrift.OperationSamplingStrategy'{}.

%% struct 'Jaeger.Thrift.PerOperationSamplingStrategies'

-record('Jaeger.Thrift.PerOperationSamplingStrategies', {'defaultSamplingProbability' :: float(),
                                                         'defaultLowerBoundTracesPerSecond' :: float(),
                                                         'perOperationStrategies' = [] :: list(),
                                                         'defaultUpperBoundTracesPerSecond' :: float() | 'undefined'}).
-type 'Jaeger.Thrift.PerOperationSamplingStrategies'() :: #'Jaeger.Thrift.PerOperationSamplingStrategies'{}.

%% struct 'Jaeger.Thrift.SamplingStrategyResponse'

-record('Jaeger.Thrift.SamplingStrategyResponse', {'strategyType' :: integer(),
                                                   'probabilisticSampling' :: 'Jaeger.Thrift.ProbabilisticSamplingStrategy'() | 'undefined',
                                                   'rateLimitingSampling' :: 'Jaeger.Thrift.RateLimitingSamplingStrategy'() | 'undefined',
                                                   'operationSampling' :: 'Jaeger.Thrift.PerOperationSamplingStrategies'() | 'undefined'}).
-type 'Jaeger.Thrift.SamplingStrategyResponse'() :: #'Jaeger.Thrift.SamplingStrategyResponse'{}.

-endif.
