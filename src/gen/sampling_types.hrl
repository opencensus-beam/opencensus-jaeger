-ifndef(_sampling_types_included).
-define(_sampling_types_included, yeah).

-define(SAMPLING_SAMPLINGSTRATEGYTYPE_PROBABILISTIC, 0).
-define(SAMPLING_SAMPLINGSTRATEGYTYPE_RATE_LIMITING, 1).

%% struct 'Jaeger_Thrift_Agent.ProbabilisticSamplingStrategy'

-record('Jaeger_Thrift_Agent.ProbabilisticSamplingStrategy', {'samplingRate' :: float()}).
-type 'Jaeger_Thrift_Agent.ProbabilisticSamplingStrategy'() :: #'Jaeger_Thrift_Agent.ProbabilisticSamplingStrategy'{}.

%% struct 'Jaeger_Thrift_Agent.RateLimitingSamplingStrategy'

-record('Jaeger_Thrift_Agent.RateLimitingSamplingStrategy', {'maxTracesPerSecond' :: integer()}).
-type 'Jaeger_Thrift_Agent.RateLimitingSamplingStrategy'() :: #'Jaeger_Thrift_Agent.RateLimitingSamplingStrategy'{}.

%% struct 'Jaeger_Thrift_Agent.OperationSamplingStrategy'

-record('Jaeger_Thrift_Agent.OperationSamplingStrategy', {'operation' :: string() | binary(),
                                                          'probabilisticSampling' = #'Jaeger_Thrift_Agent.ProbabilisticSamplingStrategy'{} :: 'Jaeger_Thrift_Agent.ProbabilisticSamplingStrategy'()}).
-type 'Jaeger_Thrift_Agent.OperationSamplingStrategy'() :: #'Jaeger_Thrift_Agent.OperationSamplingStrategy'{}.

%% struct 'Jaeger_Thrift_Agent.PerOperationSamplingStrategies'

-record('Jaeger_Thrift_Agent.PerOperationSamplingStrategies', {'defaultSamplingProbability' :: float(),
                                                               'defaultLowerBoundTracesPerSecond' :: float(),
                                                               'perOperationStrategies' = [] :: list(),
                                                               'defaultUpperBoundTracesPerSecond' :: float() | 'undefined'}).
-type 'Jaeger_Thrift_Agent.PerOperationSamplingStrategies'() :: #'Jaeger_Thrift_Agent.PerOperationSamplingStrategies'{}.

%% struct 'Jaeger_Thrift_Agent.SamplingStrategyResponse'

-record('Jaeger_Thrift_Agent.SamplingStrategyResponse', {'strategyType' :: integer(),
                                                         'probabilisticSampling' :: 'Jaeger_Thrift_Agent.ProbabilisticSamplingStrategy'() | 'undefined',
                                                         'rateLimitingSampling' :: 'Jaeger_Thrift_Agent.RateLimitingSamplingStrategy'() | 'undefined',
                                                         'operationSampling' :: 'Jaeger_Thrift_Agent.PerOperationSamplingStrategies'() | 'undefined'}).
-type 'Jaeger_Thrift_Agent.SamplingStrategyResponse'() :: #'Jaeger_Thrift_Agent.SamplingStrategyResponse'{}.

-endif.
