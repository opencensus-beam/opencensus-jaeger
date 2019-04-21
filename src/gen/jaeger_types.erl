%%
%% Autogenerated by Thrift Compiler ()
%%
%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
%%

-module(jaeger_types).

-include("jaeger_types.hrl").

-export([struct_info/1, struct_info_ext/1, enum_info/1, enum_names/0, struct_names/0, exception_names/0]).

struct_info('Jaeger_Thrift.Tag') ->
  {struct, [{1, string},
          {2, i32},
          {3, string},
          {4, double},
          {5, bool},
          {6, i64},
          {7, string}]}
;

struct_info('Jaeger_Thrift.Log') ->
  {struct, [{1, i64},
          {2, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.Tag'}}}}]}
;

struct_info('Jaeger_Thrift.SpanRef') ->
  {struct, [{1, i32},
          {2, i64},
          {3, i64},
          {4, i64}]}
;

struct_info('Jaeger_Thrift.Span') ->
  {struct, [{1, i64},
          {2, i64},
          {3, i64},
          {4, i64},
          {5, string},
          {6, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.SpanRef'}}}},
          {7, i32},
          {8, i64},
          {9, i64},
          {10, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.Tag'}}}},
          {11, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.Log'}}}}]}
;

struct_info('Jaeger_Thrift.Process') ->
  {struct, [{1, string},
          {2, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.Tag'}}}}]}
;

struct_info('Jaeger_Thrift.Batch') ->
  {struct, [{1, {struct, {'jaeger_types', 'Jaeger_Thrift.Process'}}},
          {2, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.Span'}}}}]}
;

struct_info('Jaeger_Thrift.BatchSubmitResponse') ->
  {struct, [{1, bool}]}
;

struct_info(_) -> erlang:error(function_clause).

struct_info_ext('Jaeger_Thrift.Tag') ->
  {struct, [{1, required, string, 'key', undefined},
          {2, required, i32, 'vType', undefined},
          {3, optional, string, 'vStr', undefined},
          {4, optional, double, 'vDouble', undefined},
          {5, optional, bool, 'vBool', undefined},
          {6, optional, i64, 'vLong', undefined},
          {7, optional, string, 'vBinary', undefined}]}
;

struct_info_ext('Jaeger_Thrift.Log') ->
  {struct, [{1, required, i64, 'timestamp', undefined},
          {2, required, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.Tag'}}}, 'fields', []}]}
;

struct_info_ext('Jaeger_Thrift.SpanRef') ->
  {struct, [{1, required, i32, 'refType', undefined},
          {2, required, i64, 'traceIdLow', undefined},
          {3, required, i64, 'traceIdHigh', undefined},
          {4, required, i64, 'spanId', undefined}]}
;

struct_info_ext('Jaeger_Thrift.Span') ->
  {struct, [{1, required, i64, 'traceIdLow', undefined},
          {2, required, i64, 'traceIdHigh', undefined},
          {3, required, i64, 'spanId', undefined},
          {4, required, i64, 'parentSpanId', undefined},
          {5, required, string, 'operationName', undefined},
          {6, optional, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.SpanRef'}}}, 'references', []},
          {7, required, i32, 'flags', undefined},
          {8, required, i64, 'startTime', undefined},
          {9, required, i64, 'duration', undefined},
          {10, optional, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.Tag'}}}, 'tags', []},
          {11, optional, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.Log'}}}, 'logs', []}]}
;

struct_info_ext('Jaeger_Thrift.Process') ->
  {struct, [{1, required, string, 'serviceName', undefined},
          {2, optional, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.Tag'}}}, 'tags', []}]}
;

struct_info_ext('Jaeger_Thrift.Batch') ->
  {struct, [{1, required, {struct, {'jaeger_types', 'Jaeger_Thrift.Process'}}, 'process', #'Jaeger_Thrift.Process'{}},
          {2, required, {list, {struct, {'jaeger_types', 'Jaeger_Thrift.Span'}}}, 'spans', []}]}
;

struct_info_ext('Jaeger_Thrift.BatchSubmitResponse') ->
  {struct, [{1, required, bool, 'ok', undefined}]}
;

struct_info_ext(_) -> erlang:error(function_clause).

struct_names() ->
  ['Jaeger_Thrift.Tag', 'Jaeger_Thrift.Log', 'Jaeger_Thrift.SpanRef', 'Jaeger_Thrift.Span', 'Jaeger_Thrift.Process', 'Jaeger_Thrift.Batch', 'Jaeger_Thrift.BatchSubmitResponse'].

enum_info('TagType') ->
  [
    {'STRING', 0},
    {'DOUBLE', 1},
    {'BOOL', 2},
    {'LONG', 3},
    {'BINARY', 4}
  ];

enum_info('SpanRefType') ->
  [
    {'CHILD_OF', 0},
    {'FOLLOWS_FROM', 1}
  ];

enum_info(_) -> erlang:error(function_clause).

enum_names() ->
  ['TagType', 'SpanRefType'].

exception_names() ->
  [].

