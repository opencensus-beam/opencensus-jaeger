@copyright 2019 Ilya Khaprov <<i.khaprov@gmail.com>>.
@title OpenCensus Jaeger Reporter
@version 0.0.1

@doc

[![Hex.pm][Hex badge]][Hex link]
[![Hex.pm Downloads][Hex downloads badge]][Hex link]
[![Build Status][Travis badge]][Travis link]
[![Coverage Status][Coveralls badge]][Coveralls link]

To use, add `opencensus_jaeger' dependency as a runtime application (in rebar3 this means add to the `applications' list of `.app.src') and set as the reporter in the `opencensus' configuration:

<pre lang="erlang">
{opencensus, [
    {reporters, [{oc_reporter_jaeger, [{hostname, "localhost"},
                                       {port, 6831}, %% default for compact protocol
                                       {service_name, "service"},
                                       {service_tags, #{"key" => "value"}}]}]},
    ...]}
</pre>

## License

MIT

<!-- Named Links -->

[Hex badge]: https://img.shields.io/hexpm/v/opencensus_jaeger.svg?maxAge=2592000?style=plastic
[Hex link]: https://hex.pm/packages/opencensus_jaeger
[Hex downloads badge]: https://img.shields.io/hexpm/dt/opencensus_jaeger.svg?maxAge=2592000
[Travis badge]: https://travis-ci.org/opencensus-beam/opencensus_jaeger.svg?branch=version-3
[Travis link]: https://travis-ci.org/opencensus-beam/opencensus_jaeger
[Coveralls badge]: https://coveralls.io/repos/github/opencensus-beam/opencensus_jaeger/badge.svg?branch=master
[Coveralls link]: https://coveralls.io/github/opencensus-beam/opencensus_jaeger?branch=master
