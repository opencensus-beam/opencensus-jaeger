defmodule OpencensusJaeger.Mixfile do
  use Mix.Project

  def project do
    [app: :opencensus_jaeger,
     version: "0.0.1",
     deps: deps(),
     description: description(),
     package: package(),
     erlc_options: [:warnings_as_errors,
                    :warn_export_vars,
                    :warn_shadow_vars,
                    :warn_obsolete_guard],
     elixirc_options: [warnings_as_errors: true],
     test_coverage: [tool: ExCoveralls],
     preferred_cli_env: ["coveralls": :test, "coveralls.html": :test]]
  end

  defp description do
    """
    opencensus_jaeger
    """
  end

  defp package do
    [maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/opencensus-jaeger"},
     files: ["priv", "src", "README.md", "rebar.config"]]
  end

  defp deps do
    [{:credo, "~> 0.8.7", only: [:dev, :test]},
     {:excoveralls, "~> 0.7.3", only: [:test]}]
  end
end
