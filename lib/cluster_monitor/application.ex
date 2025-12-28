defmodule ClusterMonitor.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    topologies = Application.get_env(:cluster_monitor, :cluster_topologies, [])

    children = [
      ClusterMonitorWeb.Telemetry,
      {Cluster.Supervisor, [topologies, [name: ClusterMonitor.ClusterSupervisor]]},
      {Phoenix.PubSub, name: ClusterMonitor.PubSub},
      # Start a worker by calling: ClusterMonitor.Worker.start_link(arg)
      # {ClusterMonitor.Worker, arg},
      # Start to serve requests, typically the last entry
      ClusterMonitorWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: ClusterMonitor.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    ClusterMonitorWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
