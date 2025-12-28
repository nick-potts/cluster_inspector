defmodule ClusterMonitor.DNSPollIPv6 do
  @moduledoc """
  DNS polling strategy for Railway with proper IPv6 bracket notation.
  """
  use Cluster.Strategy
  alias Cluster.Strategy.State

  @default_polling_interval 5_000

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init([%State{config: config} = state]) do
    query = Keyword.fetch!(config, :query)
    basename = Keyword.fetch!(config, :node_basename)
    interval = Keyword.get(config, :polling_interval, @default_polling_interval)

    state = %State{state | meta: %{query: query, basename: basename, interval: interval}}

    {:ok, state, 0}
  end

  def handle_info(:timeout, state), do: handle_info(:poll, state)

  def handle_info(:poll, %State{meta: %{query: query, basename: basename, interval: interval}} = state) do
    nodes = discover_nodes(query, basename)

    new_state =
      case Cluster.Strategy.connect_nodes(state.topology, state.connect, state.list_nodes, nodes) do
        :ok -> state
        {:error, bad_nodes} ->
          Enum.each(bad_nodes, fn {node, reason} ->
            Cluster.Logger.warn(state.topology, "unable to connect to #{node}: #{inspect(reason)}")
          end)
          state
      end

    Process.send_after(self(), :poll, interval)
    {:noreply, new_state}
  end

  def handle_info(_, state), do: {:noreply, state}

  defp discover_nodes(query, basename) do
    # Get IPv6 addresses only (Railway uses IPv6 internally)
    dns_lookup(query, :aaaa)
    |> Enum.map(&format_node(&1, basename))
    |> Enum.uniq()
    |> Enum.reject(&(&1 == node()))
  end

  defp dns_lookup(query, type) do
    case :inet_res.getbyname(~c"#{query}", type) do
      {:ok, {:hostent, _, _, _, _, addrs}} ->
        Enum.map(addrs, &:inet.ntoa/1) |> Enum.map(&to_string/1)
      {:error, _} ->
        []
    end
  end

  defp format_node(ip, basename) do
    # With -proto_dist inet6_tcp, no brackets needed
    :"#{basename}@#{ip}"
  end
end
