defmodule ClusterMonitor.RailwayCluster do
  @moduledoc """
  DNS-based cluster discovery for Railway deployments.

  Polls RAILWAY_PRIVATE_DOMAIN for peer IPs and attempts connections
  using hostnames derived from reverse DNS lookups.
  """
  use GenServer
  require Logger

  @poll_interval 5_000

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    query = Keyword.get(opts, :query)

    if query && query != :ignore do
      Logger.info("[RailwayCluster] Starting with query: #{query}")
      Logger.info("[RailwayCluster] Current node: #{node()}")
      schedule_poll(0)
      {:ok, %{query: query, known_nodes: MapSet.new()}}
    else
      Logger.info("[RailwayCluster] No query configured, ignoring")
      :ignore
    end
  end

  @impl true
  def handle_info(:poll, state) do
    new_state = discover_and_connect(state)
    schedule_poll(@poll_interval)
    {:noreply, new_state}
  end

  defp schedule_poll(delay) do
    Process.send_after(self(), :poll, delay)
  end

  defp discover_and_connect(%{query: query} = state) do
    Logger.info("[RailwayCluster] Polling DNS: #{query}")
    Logger.info("[RailwayCluster] Current node: #{node()}")

    # Get both IPv4 and IPv6, prefer IPv6 for Erlang compatibility
    ipv6_addrs = lookup_ips(query, :aaaa)
    ipv4_addrs = lookup_ips(query, :a)

    Logger.info("[RailwayCluster] Found #{length(ipv6_addrs)} IPv6, #{length(ipv4_addrs)} IPv4 addresses")

    if length(ipv6_addrs) > 0 do
      Logger.info("[RailwayCluster] IPv6 addresses: #{inspect(Enum.take(ipv6_addrs, 5))}...")
    end

    my_ip = get_my_ip()
    Logger.info("[RailwayCluster] My IP: #{my_ip}")

    # Use IPv6 addresses if available (they work with Erlang longnames via brackets)
    peer_ips =
      if length(ipv6_addrs) > 0 do
        Enum.reject(ipv6_addrs, &(&1 == my_ip))
      else
        Enum.reject(ipv4_addrs, &(&1 == my_ip))
      end

    Logger.info("[RailwayCluster] Attempting to connect to #{length(peer_ips)} peers")

    # Connect to peers
    Enum.each(peer_ips, fn ip ->
      node_name = format_node_name(ip)
      try_connect(node_name)
    end)

    connected = Node.list()
    Logger.info("[RailwayCluster] Connected nodes: #{inspect(connected)}")

    state
  end

  # Format node name with proper IPv6 bracket notation
  defp format_node_name(ip) do
    if String.contains?(ip, ":") do
      # IPv6 - wrap in brackets
      :"cluster_monitor@[#{ip}]"
    else
      # IPv4 - use as-is (will likely fail, but log it)
      :"cluster_monitor@#{ip}"
    end
  end

  defp try_connect(node_name) do
    case Node.connect(node_name) do
      true ->
        Logger.info("[RailwayCluster] Connected to #{node_name}")
      false ->
        Logger.warning("[RailwayCluster] Failed to connect to #{node_name}")
      :ignored ->
        Logger.warning("[RailwayCluster] Connection ignored for #{node_name} (node not alive?)")
    end
  end

  defp lookup_ips(query, type) when type in [:a, :aaaa] do
    do_lookup(query, type)
    |> Enum.map(&format_ip/1)
    |> Enum.uniq()
  end

  defp do_lookup(query, type) do
    case :inet_res.getbyname(~c"#{query}", type) do
      {:ok, {:hostent, _, _, _, _, addrs}} -> addrs
      {:error, reason} ->
        Logger.debug("[RailwayCluster] DNS lookup failed for #{query} (#{type}): #{inspect(reason)}")
        []
    end
  end

  defp format_ip(ip) when is_tuple(ip) do
    :inet.ntoa(ip) |> to_string()
  end

  defp get_my_ip do
    case System.cmd("hostname", ["-i"]) do
      {ip, 0} -> String.trim(ip) |> String.split() |> List.first()
      _ -> nil
    end
  end
end
