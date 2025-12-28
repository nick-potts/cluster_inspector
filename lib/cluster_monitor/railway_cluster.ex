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

    ips = lookup_ips(query)
    Logger.info("[RailwayCluster] Found #{length(ips)} IPs: #{inspect(ips)}")

    my_ip = get_my_ip()
    Logger.info("[RailwayCluster] My IP: #{my_ip}")

    peer_ips = Enum.reject(ips, &(&1 == my_ip))
    Logger.info("[RailwayCluster] Peer IPs (excluding self): #{inspect(peer_ips)}")

    # Try reverse DNS to get hostnames, then connect
    Enum.each(peer_ips, fn ip ->
      case reverse_lookup(ip) do
        {:ok, hostname} ->
          node_name = :"cluster_monitor@#{hostname}"
          Logger.info("[RailwayCluster] Resolved #{ip} -> #{hostname}")
          try_connect(node_name)

        :error ->
          # Fallback: try IP directly (may fail)
          Logger.warning("[RailwayCluster] No reverse DNS for #{ip}, trying direct IP")
          try_connect(:"cluster_monitor@#{ip}")
      end
    end)

    connected = Node.list()
    Logger.info("[RailwayCluster] Connected nodes: #{inspect(connected)}")

    state
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

  defp reverse_lookup(ip) do
    # Parse IP string to tuple
    case :inet.parse_address(~c"#{ip}") do
      {:ok, ip_tuple} ->
        case :inet.gethostbyaddr(ip_tuple) do
          {:ok, {:hostent, hostname, _, _, _, _}} ->
            {:ok, to_string(hostname)}
          {:error, reason} ->
            Logger.debug("[RailwayCluster] Reverse DNS failed for #{ip}: #{inspect(reason)}")
            :error
        end
      _ ->
        :error
    end
  end

  defp lookup_ips(query) do
    # Try both A and AAAA records
    a_records = do_lookup(query, :a)
    aaaa_records = do_lookup(query, :aaaa)

    (a_records ++ aaaa_records)
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
