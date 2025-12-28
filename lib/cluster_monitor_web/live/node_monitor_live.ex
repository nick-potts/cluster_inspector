defmodule ClusterMonitorWeb.NodeMonitorLive do
  use ClusterMonitorWeb, :live_view

  @refresh_interval 250
  @cpu_history_size 60

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(@refresh_interval, self(), :refresh)
    end

    nodes = fetch_all_nodes(%{})
    cpu_history = init_cpu_history(nodes)

    {:ok, assign(socket, nodes: nodes, cpu_history: cpu_history, cpu_samples: %{})}
  end

  @impl true
  def handle_info(:refresh, socket) do
    {nodes, new_samples} = fetch_all_nodes_with_cpu(socket.assigns.cpu_samples)
    cpu_history = update_cpu_history(socket.assigns.cpu_history, nodes)
    {:noreply, assign(socket, nodes: nodes, cpu_history: cpu_history, cpu_samples: new_samples)}
  end

  defp init_cpu_history(nodes) do
    Map.new(nodes, fn node -> {node.name, []} end)
  end

  defp update_cpu_history(history, nodes) do
    Enum.reduce(nodes, history, fn node, acc ->
      cpu_val = if node.cpu && node.cpu.util, do: node.cpu.util, else: 0
      node_history = Map.get(acc, node.name, [])
      new_history = Enum.take([cpu_val | node_history], @cpu_history_size)
      Map.put(acc, node.name, new_history)
    end)
  end

  defp fetch_all_nodes(prev_samples) do
    {nodes, _} = fetch_all_nodes_with_cpu(prev_samples)
    nodes
  end

  defp fetch_all_nodes_with_cpu(prev_samples) do
    node_list = [node() | Node.list()]

    {nodes, new_samples} =
      Enum.map_reduce(node_list, prev_samples, fn n, samples ->
        {cpu, new_sample} = get_cpu_with_sample(n, Map.get(samples, n))

        node_data = %{
          name: n,
          memory: get_memory(n),
          cpu: cpu,
          disk: get_disk(n),
          containers: get_containers(n),
          system: get_system_info(n),
          network: get_network_info(n)
        }

        {node_data, Map.put(samples, n, new_sample)}
      end)

    {nodes, new_samples}
  end

  defp get_memory(n) do
    case :rpc.call(n, :memsup, :get_system_memory_data, [], 5_000) do
      {:badrpc, _} ->
        nil

      mem when is_list(mem) ->
        total = Keyword.get(mem, :total_memory, 0)
        free = Keyword.get(mem, :free_memory, 0)
        available = Keyword.get(mem, :available_memory, free)
        used = total - available

        %{
          total: total,
          used: used,
          percent: if(total > 0, do: Float.round(used / total * 100, 1), else: 0)
        }
    end
  end

  def sample_cpu do
    # First call initializes, second call gets actual reading
    :cpu_sup.util()
    Process.sleep(10)
    :cpu_sup.util()
  end

  defp get_cpu_with_sample(n, _prev_sample) do
    cores =
      case :rpc.call(n, :erlang, :system_info, [:logical_processors], 5_000) do
        {:badrpc, _} -> nil
        c when is_integer(c) -> c
        _ -> nil
      end

    # cpu_sup:util() - call twice with delay to get fresh reading
    util =
      case :rpc.call(n, __MODULE__, :sample_cpu, [], 5_000) do
        {:badrpc, _} -> nil
        u when is_float(u) -> Float.round(u, 1)
        u when is_integer(u) -> u * 1.0
        _ -> nil
      end

    cpu = if util || cores, do: %{util: util, cores: cores}, else: nil
    {cpu, nil}
  end

  defp get_disk(n) do
    case :rpc.call(n, :disksup, :get_disk_data, [], 5_000) do
      {:badrpc, _} ->
        nil

      disks when is_list(disks) ->
        disks
        |> Enum.reject(fn {mount, _size_kb, _percent} ->
          mount_str = to_string(mount)
          # Filter out virtual/system mounts
          String.starts_with?(mount_str, "/dev") or
            String.starts_with?(mount_str, "/proc") or
            String.starts_with?(mount_str, "/sys") or
            String.starts_with?(mount_str, "/run") or
            String.starts_with?(mount_str, "/etc/") or
            String.starts_with?(mount_str, "/snap")
        end)
        |> Enum.map(fn {mount, size_kb, percent_used} ->
          total = size_kb * 1024
          used = trunc(total * percent_used / 100)
          %{mount: to_string(mount), total: total, used: used, percent_used: percent_used}
        end)
    end
  end

  defp get_containers(n) do
    case :rpc.call(
           n,
           System,
           :cmd,
           ["docker", ["ps", "--format", "{{.Names}}\t{{.Status}}\t{{.Image}}"]],
           5_000
         ) do
      {:badrpc, _} ->
        nil

      {output, 0} ->
        output
        |> String.trim()
        |> String.split("\n", trim: true)
        |> Enum.map(fn line ->
          case String.split(line, "\t", parts: 3) do
            [name, status, image] -> %{name: name, status: status, image: image}
            _ -> nil
          end
        end)
        |> Enum.reject(&is_nil/1)

      _ ->
        nil
    end
  end

  defp get_system_info(n) do
    rpc = fn m, f, a -> :rpc.call(n, m, f, a, 5_000) end

    schedulers = safe_call(rpc, :erlang, :system_info, [:schedulers])
    schedulers_online = safe_call(rpc, :erlang, :system_info, [:schedulers_online])
    logical_cpus = safe_call(rpc, :erlang, :system_info, [:logical_processors])
    process_count = safe_call(rpc, :erlang, :system_info, [:process_count])
    process_limit = safe_call(rpc, :erlang, :system_info, [:process_limit])
    port_count = safe_call(rpc, :erlang, :system_info, [:port_count])
    port_limit = safe_call(rpc, :erlang, :system_info, [:port_limit])
    otp_release = safe_call(rpc, :erlang, :system_info, [:otp_release])
    erts_version = safe_call(rpc, :erlang, :system_info, [:version])

    # Uptime in milliseconds
    uptime = safe_call(rpc, :erlang, :statistics, [:wall_clock])
    uptime_ms = if is_tuple(uptime), do: elem(uptime, 0), else: nil

    # Load averages (returns nprocs * 256 scale)
    load1 = safe_call(rpc, :cpu_sup, :avg1, [])
    load5 = safe_call(rpc, :cpu_sup, :avg5, [])
    load15 = safe_call(rpc, :cpu_sup, :avg15, [])

    # Normalize load averages (divide by 256)
    normalize_load = fn
      l when is_integer(l) -> Float.round(l / 256, 2)
      _ -> nil
    end

    %{
      schedulers: schedulers,
      schedulers_online: schedulers_online,
      logical_cpus: logical_cpus,
      process_count: process_count,
      process_limit: process_limit,
      port_count: port_count,
      port_limit: port_limit,
      otp_release: if(is_list(otp_release), do: to_string(otp_release), else: nil),
      erts_version: if(is_list(erts_version), do: to_string(erts_version), else: nil),
      uptime_ms: uptime_ms,
      load1: normalize_load.(load1),
      load5: normalize_load.(load5),
      load15: normalize_load.(load15)
    }
  end

  defp get_network_info(n) do
    rpc = fn m, f, a -> :rpc.call(n, m, f, a, 5_000) end

    hostname =
      case rpc.(:inet, :gethostname, []) do
        {:ok, name} -> to_string(name)
        _ -> nil
      end

    ips =
      case rpc.(:inet, :getifaddrs, []) do
        {:ok, ifaddrs} ->
          ifaddrs
          |> Enum.flat_map(fn {_iface, opts} ->
            opts
            |> Enum.filter(fn
              {:addr, addr} when tuple_size(addr) in [4, 8] -> true
              _ -> false
            end)
            |> Enum.map(fn {:addr, addr} -> format_ip(addr) end)
          end)
          |> Enum.reject(&(&1 == "127.0.0.1" or &1 == "::1"))
          |> Enum.uniq()

        _ ->
          []
      end

    %{hostname: hostname, ips: ips}
  end

  defp safe_call(rpc, m, f, a) do
    case rpc.(m, f, a) do
      {:badrpc, _} -> nil
      result -> result
    end
  end

  defp format_ip(addr) when tuple_size(addr) == 4 do
    addr |> Tuple.to_list() |> Enum.join(".")
  end

  defp format_ip(addr) when tuple_size(addr) == 8 do
    addr
    |> Tuple.to_list()
    |> Enum.map(&Integer.to_string(&1, 16))
    |> Enum.join(":")
    |> String.downcase()
  end

  defp format_uptime(nil), do: "N/A"

  defp format_uptime(ms) when is_integer(ms) do
    seconds = div(ms, 1000)
    minutes = div(seconds, 60)
    hours = div(minutes, 60)
    days = div(hours, 24)

    cond do
      days > 0 -> "#{days}d #{rem(hours, 24)}h"
      hours > 0 -> "#{hours}h #{rem(minutes, 60)}m"
      minutes > 0 -> "#{minutes}m #{rem(seconds, 60)}s"
      true -> "#{seconds}s"
    end
  end

  defp format_number(nil), do: "N/A"

  defp format_number(n) when is_integer(n) and n >= 1_000_000,
    do: "#{Float.round(n / 1_000_000, 1)}M"

  defp format_number(n) when is_integer(n) and n >= 1_000, do: "#{Float.round(n / 1_000, 1)}K"
  defp format_number(n) when is_integer(n), do: Integer.to_string(n)

  defp format_bytes(bytes) when is_integer(bytes) do
    cond do
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 2)} GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 2)} MB"
      bytes >= 1024 -> "#{Float.round(bytes / 1024, 2)} KB"
      true -> "#{bytes} B"
    end
  end

  defp format_bytes(_), do: "N/A"

  @impl true
  def render(assigns) do
    ~H"""
    <div class="space-y-6">
      <h1 class="text-2xl font-bold">Cluster Node Monitor</h1>

      <div class="grid gap-6 md:grid-cols-2 lg:grid-cols-3">
        <.node_card :for={node <- @nodes} node={node} cpu_history={Map.get(@cpu_history, node.name, [])} />
      </div>
    </div>
    """
  end

  attr(:node, :map, required: true)
  attr(:cpu_history, :list, required: true)

  defp node_card(assigns) do
    ~H"""
    <div class="card bg-base-200 shadow-xl">
      <div class="card-body">
        <h2 class="card-title text-primary break-all">{@node.name}</h2>

        <%= if @node.system do %>
          <div class="text-xs opacity-60">
            OTP {@node.system.otp_release} / ERTS {@node.system.erts_version}
          </div>
        <% end %>

        <div class="divider my-1"></div>

        <%= if @node.system do %>
          <.stat_section title="System">
            <div class="grid grid-cols-2 gap-2 text-sm">
              <div>
                <span class="opacity-70">Uptime:</span>
                <span class="font-mono">{format_uptime(@node.system.uptime_ms)}</span>
              </div>
              <div>
                <span class="opacity-70">Load:</span>
                <span class="font-mono">
                  {if @node.system.load1, do: "#{@node.system.load1} / #{@node.system.load5} / #{@node.system.load15}", else: "N/A"}
                </span>
              </div>
              <div>
                <span class="opacity-70">Schedulers:</span>
                <span class="font-mono">{@node.system.schedulers_online} / {@node.system.schedulers}</span>
              </div>
              <div>
                <span class="opacity-70">CPUs:</span>
                <span class="font-mono">{@node.system.logical_cpus}</span>
              </div>
              <div>
                <span class="opacity-70">Processes:</span>
                <span class="font-mono">{format_number(@node.system.process_count)} / {format_number(@node.system.process_limit)}</span>
              </div>
              <div>
                <span class="opacity-70">Ports:</span>
                <span class="font-mono">{format_number(@node.system.port_count)} / {format_number(@node.system.port_limit)}</span>
              </div>
            </div>
          </.stat_section>
        <% end %>

        <%= if @node.network do %>
          <.stat_section title="Network">
            <div class="text-sm space-y-1">
              <%= if @node.network.hostname do %>
                <div>
                  <span class="opacity-70">Hostname:</span>
                  <span class="font-mono">{@node.network.hostname}</span>
                </div>
              <% end %>
              <%= if @node.network.ips && length(@node.network.ips) > 0 do %>
                <div>
                  <span class="opacity-70">IPs:</span>
                  <div class="font-mono text-xs pl-2">
                    <div :for={ip <- @node.network.ips}>{ip}</div>
                  </div>
                </div>
              <% end %>
            </div>
          </.stat_section>
        <% end %>

        <.stat_section title="Memory">
          <%= if @node.memory do %>
            <div class="flex justify-between text-sm mb-1">
              <span>{format_bytes(@node.memory.used)} / {format_bytes(@node.memory.total)}</span>
              <span>{@node.memory.percent}%</span>
            </div>
            <progress
              class={"progress w-full #{if @node.memory.percent > 90, do: "progress-error", else: "progress-primary"}"}
              value={@node.memory.percent}
              max="100"
            />
          <% else %>
            <div class="text-sm opacity-50">Unavailable (os_mon not running)</div>
          <% end %>
        </.stat_section>

        <.stat_section title="CPU">
          <%= if @node.cpu do %>
            <div class="flex justify-between text-sm mb-2">
              <span>{@node.cpu.cores} cores</span>
              <span class={"font-mono #{if @node.cpu.util && @node.cpu.util > 90, do: "text-error", else: ""}"}>{if @node.cpu.util, do: "#{Float.round(@node.cpu.util, 1)}%", else: "N/A"}</span>
            </div>
            <.cpu_graph history={@cpu_history} />
          <% else %>
            <div class="text-sm opacity-50">Unavailable</div>
          <% end %>
        </.stat_section>

        <.stat_section title="Disk">
          <%= if @node.disk && length(@node.disk) > 0 do %>
            <div class="space-y-3">
              <div :for={disk <- @node.disk} class="text-sm">
                <div class="font-mono mb-1">{disk.mount}</div>
                <div class="flex justify-between mb-1">
                  <span>{format_bytes(disk.used)} / {format_bytes(disk.total)}</span>
                  <span>{disk.percent_used}%</span>
                </div>
                <progress
                  class={"progress w-full #{if disk.percent_used > 90, do: "progress-error", else: "progress-primary"}"}
                  value={disk.percent_used}
                  max="100"
                />
              </div>
            </div>
          <% else %>
            <div class="text-sm opacity-50">Unavailable (os_mon not running)</div>
          <% end %>
        </.stat_section>

        <%= if @node.containers && length(@node.containers) > 0 do %>
          <.stat_section title="Containers">
            <div class="space-y-2">
              <div :for={container <- @node.containers} class="text-sm border-l-2 border-primary pl-2">
                <div class="font-semibold">{container.name}</div>
                <div class="opacity-70 text-xs">{container.image}</div>
                <div class="opacity-70 text-xs">{container.status}</div>
              </div>
            </div>
          </.stat_section>
        <% end %>
      </div>
    </div>
    """
  end

  slot(:inner_block, required: true)
  attr(:title, :string, required: true)

  defp stat_section(assigns) do
    ~H"""
    <div class="mb-3">
      <h3 class="text-sm font-semibold opacity-70 mb-1">{@title}</h3>
      {render_slot(@inner_block)}
    </div>
    """
  end

  attr(:history, :list, required: true)

  defp cpu_graph(assigns) do
    # Reverse so oldest is first (left side of graph)
    history = Enum.reverse(assigns.history)
    points = build_svg_points(history, 200, 40)

    assigns = assign(assigns, points: points, history: history)

    ~H"""
    <div class="w-full h-32 bg-base-300 rounded overflow-hidden">
      <svg viewBox="0 0 200 40" preserveAspectRatio="none" class="w-full h-full">
        <!-- Grid lines -->
        <line x1="0" y1="20" x2="200" y2="20" stroke="currentColor" stroke-opacity="0.1" />
        <!-- Fill area -->
        <%= if length(@history) > 1 do %>
          <polygon
            points={"0,40 #{@points} 200,40"}
            fill="currentColor"
            class="text-primary opacity-30"
          />
          <polyline
            points={@points}
            fill="none"
            stroke="currentColor"
            stroke-width="1.5"
            class="text-primary"
          />
        <% end %>
      </svg>
    </div>
    """
  end

  defp build_svg_points(history, width, height) do
    count = length(history)

    if count < 2 do
      ""
    else
      history
      |> Enum.with_index()
      |> Enum.map(fn {val, i} ->
        x = i / (count - 1) * width
        y = height - val / 100 * height
        "#{Float.round(x, 1)},#{Float.round(y, 1)}"
      end)
      |> Enum.join(" ")
    end
  end
end
