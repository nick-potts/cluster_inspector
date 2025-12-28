defmodule ClusterMonitorWeb.NodeMonitorLive do
  use ClusterMonitorWeb, :live_view

  @refresh_interval 2_000

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(@refresh_interval, self(), :refresh)
    end

    {:ok, assign(socket, nodes: fetch_all_nodes())}
  end

  @impl true
  def handle_info(:refresh, socket) do
    {:noreply, assign(socket, nodes: fetch_all_nodes())}
  end

  defp fetch_all_nodes do
    nodes = [node() | Node.list()]

    Enum.map(nodes, fn n ->
      %{
        name: n,
        memory: get_memory(n),
        cpu: get_cpu(n),
        disk: get_disk(n),
        containers: get_containers(n)
      }
    end)
  end

  defp get_memory(n) do
    case :rpc.call(n, :erlang, :memory, [], 5_000) do
      {:badrpc, _} ->
        nil

      mem when is_list(mem) ->
        total = Keyword.get(mem, :total, 0)
        processes = Keyword.get(mem, :processes, 0)
        %{total: total, processes: processes}
    end
  end

  defp get_cpu(n) do
    case :rpc.call(n, :cpu_sup, :util, [], 5_000) do
      {:badrpc, _} -> nil
      util when is_number(util) -> util
      _ -> nil
    end
  end

  defp get_disk(n) do
    case :rpc.call(n, :disksup, :get_disk_data, [], 5_000) do
      {:badrpc, _} ->
        nil

      disks when is_list(disks) ->
        Enum.map(disks, fn {mount, size_kb, percent_used} ->
          %{mount: to_string(mount), size_kb: size_kb, percent_used: percent_used}
        end)
    end
  end

  defp get_containers(n) do
    case :rpc.call(n, System, :cmd, ["docker", ["ps", "--format", "{{.Names}}\t{{.Status}}\t{{.Image}}"]], 5_000) do
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
        <.node_card :for={node <- @nodes} node={node} />
      </div>
    </div>
    """
  end

  attr :node, :map, required: true

  defp node_card(assigns) do
    ~H"""
    <div class="card bg-base-200 shadow-xl">
      <div class="card-body">
        <h2 class="card-title text-primary break-all">{@node.name}</h2>

        <div class="divider my-1"></div>

        <.stat_section title="Memory">
          <%= if @node.memory do %>
            <div class="stat-value text-lg">{format_bytes(@node.memory.total)}</div>
            <div class="text-sm opacity-70">
              Processes: {format_bytes(@node.memory.processes)}
            </div>
          <% else %>
            <div class="text-sm opacity-50">Unavailable</div>
          <% end %>
        </.stat_section>

        <.stat_section title="CPU">
          <%= if @node.cpu do %>
            <div class="stat-value text-lg">{Float.round(@node.cpu, 1)}%</div>
          <% else %>
            <div class="text-sm opacity-50">Unavailable (os_mon not running)</div>
          <% end %>
        </.stat_section>

        <.stat_section title="Disk">
          <%= if @node.disk && length(@node.disk) > 0 do %>
            <div class="space-y-2">
              <div :for={disk <- @node.disk} class="text-sm">
                <div class="flex justify-between">
                  <span class="font-mono">{disk.mount}</span>
                  <span>{disk.percent_used}% used</span>
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

        <.stat_section title="Containers">
          <%= if @node.containers && length(@node.containers) > 0 do %>
            <div class="space-y-2">
              <div :for={container <- @node.containers} class="text-sm border-l-2 border-primary pl-2">
                <div class="font-semibold">{container.name}</div>
                <div class="opacity-70 text-xs">{container.image}</div>
                <div class="opacity-70 text-xs">{container.status}</div>
              </div>
            </div>
          <% else %>
            <div class="text-sm opacity-50">
              <%= if @node.containers == nil, do: "Docker unavailable", else: "No containers" %>
            </div>
          <% end %>
        </.stat_section>
      </div>
    </div>
    """
  end

  slot :inner_block, required: true
  attr :title, :string, required: true

  defp stat_section(assigns) do
    ~H"""
    <div class="mb-3">
      <h3 class="text-sm font-semibold opacity-70 mb-1">{@title}</h3>
      {render_slot(@inner_block)}
    </div>
    """
  end
end
