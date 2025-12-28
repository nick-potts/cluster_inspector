defmodule ClusterMonitorWeb.PageController do
  use ClusterMonitorWeb, :controller

  def home(conn, _params) do
    nodes = Node.list()
    node_count = length(nodes) + 1

    render(conn, :home, node: Node.self(), nodes: nodes, node_count: node_count)
  end
end
