defmodule ClusterMonitorWeb.PageController do
  use ClusterMonitorWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
