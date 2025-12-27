import cluster.{type ClusterState, type Message as ClusterMessage}
import config.{type Config}
import gleam/bytes_tree
import gleam/erlang/process.{type Subject}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import lustre/element
import metrics
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage,
}
import web/views

/// Context passed to request handlers
pub type Context {
  Context(config: Config, cluster: Subject(ClusterMessage))
}

/// Handle incoming HTTP requests
pub fn handle_request(
  req: Request(Connection),
  ctx: Context,
) -> Response(ResponseData) {
  case request.path_segments(req) {
    // WebSocket upgrade for live updates
    ["ws"] -> handle_websocket(req, ctx)

    // API endpoint - return this node's metrics as JSON
    ["api", "metrics"] -> handle_metrics_api(ctx)

    // API endpoint - return cluster state
    ["api", "cluster"] -> handle_cluster_api(ctx)

    // Static files
    ["static", ..path] -> serve_static(path)

    // Dashboard - serve HTML with embedded state
    [] -> handle_dashboard(ctx)
    _ -> handle_dashboard(ctx)
  }
}

fn handle_dashboard(ctx: Context) -> Response(ResponseData) {
  let state = cluster.get_state(ctx.cluster)
  let html = views.render_page(state)
  let body = element.to_document_string(html)

  response.new(200)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

fn handle_metrics_api(ctx: Context) -> Response(ResponseData) {
  let self_ip = discovery_get_local_ip()
  let m =
    metrics.collect(ctx.config.replica_id, ctx.config.replica_region, self_ip)
  let body = json.to_string(metrics.to_json(m))

  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

fn handle_cluster_api(ctx: Context) -> Response(ResponseData) {
  let state = cluster.get_state(ctx.cluster)
  let body = cluster.state_to_json(state)

  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}

fn serve_static(path: List(String)) -> Response(ResponseData) {
  let file_path = "priv/static/" <> string.join(path, "/")

  case mist.send_file(file_path, offset: 0, limit: None) {
    Ok(body) -> {
      let content_type = get_content_type(file_path)
      response.new(200)
      |> response.set_header("content-type", content_type)
      |> response.set_body(body)
    }
    Error(_) -> {
      response.new(404)
      |> response.set_body(mist.Bytes(bytes_tree.from_string("Not found")))
    }
  }
}

fn get_content_type(path: String) -> String {
  case
    string.ends_with(path, ".js"),
    string.ends_with(path, ".mjs"),
    string.ends_with(path, ".css"),
    string.ends_with(path, ".html"),
    string.ends_with(path, ".json")
  {
    True, _, _, _, _ -> "application/javascript"
    _, True, _, _, _ -> "application/javascript"
    _, _, True, _, _ -> "text/css"
    _, _, _, True, _ -> "text/html"
    _, _, _, _, True -> "application/json"
    _, _, _, _, _ -> "application/octet-stream"
  }
}

@external(erlang, "discovery_ffi", "get_local_ip")
fn discovery_get_local_ip_ffi() -> Result(String, Nil)

fn discovery_get_local_ip() -> String {
  case discovery_get_local_ip_ffi() {
    Ok(ip) -> ip
    Error(_) -> "127.0.0.1"
  }
}

/// WebSocket state
type WsState {
  WsState(
    cluster: Subject(ClusterMessage),
    state_subject: Subject(ClusterState),
  )
}

fn handle_websocket(
  req: Request(Connection),
  ctx: Context,
) -> Response(ResponseData) {
  mist.websocket(
    request: req,
    on_init: fn(_conn) {
      // Create a subject to receive cluster state updates
      let state_subject = process.new_subject()

      // Subscribe to cluster updates
      cluster.subscribe(ctx.cluster, state_subject)

      // Set up selector to receive cluster state updates
      let selector =
        process.new_selector()
        |> process.select(state_subject)

      #(
        WsState(cluster: ctx.cluster, state_subject: state_subject),
        Some(selector),
      )
    },
    on_close: fn(state) {
      cluster.unsubscribe(state.cluster, state.state_subject)
    },
    handler: handle_ws_message,
  )
}

fn handle_ws_message(
  state: WsState,
  message: WebsocketMessage(ClusterState),
  conn: WebsocketConnection,
) -> mist.Next(WsState, ClusterState) {
  case message {
    mist.Text(_) -> {
      // Client messages - could handle ping/pong or commands
      mist.continue(state)
    }
    mist.Binary(_) -> {
      mist.continue(state)
    }
    mist.Custom(cluster_state) -> {
      // Received cluster state update - send to client
      let json_str = cluster.state_to_json(cluster_state)
      case mist.send_text_frame(conn, json_str) {
        Ok(_) -> mist.continue(state)
        Error(_) -> mist.stop()
      }
    }
    mist.Closed | mist.Shutdown -> {
      mist.stop()
    }
  }
}
