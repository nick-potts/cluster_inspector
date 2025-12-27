import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lustre
import lustre/attribute.{attribute, class, id, style}
import lustre/effect.{type Effect}
import lustre/element.{type Element, text}
import lustre/element/html.{div, h1, p, span}

// -- TYPES --

pub type NodeMetrics {
  NodeMetrics(
    replica_id: String,
    region: String,
    ip_address: String,
    memory_total_mb: Int,
    memory_used_mb: Int,
    memory_percent: Float,
    cpu_load_1: Float,
    cpu_load_5: Float,
    cpu_load_15: Float,
    process_count: Int,
    uptime_seconds: Int,
    timestamp: Int,
  )
}

pub type ClusterState {
  ClusterState(
    nodes: List(NodeMetrics),
    self_id: String,
    last_update: Int,
    node_count: Int,
  )
}

pub type Model {
  Model(state: Option(ClusterState), connected: Bool, error: Option(String))
}

pub type Msg {
  WebSocketConnected
  WebSocketDisconnected
  WebSocketMessage(String)
  WebSocketError(String)
  Reconnect
}

// -- MAIN --

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  let initial_state = load_initial_state()
  let model = Model(state: initial_state, connected: False, error: None)
  #(model, connect_websocket())
}

// -- UPDATE --

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    WebSocketConnected -> {
      #(Model(..model, connected: True, error: None), effect.none())
    }

    WebSocketDisconnected -> {
      #(
        Model(..model, connected: False),
        // Reconnect after 2 seconds
        effect.from(fn(dispatch) {
          set_timeout(2000, fn() { dispatch(Reconnect) })
        }),
      )
    }

    WebSocketMessage(data) -> {
      case decode_cluster_state(data) {
        Ok(state) -> #(Model(..model, state: Some(state)), effect.none())
        Error(_) -> #(model, effect.none())
      }
    }

    WebSocketError(err) -> {
      #(Model(..model, error: Some(err), connected: False), effect.none())
    }

    Reconnect -> {
      #(model, connect_websocket())
    }
  }
}

// -- VIEW --

fn view(model: Model) -> Element(Msg) {
  div([class("dashboard")], [
    case model.state {
      Some(state) -> view_dashboard(state, model.connected)
      None -> view_loading()
    },
  ])
}

fn view_dashboard(state: ClusterState, connected: Bool) -> Element(Msg) {
  let sorted_nodes =
    list.sort(state.nodes, fn(a, b) {
      case a.replica_id, b.replica_id {
        x, y if x < y -> order.Lt
        x, y if x > y -> order.Gt
        _, _ -> order.Eq
      }
    })

  div([], [
    view_header(list.length(sorted_nodes), connected),
    view_summary(sorted_nodes),
    div([class("nodes-grid")], list.map(sorted_nodes, view_node_card)),
    view_footer(state.self_id),
  ])
}

fn view_header(node_count: Int, connected: Bool) -> Element(Msg) {
  let status_class = case connected {
    True -> "status-dot online"
    False -> "status-dot"
  }
  let status_text = case connected {
    True -> int.to_string(node_count) <> " nodes online"
    False -> "Reconnecting..."
  }

  div([class("header")], [
    h1([], [text("Cluster Monitor")]),
    div([class("status")], [span([class(status_class)], []), text(status_text)]),
  ])
}

fn view_summary(nodes: List(NodeMetrics)) -> Element(Msg) {
  let node_count = list.length(nodes)

  let #(total_mem, used_mem, total_cpu) =
    list.fold(nodes, #(0, 0, 0.0), fn(acc, n) {
      let #(tm, um, tc) = acc
      #(tm + n.memory_total_mb, um + n.memory_used_mb, tc +. n.cpu_load_1)
    })

  let avg_mem_pct = case total_mem {
    0 -> 0.0
    _ -> int.to_float(used_mem) /. int.to_float(total_mem) *. 100.0
  }

  let avg_cpu = case node_count {
    0 -> 0.0
    n -> total_cpu /. int.to_float(n)
  }

  div([class("summary")], [
    view_summary_card(int.to_string(node_count), "Nodes"),
    view_summary_card(format_percent(avg_mem_pct), "Avg Memory"),
    view_summary_card(format_percent(avg_cpu), "Avg CPU"),
    view_summary_card(format_total_memory(total_mem), "Total Memory"),
  ])
}

fn view_summary_card(value: String, label: String) -> Element(Msg) {
  div([class("summary-card")], [
    div([class("summary-value")], [text(value)]),
    div([class("summary-label")], [text(label)]),
  ])
}

fn view_node_card(node: NodeMetrics) -> Element(Msg) {
  div([class("node-card")], [
    div([class("node-header")], [
      span([class("node-id")], [text(node.replica_id)]),
      span([class("node-region")], [text(node.region)]),
    ]),
    div([class("node-metrics")], [
      view_metric_bar(
        "Memory",
        node.memory_percent,
        node.memory_used_mb,
        node.memory_total_mb,
        "MB",
      ),
      view_metric_row("CPU (1m)", node.cpu_load_1),
      view_metric_row("CPU (5m)", node.cpu_load_5),
      view_metric_row("CPU (15m)", node.cpu_load_15),
      div([class("metric-row")], [
        span([class("metric-label")], [text("Processes")]),
        span([class("metric-value")], [text(int.to_string(node.process_count))]),
      ]),
      div([class("metric-row")], [
        span([class("metric-label")], [text("Uptime")]),
        span([class("metric-value")], [text(format_uptime(node.uptime_seconds))]),
      ]),
    ]),
  ])
}

fn view_metric_bar(
  label: String,
  percent: Float,
  used: Int,
  total: Int,
  unit: String,
) -> Element(Msg) {
  let width_pct = case percent >. 100.0 {
    True -> 100.0
    False -> percent
  }
  let bar_class = case percent {
    p if p >. 90.0 -> "bar-fill critical"
    p if p >. 70.0 -> "bar-fill warning"
    _ -> "bar-fill"
  }

  div([class("metric-bar-container")], [
    div([class("metric-bar-header")], [
      span([class("metric-label")], [text(label)]),
      span([class("metric-value")], [
        text(
          int.to_string(used)
          <> "/"
          <> int.to_string(total)
          <> " "
          <> unit
          <> " ("
          <> format_percent(percent)
          <> ")",
        ),
      ]),
    ]),
    div([class("metric-bar")], [
      div(
        [
          class(bar_class),
          style([#("width", float.to_string(width_pct) <> "%")]),
        ],
        [],
      ),
    ]),
  ])
}

fn view_metric_row(label: String, value: Float) -> Element(Msg) {
  let value_class = case value {
    v if v >. 90.0 -> "metric-value critical"
    v if v >. 70.0 -> "metric-value warning"
    _ -> "metric-value"
  }

  div([class("metric-row")], [
    span([class("metric-label")], [text(label)]),
    span([class(value_class)], [text(format_percent(value))]),
  ])
}

fn view_footer(self_id: String) -> Element(Msg) {
  div([class("footer")], [text("Connected to node: " <> self_id)])
}

fn view_loading() -> Element(Msg) {
  div([class("loading")], [p([], [text("Connecting to cluster...")])])
}

// -- HELPERS --

fn format_percent(pct: Float) -> String {
  let rounded = float.round(pct *. 10.0) |> int.to_float
  float.to_string(rounded /. 10.0) <> "%"
}

fn format_total_memory(mb: Int) -> String {
  case mb {
    m if m >= 1024 -> {
      let gb = int.to_float(m) /. 1024.0
      let rounded = float.round(gb *. 10.0) |> int.to_float
      float.to_string(rounded /. 10.0) <> " GB"
    }
    m -> int.to_string(m) <> " MB"
  }
}

fn format_uptime(seconds: Int) -> String {
  let days = seconds / 86_400
  let hours = { seconds % 86_400 } / 3600
  let minutes = { seconds % 3600 } / 60
  let secs = seconds % 60

  case days, hours, minutes {
    d, _, _ if d > 0 -> int.to_string(d) <> "d " <> int.to_string(hours) <> "h"
    _, h, _ if h > 0 ->
      int.to_string(h) <> "h " <> int.to_string(minutes) <> "m"
    _, _, m if m > 0 -> int.to_string(m) <> "m " <> int.to_string(secs) <> "s"
    _, _, _ -> int.to_string(secs) <> "s"
  }
}

// -- DECODERS --

fn decode_cluster_state(data: String) -> Result(ClusterState, Nil) {
  json.decode(data, cluster_state_decoder())
  |> result.map_error(fn(_) { Nil })
}

fn cluster_state_decoder() -> dynamic.Decoder(ClusterState) {
  dynamic.decode4(
    ClusterState,
    dynamic.field("nodes", dynamic.list(node_metrics_decoder())),
    dynamic.field("self_id", dynamic.string),
    dynamic.field("last_update", dynamic.int),
    dynamic.field("node_count", dynamic.int),
  )
}

fn node_metrics_decoder() -> dynamic.Decoder(NodeMetrics) {
  dynamic.decode12(
    NodeMetrics,
    dynamic.field("replica_id", dynamic.string),
    dynamic.field("region", dynamic.string),
    dynamic.field("ip_address", dynamic.string),
    dynamic.field("memory_total_mb", dynamic.int),
    dynamic.field("memory_used_mb", dynamic.int),
    dynamic.field("memory_percent", dynamic.float),
    dynamic.field("cpu_load_1", dynamic.float),
    dynamic.field("cpu_load_5", dynamic.float),
    dynamic.field("cpu_load_15", dynamic.float),
    dynamic.field("process_count", dynamic.int),
    dynamic.field("uptime_seconds", dynamic.int),
    dynamic.field("timestamp", dynamic.int),
  )
}

// -- FFI --

@external(javascript, "./client_ffi.mjs", "loadInitialState")
fn load_initial_state_ffi() -> Option(String)

fn load_initial_state() -> Option(ClusterState) {
  case load_initial_state_ffi() {
    Some(data) -> {
      case decode_cluster_state(data) {
        Ok(state) -> Some(state)
        Error(_) -> None
      }
    }
    None -> None
  }
}

@external(javascript, "./client_ffi.mjs", "connectWebSocket")
fn connect_websocket_ffi(
  on_open: fn() -> Nil,
  on_close: fn() -> Nil,
  on_message: fn(String) -> Nil,
  on_error: fn(String) -> Nil,
) -> Nil

fn connect_websocket() -> Effect(Msg) {
  effect.from(fn(dispatch) {
    connect_websocket_ffi(
      fn() { dispatch(WebSocketConnected) },
      fn() { dispatch(WebSocketDisconnected) },
      fn(data) { dispatch(WebSocketMessage(data)) },
      fn(err) { dispatch(WebSocketError(err)) },
    )
  })
}

@external(javascript, "./client_ffi.mjs", "setTimeout")
fn set_timeout(ms: Int, callback: fn() -> Nil) -> Nil
