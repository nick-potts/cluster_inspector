import cluster.{type ClusterState}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/string
import lustre/attribute.{attribute, class, id, src, type_}
import lustre/element.{type Element, text}
import lustre/element/html.{
  body, div, h1, head, html, meta, script, span, style, title,
}
import metrics.{type NodeMetrics}

/// Render the full page HTML with embedded state for hydration
pub fn render_page(state: ClusterState) -> Element(msg) {
  let state_json = cluster.state_to_json(state)

  html([attribute("lang", "en")], [
    head([], [
      meta([attribute("charset", "UTF-8")]),
      meta([
        attribute("name", "viewport"),
        attribute("content", "width=device-width, initial-scale=1.0"),
      ]),
      title([], "Cluster Monitor"),
      style([], css()),
    ]),
    body([], [
      div([id("app")], [render_dashboard(state)]),
      // Embed state for hydration
      script([id("initial-state"), type_("application/json")], state_json),
      // Client-side app
      script([type_("module"), src("/static/client.mjs")], ""),
    ]),
  ])
}

/// Render the dashboard content
pub fn render_dashboard(state: ClusterState) -> Element(msg) {
  let nodes = dict.to_list(state.nodes) |> list.map(fn(pair) { pair.1 })
  let sorted_nodes =
    list.sort(nodes, fn(a, b) { string.compare(a.replica_id, b.replica_id) })

  div([class("dashboard")], [
    render_header(list.length(sorted_nodes)),
    render_summary(sorted_nodes),
    div([class("nodes-grid")], list.map(sorted_nodes, render_node_card)),
    render_footer(state.self_id),
  ])
}

fn render_header(node_count: Int) -> Element(msg) {
  div([class("header")], [
    h1([], [text("Cluster Monitor")]),
    div([class("status")], [
      span([class("status-dot online")], []),
      text(int.to_string(node_count) <> " nodes online"),
    ]),
  ])
}

fn render_summary(nodes: List(NodeMetrics)) -> Element(msg) {
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
    div([class("summary-card")], [
      div([class("summary-value")], [text(int.to_string(node_count))]),
      div([class("summary-label")], [text("Nodes")]),
    ]),
    div([class("summary-card")], [
      div([class("summary-value")], [text(format_percent(avg_mem_pct))]),
      div([class("summary-label")], [text("Avg Memory")]),
    ]),
    div([class("summary-card")], [
      div([class("summary-value")], [text(format_percent(avg_cpu))]),
      div([class("summary-label")], [text("Avg CPU")]),
    ]),
    div([class("summary-card")], [
      div([class("summary-value")], [text(format_total_memory(total_mem))]),
      div([class("summary-label")], [text("Total Memory")]),
    ]),
  ])
}

fn render_node_card(node: NodeMetrics) -> Element(msg) {
  div([class("node-card")], [
    div([class("node-header")], [
      span([class("node-id")], [text(node.replica_id)]),
      span([class("node-region")], [text(node.region)]),
    ]),
    div([class("node-metrics")], [
      render_metric_bar(
        "Memory",
        node.memory_percent,
        node.memory_used_mb,
        node.memory_total_mb,
        "MB",
      ),
      render_metric_row("CPU (1m)", node.cpu_load_1),
      render_metric_row("CPU (5m)", node.cpu_load_5),
      render_metric_row("CPU (15m)", node.cpu_load_15),
      div([class("metric-row")], [
        span([class("metric-label")], [text("Processes")]),
        span([class("metric-value")], [text(int.to_string(node.process_count))]),
      ]),
      div([class("metric-row")], [
        span([class("metric-label")], [text("Uptime")]),
        span([class("metric-value")], [
          text(metrics.format_uptime(node.uptime_seconds)),
        ]),
      ]),
    ]),
  ])
}

fn render_metric_bar(
  label: String,
  percent: Float,
  used: Int,
  total: Int,
  unit: String,
) -> Element(msg) {
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
          attribute("style", "width: " <> float.to_string(width_pct) <> "%"),
        ],
        [],
      ),
    ]),
  ])
}

fn render_metric_row(label: String, value: Float) -> Element(msg) {
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

fn render_footer(self_id: String) -> Element(msg) {
  div([class("footer")], [text("Connected to node: " <> self_id)])
}

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

fn css() -> String {
  "
:root {
  --bg: #0a0a0f;
  --card-bg: #12121a;
  --border: #2a2a3a;
  --text: #e0e0e8;
  --text-dim: #888899;
  --accent: #6366f1;
  --success: #22c55e;
  --warning: #f59e0b;
  --critical: #ef4444;
}

* {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  background: var(--bg);
  color: var(--text);
  line-height: 1.6;
  min-height: 100vh;
}

.dashboard {
  max-width: 1400px;
  margin: 0 auto;
  padding: 2rem;
}

.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 2rem;
}

.header h1 {
  font-size: 1.75rem;
  font-weight: 600;
}

.status {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  color: var(--text-dim);
}

.status-dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: var(--text-dim);
}

.status-dot.online {
  background: var(--success);
  box-shadow: 0 0 8px var(--success);
}

.summary {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
  gap: 1rem;
  margin-bottom: 2rem;
}

.summary-card {
  background: var(--card-bg);
  border: 1px solid var(--border);
  border-radius: 12px;
  padding: 1.25rem;
  text-align: center;
}

.summary-value {
  font-size: 1.75rem;
  font-weight: 700;
  color: var(--accent);
}

.summary-label {
  font-size: 0.875rem;
  color: var(--text-dim);
  margin-top: 0.25rem;
}

.nodes-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
  gap: 1rem;
}

.node-card {
  background: var(--card-bg);
  border: 1px solid var(--border);
  border-radius: 12px;
  padding: 1.25rem;
}

.node-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
  padding-bottom: 0.75rem;
  border-bottom: 1px solid var(--border);
}

.node-id {
  font-weight: 600;
  font-family: monospace;
  font-size: 0.9rem;
}

.node-region {
  font-size: 0.75rem;
  color: var(--text-dim);
  background: var(--border);
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
}

.node-metrics {
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
}

.metric-bar-container {
  margin-bottom: 0.25rem;
}

.metric-bar-header {
  display: flex;
  justify-content: space-between;
  margin-bottom: 0.35rem;
  font-size: 0.8rem;
}

.metric-bar {
  height: 8px;
  background: var(--border);
  border-radius: 4px;
  overflow: hidden;
}

.bar-fill {
  height: 100%;
  background: var(--accent);
  border-radius: 4px;
  transition: width 0.3s ease;
}

.bar-fill.warning {
  background: var(--warning);
}

.bar-fill.critical {
  background: var(--critical);
}

.metric-row {
  display: flex;
  justify-content: space-between;
  font-size: 0.85rem;
}

.metric-label {
  color: var(--text-dim);
}

.metric-value {
  font-family: monospace;
}

.metric-value.warning {
  color: var(--warning);
}

.metric-value.critical {
  color: var(--critical);
}

.footer {
  margin-top: 2rem;
  text-align: center;
  color: var(--text-dim);
  font-size: 0.85rem;
}

@media (max-width: 640px) {
  .dashboard {
    padding: 1rem;
  }
  
  .header {
    flex-direction: column;
    gap: 1rem;
    text-align: center;
  }
  
  .nodes-grid {
    grid-template-columns: 1fr;
  }
}
"
}
