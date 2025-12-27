import config.{type Config}
import discovery
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/list
import gleam/otp/actor
import gleam/result
import metrics.{type NodeMetrics}

/// Get current system time in seconds
@external(erlang, "erlang", "system_time")
fn erlang_system_time(unit: metrics.SystemTimeUnit) -> Int

/// Messages the cluster actor handles
pub type Message {
  /// Periodic tick to refresh metrics
  Tick
  /// Update metrics for a specific node
  UpdateNode(NodeMetrics)
  /// Remove stale nodes
  PruneStale(max_age_seconds: Int)
  /// Get current cluster state
  GetState(reply_to: Subject(ClusterState))
  /// Subscribe to state updates (for WebSocket)
  Subscribe(Subject(ClusterState))
  /// Unsubscribe from updates
  Unsubscribe(Subject(ClusterState))
  /// Shutdown the actor
  Shutdown
}

/// The cluster state
pub type ClusterState {
  ClusterState(
    nodes: Dict(String, NodeMetrics),
    self_id: String,
    last_update: Int,
  )
}

/// Internal actor state
pub type State {
  State(
    config: Config,
    nodes: Dict(String, NodeMetrics),
    subscribers: List(Subject(ClusterState)),
    self_ip: String,
  )
}

/// Start the cluster actor
pub fn start(
  cfg: Config,
) -> Result(actor.Started(Subject(Message)), actor.StartError) {
  let self_ip = discovery.get_local_ip() |> result.unwrap("127.0.0.1")

  // Start os_mon
  metrics.ensure_os_mon_started()

  // Collect initial metrics
  let initial_metrics =
    metrics.collect(cfg.replica_id, cfg.replica_region, self_ip)
  let nodes = dict.from_list([#(cfg.replica_id, initial_metrics)])
  let initial_state =
    State(config: cfg, nodes: nodes, subscribers: [], self_ip: self_ip)

  actor.new(initial_state)
  |> actor.on_message(handle_message)
  |> actor.start
}

fn handle_message(state: State, msg: Message) -> actor.Next(State, Message) {
  case msg {
    Tick -> {
      // Collect our own metrics
      let my_metrics =
        metrics.collect(
          state.config.replica_id,
          state.config.replica_region,
          state.self_ip,
        )
      let nodes = dict.insert(state.nodes, state.config.replica_id, my_metrics)
      let new_state = State(..state, nodes: nodes)

      // Notify subscribers
      broadcast_state(new_state)

      actor.continue(new_state)
    }

    UpdateNode(node_metrics) -> {
      let nodes =
        dict.insert(state.nodes, node_metrics.replica_id, node_metrics)
      let new_state = State(..state, nodes: nodes)

      // Notify subscribers
      broadcast_state(new_state)

      actor.continue(new_state)
    }

    PruneStale(max_age) -> {
      let now = erlang_system_time(metrics.Second)
      let nodes =
        dict.filter(state.nodes, fn(_id, m) { now - m.timestamp < max_age })
      actor.continue(State(..state, nodes: nodes))
    }

    GetState(reply_to) -> {
      let cluster_state =
        ClusterState(
          nodes: state.nodes,
          self_id: state.config.replica_id,
          last_update: erlang_system_time(metrics.Second),
        )
      process.send(reply_to, cluster_state)
      actor.continue(state)
    }

    Subscribe(subscriber) -> {
      let subscribers = [subscriber, ..state.subscribers]
      // Send current state immediately
      let cluster_state =
        ClusterState(
          nodes: state.nodes,
          self_id: state.config.replica_id,
          last_update: erlang_system_time(metrics.Second),
        )
      process.send(subscriber, cluster_state)
      actor.continue(State(..state, subscribers: subscribers))
    }

    Unsubscribe(subscriber) -> {
      let subscribers =
        list.filter(state.subscribers, fn(s) { s != subscriber })
      actor.continue(State(..state, subscribers: subscribers))
    }

    Shutdown -> {
      actor.stop()
    }
  }
}

fn broadcast_state(state: State) -> Nil {
  let cluster_state =
    ClusterState(
      nodes: state.nodes,
      self_id: state.config.replica_id,
      last_update: erlang_system_time(metrics.Second),
    )
  list.each(state.subscribers, fn(sub) { process.send(sub, cluster_state) })
}

/// Get current state synchronously
pub fn get_state(actor_subject: Subject(Message)) -> ClusterState {
  actor.call(actor_subject, 5000, GetState)
}

/// Subscribe to state updates
pub fn subscribe(
  actor_subject: Subject(Message),
  subscriber: Subject(ClusterState),
) -> Nil {
  process.send(actor_subject, Subscribe(subscriber))
}

/// Unsubscribe from updates
pub fn unsubscribe(
  actor_subject: Subject(Message),
  subscriber: Subject(ClusterState),
) -> Nil {
  process.send(actor_subject, Unsubscribe(subscriber))
}

/// Trigger a tick manually
pub fn tick(actor_subject: Subject(Message)) -> Nil {
  process.send(actor_subject, Tick)
}

/// Update node metrics
pub fn update_node(
  actor_subject: Subject(Message),
  node_metrics: NodeMetrics,
) -> Nil {
  process.send(actor_subject, UpdateNode(node_metrics))
}

/// Convert cluster state to JSON
pub fn state_to_json(state: ClusterState) -> String {
  let nodes_list =
    dict.to_list(state.nodes)
    |> list.map(fn(pair) { metrics.to_json(pair.1) })

  json.object([
    #("nodes", json.array(nodes_list, fn(j) { j })),
    #("self_id", json.string(state.self_id)),
    #("last_update", json.int(state.last_update)),
    #("node_count", json.int(dict.size(state.nodes))),
  ])
  |> json.to_string
}
