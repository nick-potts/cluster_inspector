/// DNS Poll clustering - a simple libcluster-like implementation for Gleam
/// Uses DNS to discover peer nodes and Erlang distribution to connect.
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process
import gleam/io

/// Start the DNS poll cluster supervisor
/// This spawns a process that periodically polls DNS and connects to discovered nodes
pub fn start(
  query: String,
  node_basename: String,
  poll_interval: Int,
) -> Result(Nil, Nil) {
  process.spawn(fn() { poll_loop(query, node_basename, poll_interval) })
  Ok(Nil)
}

fn poll_loop(query: String, node_basename: String, poll_interval: Int) -> Nil {
  // Poll and connect
  let connected = poll_and_connect(query, node_basename)

  // Log connected nodes
  case connected {
    [] -> Nil
    _ -> io.println("Connected nodes: " <> format_nodes(connected))
  }

  // Sleep and loop
  process.sleep(poll_interval)
  poll_loop(query, node_basename, poll_interval)
}

fn format_nodes(nodes: List(Atom)) -> String {
  do_format_nodes(nodes)
}

@external(erlang, "cluster_supervisor_ffi", "format_nodes")
fn do_format_nodes(nodes: List(Atom)) -> String

/// Poll DNS and connect to discovered nodes
@external(erlang, "cluster_supervisor_ffi", "poll_and_connect")
fn poll_and_connect(query: String, node_basename: String) -> List(Atom)

/// Get all connected Erlang nodes
@external(erlang, "erlang", "nodes")
pub fn nodes() -> List(Atom)
