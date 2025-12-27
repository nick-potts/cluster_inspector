import cluster.{type Message as ClusterMessage}
import config.{type Config}
import discovery
import gleam/erlang/process.{type Subject}
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import metrics.{type NodeMetrics}

/// Start the gossip loop
/// This runs in a separate process and periodically polls peers
pub fn start_gossip_loop(
  cfg: Config,
  cluster_actor: Subject(ClusterMessage),
) -> Nil {
  // Start the gossip process
  process.spawn(fn() { gossip_loop(cfg, cluster_actor) })
  Nil
}

fn gossip_loop(cfg: Config, cluster_actor: Subject(ClusterMessage)) -> Nil {
  // First, trigger our own tick
  cluster.tick(cluster_actor)

  // Discover peers
  let peers = discovery.discover_peers(cfg.private_domain)
  let self_ip = discovery.get_local_ip() |> result.unwrap("")
  let other_peers = discovery.filter_self(peers, self_ip)

  // Poll each peer (fire and forget via spawn)
  list.each(other_peers, fn(peer_ip) {
    process.spawn(fn() {
      case fetch_peer_metrics(peer_ip, cfg.port) {
        Ok(node_metrics) -> {
          cluster.update_node(cluster_actor, node_metrics)
        }
        Error(_) -> {
          // Peer unavailable, will be pruned if stale
          Nil
        }
      }
    })
    Nil
  })

  // Sleep until next tick
  process.sleep(cfg.gossip_interval_ms)

  // Prune stale nodes (3x gossip interval)
  process.send(
    cluster_actor,
    cluster.PruneStale(cfg.gossip_interval_ms * 3 / 1000),
  )

  // Loop
  gossip_loop(cfg, cluster_actor)
}

/// Fetch metrics from a peer node
fn fetch_peer_metrics(peer_ip: String, port: Int) -> Result(NodeMetrics, Nil) {
  // Build URL - handle IPv6 addresses
  let host = case string.contains(peer_ip, ":") {
    True -> "[" <> peer_ip <> "]"
    False -> peer_ip
  }
  let url = "http://" <> host <> ":" <> int.to_string(port) <> "/api/metrics"

  case request.to(url) {
    Ok(req) -> {
      let req = request.set_header(req, "accept", "application/json")

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 -> {
              case json.parse(resp.body, metrics.decoder()) {
                Ok(node_metrics) -> Ok(node_metrics)
                Error(_) -> Error(Nil)
              }
            }
            _ -> Error(Nil)
          }
        }
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}
