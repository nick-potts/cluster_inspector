import cluster
import cluster_supervisor
import config
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/result
import mist.{type Connection, type ResponseData}
import web/router.{Context}

pub fn main() {
  // Load configuration
  let cfg = config.load()

  io.println("Starting Cluster Monitor...")
  io.println("  Replica ID: " <> cfg.replica_id)
  io.println("  Region: " <> cfg.replica_region)
  io.println("  Port: " <> int.to_string(cfg.port))
  io.println("  Private Domain: " <> cfg.private_domain)

  // Start Erlang distribution if on Railway (not localhost)
  case cfg.private_domain {
    "localhost" -> {
      io.println("Local mode - skipping distribution")
    }
    _ -> {
      // Get local IP and build node name: service_name@local_ip
      io.println("Attempting to start distribution...")
      case get_local_ip() {
        Ok(local_ip) -> {
          io.println("  Local IP: " <> local_ip)
          let node_name = cfg.service_name <> "@" <> local_ip
          io.println("  Node name: " <> node_name)
          case start_distribution(node_name, cfg.erlang_cookie) {
            Ok(_) -> {
              io.println("Distribution started as: " <> node_name)

              // Start DNS poll clustering
              let _ =
                cluster_supervisor.start(
                  cfg.private_domain,
                  cfg.service_name,
                  cfg.gossip_interval_ms,
                )
              io.println("DNS poll clustering started")
            }
            Error(reason) -> {
              io.println("Warning: Failed to start distribution: " <> reason)
            }
          }
        }
        Error(_) -> {
          io.println(
            "Warning: Could not determine local IP, skipping distribution",
          )
        }
      }
    }
  }

  // Start the cluster actor (for metrics tracking)
  let assert Ok(started) = cluster.start(cfg)
  let cluster_subject = started.data
  io.println("Cluster actor started")

  // Create request context
  let ctx = Context(config: cfg, cluster: cluster_subject)

  // Start the web server
  let assert Ok(_) =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      router.handle_request(req, ctx)
    }
    |> mist.new
    |> mist.bind(cfg.host)
    |> mist.port(cfg.port)
    |> mist.start

  io.println("Server started on " <> cfg.host <> ":" <> int.to_string(cfg.port))

  // Keep the main process alive
  process.sleep_forever()
}

/// Start Erlang distribution with the given node name and cookie
fn start_distribution(
  node_name: String,
  cookie: String,
) -> Result(String, String) {
  do_start_distribution(node_name, cookie)
  |> result.map(fn(_) { node_name })
  |> result.map_error(fn(_) { "Failed to start distribution" })
}

@external(erlang, "cluster_supervisor_ffi", "start_distribution")
fn do_start_distribution(node_name: String, cookie: String) -> Result(Nil, Nil)

/// Get the local IP address for this node
fn get_local_ip() -> Result(String, Nil) {
  do_get_local_ip()
}

@external(erlang, "cluster_supervisor_ffi", "get_local_ip")
fn do_get_local_ip() -> Result(String, Nil)
