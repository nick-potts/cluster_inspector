import cluster
import config
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gossip
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

  // Start the cluster actor
  let assert Ok(started) = cluster.start(cfg)
  let cluster_subject = started.data
  io.println("Cluster actor started")

  // Start the gossip loop
  let _gossip = gossip.start_gossip_loop(cfg, cluster_subject)
  io.println("Gossip loop started")

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
