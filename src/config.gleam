import envoy
import gleam/crypto
import gleam/int
import gleam/result

/// Configuration loaded from Railway environment variables
pub type Config {
  Config(
    port: Int,
    host: String,
    replica_id: String,
    replica_region: String,
    private_domain: String,
    service_name: String,
    gossip_interval_ms: Int,
  )
}

/// Load configuration from environment variables
/// Falls back to sensible defaults for local development
pub fn load() -> Config {
  let port =
    envoy.get("PORT")
    |> result.try(int.parse)
    |> result.unwrap(8080)

  let host =
    envoy.get("HOST")
    |> result.unwrap("::")

  let replica_id =
    envoy.get("RAILWAY_REPLICA_ID")
    |> result.unwrap(generate_local_id())

  let replica_region =
    envoy.get("RAILWAY_REPLICA_REGION")
    |> result.unwrap("local")

  let private_domain =
    envoy.get("RAILWAY_PRIVATE_DOMAIN")
    |> result.unwrap("localhost")

  let service_name =
    envoy.get("RAILWAY_SERVICE_NAME")
    |> result.unwrap("cluster_monitor")

  let gossip_interval_ms =
    envoy.get("GOSSIP_INTERVAL_MS")
    |> result.try(int.parse)
    |> result.unwrap(5000)

  Config(
    port: port,
    host: host,
    replica_id: replica_id,
    replica_region: replica_region,
    private_domain: private_domain,
    service_name: service_name,
    gossip_interval_ms: gossip_interval_ms,
  )
}

/// Generate a random ID for local development
fn generate_local_id() -> String {
  crypto.strong_random_bytes(4)
  |> bytes_to_hex
}

fn bytes_to_hex(bytes: BitArray) -> String {
  do_bytes_to_hex(bytes, "")
}

fn do_bytes_to_hex(bytes: BitArray, acc: String) -> String {
  case bytes {
    <<byte:int, rest:bits>> -> {
      let hex = int.to_base16(byte)
      let padded = case byte < 16 {
        True -> "0" <> hex
        False -> hex
      }
      do_bytes_to_hex(rest, acc <> padded)
    }
    _ -> acc
  }
}
