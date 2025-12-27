import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/result

/// Resolve a hostname to all its IP addresses
@external(erlang, "discovery_ffi", "resolve_all_ips")
fn ffi_resolve_all_ips(hostname: String) -> Dynamic

/// Get local IP address
@external(erlang, "discovery_ffi", "get_local_ip")
fn ffi_get_local_ip() -> Dynamic

/// Discover all peer IP addresses from Railway private domain
pub fn discover_peers(private_domain: String) -> List(String) {
  let result = ffi_resolve_all_ips(private_domain)
  case dynamic.list(dynamic.string)(result) {
    Ok(ips) -> ips
    Error(_) -> []
  }
}

/// Get local IP address for this replica
pub fn get_local_ip() -> Result(String, Nil) {
  let result = ffi_get_local_ip()
  case dynamic.tuple2(dynamic.dynamic, dynamic.string)(result) {
    Ok(#(tag, ip)) -> {
      case dynamic.atom(tag) {
        Ok(_) -> Ok(ip)
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Filter out our own IP from peer list
pub fn filter_self(peers: List(String), self_ip: String) -> List(String) {
  list.filter(peers, fn(peer) { peer != self_ip })
}
