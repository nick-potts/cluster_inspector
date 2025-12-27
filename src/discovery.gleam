import gleam/list

/// Resolve a hostname to all its IP addresses (returns list of IP strings)
@external(erlang, "discovery_ffi", "resolve_all_ips")
pub fn discover_peers(private_domain: String) -> List(String)

/// Get local IP address
@external(erlang, "discovery_ffi", "get_local_ip")
fn ffi_get_local_ip() -> Result(String, Nil)

/// Get local IP address for this replica
pub fn get_local_ip() -> Result(String, Nil) {
  ffi_get_local_ip()
}

/// Filter out our own IP from peer list
pub fn filter_self(peers: List(String), self_ip: String) -> List(String) {
  list.filter(peers, fn(peer) { peer != self_ip })
}
