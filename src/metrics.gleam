import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json.{type Json}
import gleam/result

/// Metrics for a single node
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

/// Memory info from FFI
pub type MemoryInfo {
  MemoryInfo(total_mb: Int, used_mb: Int, percent: Float)
}

/// CPU load from FFI
pub type CpuLoad {
  CpuLoad(load_1: Float, load_5: Float, load_15: Float)
}

/// Ensure os_mon is started
@external(erlang, "metrics_ffi", "ensure_os_mon_started")
pub fn ensure_os_mon_started() -> Nil

/// Get memory info via FFI - returns {ok, {Total, Used, Percent}} or {error, _}
@external(erlang, "metrics_ffi", "get_memory_info")
fn ffi_get_memory_info() -> Result(#(Int, Int, Float), Nil)

/// Get CPU load via FFI - returns {ok, {Load1, Load5, Load15}} or {error, _}
@external(erlang, "metrics_ffi", "get_cpu_load")
fn ffi_get_cpu_load() -> Result(#(Float, Float, Float), Nil)

/// Get process count via FFI
@external(erlang, "metrics_ffi", "get_process_count")
fn ffi_get_process_count() -> Result(Int, Nil)

/// Get uptime via FFI
@external(erlang, "metrics_ffi", "get_uptime_seconds")
fn ffi_get_uptime_seconds() -> Int

/// Get current system time in seconds
@external(erlang, "erlang", "system_time")
fn erlang_system_time(unit: SystemTimeUnit) -> Int

pub type SystemTimeUnit {
  Second
}

/// Get memory information
pub fn get_memory_info() -> Result(MemoryInfo, Nil) {
  case ffi_get_memory_info() {
    Ok(#(total, used, pct)) -> Ok(MemoryInfo(total, used, pct))
    Error(_) -> Error(Nil)
  }
}

/// Get CPU load information
pub fn get_cpu_load() -> Result(CpuLoad, Nil) {
  case ffi_get_cpu_load() {
    Ok(#(l1, l5, l15)) -> Ok(CpuLoad(l1, l5, l15))
    Error(_) -> Error(Nil)
  }
}

/// Get process count
pub fn get_process_count() -> Int {
  case ffi_get_process_count() {
    Ok(count) -> count
    Error(_) -> 0
  }
}

/// Get uptime in seconds
pub fn get_uptime_seconds() -> Int {
  ffi_get_uptime_seconds()
}

/// Collect all metrics for this node
pub fn collect(replica_id: String, region: String, ip: String) -> NodeMetrics {
  let memory = get_memory_info() |> result.unwrap(MemoryInfo(0, 0, 0.0))
  let cpu = get_cpu_load() |> result.unwrap(CpuLoad(0.0, 0.0, 0.0))
  let procs = get_process_count()
  let uptime = get_uptime_seconds()
  let timestamp = erlang_system_time(Second)

  NodeMetrics(
    replica_id: replica_id,
    region: region,
    ip_address: ip,
    memory_total_mb: memory.total_mb,
    memory_used_mb: memory.used_mb,
    memory_percent: memory.percent,
    cpu_load_1: cpu.load_1,
    cpu_load_5: cpu.load_5,
    cpu_load_15: cpu.load_15,
    process_count: procs,
    uptime_seconds: uptime,
    timestamp: timestamp,
  )
}

/// Convert metrics to JSON
pub fn to_json(m: NodeMetrics) -> Json {
  json.object([
    #("replica_id", json.string(m.replica_id)),
    #("region", json.string(m.region)),
    #("ip_address", json.string(m.ip_address)),
    #("memory_total_mb", json.int(m.memory_total_mb)),
    #("memory_used_mb", json.int(m.memory_used_mb)),
    #("memory_percent", json.float(m.memory_percent)),
    #("cpu_load_1", json.float(m.cpu_load_1)),
    #("cpu_load_5", json.float(m.cpu_load_5)),
    #("cpu_load_15", json.float(m.cpu_load_15)),
    #("process_count", json.int(m.process_count)),
    #("uptime_seconds", json.int(m.uptime_seconds)),
    #("timestamp", json.int(m.timestamp)),
  ])
}

/// Decode metrics from JSON
pub fn decoder() -> Decoder(NodeMetrics) {
  use replica_id <- decode.field("replica_id", decode.string)
  use region <- decode.field("region", decode.string)
  use ip_address <- decode.field("ip_address", decode.string)
  use memory_total_mb <- decode.field("memory_total_mb", decode.int)
  use memory_used_mb <- decode.field("memory_used_mb", decode.int)
  use memory_percent <- decode.field("memory_percent", decode.float)
  use cpu_load_1 <- decode.field("cpu_load_1", decode.float)
  use cpu_load_5 <- decode.field("cpu_load_5", decode.float)
  use cpu_load_15 <- decode.field("cpu_load_15", decode.float)
  use process_count <- decode.field("process_count", decode.int)
  use uptime_seconds <- decode.field("uptime_seconds", decode.int)
  use timestamp <- decode.field("timestamp", decode.int)
  decode.success(NodeMetrics(
    replica_id:,
    region:,
    ip_address:,
    memory_total_mb:,
    memory_used_mb:,
    memory_percent:,
    cpu_load_1:,
    cpu_load_5:,
    cpu_load_15:,
    process_count:,
    uptime_seconds:,
    timestamp:,
  ))
}

/// Format uptime as human-readable string
pub fn format_uptime(seconds: Int) -> String {
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
