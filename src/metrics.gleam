import gleam/dynamic.{type Dynamic}
import gleam/erlang
import gleam/float
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

/// Get memory info via FFI
@external(erlang, "metrics_ffi", "get_memory_info")
fn ffi_get_memory_info() -> Dynamic

/// Get CPU load via FFI
@external(erlang, "metrics_ffi", "get_cpu_load")
fn ffi_get_cpu_load() -> Dynamic

/// Get process count via FFI
@external(erlang, "metrics_ffi", "get_process_count")
fn ffi_get_process_count() -> Dynamic

/// Get uptime via FFI
@external(erlang, "metrics_ffi", "get_uptime_seconds")
fn ffi_get_uptime_seconds() -> Int

/// Get memory information
pub fn get_memory_info() -> Result(MemoryInfo, Nil) {
  let result = ffi_get_memory_info()
  case decode_memory_result(result) {
    Ok(info) -> Ok(info)
    Error(_) -> Error(Nil)
  }
}

fn decode_memory_result(dyn: Dynamic) -> Result(MemoryInfo, Nil) {
  case dynamic.tuple2(dynamic.dynamic, dynamic.dynamic)(dyn) {
    Ok(#(tag, data)) -> {
      case dynamic.atom(tag) {
        Ok(atom) if atom == erlang.Atom("ok") -> {
          case dynamic.tuple3(dynamic.int, dynamic.int, dynamic.float)(data) {
            Ok(#(total, used, pct)) -> Ok(MemoryInfo(total, used, pct))
            Error(_) -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

fn erlang_atom(name: String) -> erlang.Atom {
  erlang.Atom(name)
}

/// Get CPU load information
pub fn get_cpu_load() -> Result(CpuLoad, Nil) {
  let result = ffi_get_cpu_load()
  case decode_cpu_result(result) {
    Ok(load) -> Ok(load)
    Error(_) -> Error(Nil)
  }
}

fn decode_cpu_result(dyn: Dynamic) -> Result(CpuLoad, Nil) {
  case dynamic.tuple2(dynamic.dynamic, dynamic.dynamic)(dyn) {
    Ok(#(tag, data)) -> {
      case dynamic.atom(tag) {
        Ok(atom) if atom == erlang.Atom("ok") -> {
          case
            dynamic.tuple3(dynamic.float, dynamic.float, dynamic.float)(data)
          {
            Ok(#(l1, l5, l15)) -> Ok(CpuLoad(l1, l5, l15))
            Error(_) -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Get process count
pub fn get_process_count() -> Int {
  let result = ffi_get_process_count()
  case dynamic.tuple2(dynamic.dynamic, dynamic.int)(result) {
    Ok(#(_, count)) -> count
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
  let timestamp = erlang.system_time(erlang.Second)

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
pub fn decoder() -> dynamic.Decoder(NodeMetrics) {
  dynamic.decode12(
    NodeMetrics,
    dynamic.field("replica_id", dynamic.string),
    dynamic.field("region", dynamic.string),
    dynamic.field("ip_address", dynamic.string),
    dynamic.field("memory_total_mb", dynamic.int),
    dynamic.field("memory_used_mb", dynamic.int),
    dynamic.field("memory_percent", dynamic.float),
    dynamic.field("cpu_load_1", dynamic.float),
    dynamic.field("cpu_load_5", dynamic.float),
    dynamic.field("cpu_load_15", dynamic.float),
    dynamic.field("process_count", dynamic.int),
    dynamic.field("uptime_seconds", dynamic.int),
    dynamic.field("timestamp", dynamic.int),
  )
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
