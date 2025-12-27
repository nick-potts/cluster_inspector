import { Some, None } from "../build/dev/javascript/gleam_stdlib/gleam/option.mjs";

// Load initial state from embedded JSON
export function loadInitialState() {
  const el = document.getElementById("initial-state");
  if (el && el.textContent) {
    return new Some(el.textContent);
  }
  return new None();
}

// WebSocket connection with auto-reconnect
let ws = null;

export function connectWebSocket(onOpen, onClose, onMessage, onError) {
  // Determine WebSocket URL based on current location
  const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
  const host = window.location.host;
  const url = `${protocol}//${host}/ws`;
  
  try {
    ws = new WebSocket(url);
    
    ws.onopen = () => {
      console.log("WebSocket connected");
      onOpen();
    };
    
    ws.onclose = () => {
      console.log("WebSocket disconnected");
      ws = null;
      onClose();
    };
    
    ws.onmessage = (event) => {
      onMessage(event.data);
    };
    
    ws.onerror = (error) => {
      console.error("WebSocket error:", error);
      onError("Connection error");
    };
  } catch (error) {
    console.error("Failed to create WebSocket:", error);
    onError("Failed to connect");
  }
}

export { setTimeout };
