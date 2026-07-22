use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize)]
#[serde(tag = "cmd", rename_all = "snake_case")]
pub enum Command {
    Stop,
}

#[derive(Debug, Serialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum Event {
    NodeStarted { pid: u32, started_at_unix_ms: u64 },
    WalletStarted { pid: u32, started_at_unix_ms: u64 },
    WalletReady { port: u16 },
    WalletExited { code: Option<i32>, signal: Option<String> },
    WalletRestarting { attempt: u32 },
    NodeExited { code: Option<i32>, signal: Option<String> },
    Stopped,
    #[allow(dead_code)]
    Error { message: String },
}

pub fn emit(event: &Event) {
    if let Ok(line) = serde_json::to_string(event) {
        println!("{line}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_json(ev: &Event) -> serde_json::Value {
        serde_json::to_value(ev).unwrap()
    }

    #[test]
    fn node_started() {
        let j = to_json(&Event::NodeStarted { pid: 42, started_at_unix_ms: 1000 });
        assert_eq!(j["event"], "node_started");
        assert_eq!(j["pid"], 42);
        assert_eq!(j["started_at_unix_ms"], 1000);
    }

    #[test]
    fn wallet_started() {
        let j = to_json(&Event::WalletStarted { pid: 99, started_at_unix_ms: 2000 });
        assert_eq!(j["event"], "wallet_started");
        assert_eq!(j["pid"], 99);
        assert_eq!(j["started_at_unix_ms"], 2000);
    }

    #[test]
    fn wallet_ready() {
        let j = to_json(&Event::WalletReady { port: 8090 });
        assert_eq!(j["event"], "wallet_ready");
        assert_eq!(j["port"], 8090);
    }

    #[test]
    fn wallet_exited_with_code() {
        let j = to_json(&Event::WalletExited { code: Some(1), signal: None });
        assert_eq!(j["event"], "wallet_exited");
        assert_eq!(j["code"], 1);
        assert!(j["signal"].is_null());
    }

    #[test]
    fn wallet_exited_with_signal() {
        let j = to_json(&Event::WalletExited { code: None, signal: Some("SIGTERM".into()) });
        assert_eq!(j["event"], "wallet_exited");
        assert!(j["code"].is_null());
        assert_eq!(j["signal"], "SIGTERM");
    }

    #[test]
    fn wallet_restarting() {
        let j = to_json(&Event::WalletRestarting { attempt: 3 });
        assert_eq!(j["event"], "wallet_restarting");
        assert_eq!(j["attempt"], 3);
    }

    #[test]
    fn node_exited() {
        let j = to_json(&Event::NodeExited { code: Some(0), signal: None });
        assert_eq!(j["event"], "node_exited");
        assert_eq!(j["code"], 0);
    }

    #[test]
    fn stopped() {
        let j = to_json(&Event::Stopped);
        assert_eq!(j["event"], "stopped");
    }

    #[test]
    fn stop_command_deserializes() {
        let cmd: Command = serde_json::from_str(r#"{"cmd":"stop"}"#).unwrap();
        assert!(matches!(cmd, Command::Stop));
    }

    #[test]
    fn unknown_command_fails() {
        assert!(serde_json::from_str::<Command>(r#"{"cmd":"restart"}"#).is_err());
    }
}
