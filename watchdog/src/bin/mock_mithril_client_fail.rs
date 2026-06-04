// Test helper: mock mithril-client that succeeds on "show" but fails on "download".
// Used to test the PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED error path.

use std::env;
use std::fs;
use std::path::Path;

fn get_flag(args: &[String], flag: &str) -> Option<String> {
    args.windows(2).find(|w| w[0] == flag).map(|w| w[1].clone())
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let is_show = args.iter().any(|a| a == "show");
    let is_download = args.iter().any(|a| a == "download");

    if is_show {
        let certified: u64 = env::var("MOCK_CERTIFIED_IMMUTABLE")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(999_999);
        println!(r#"{{"beacon":{{"immutable_file_number":{certified}}}}}"#);
        return;
    }

    if is_download {
        // Create partial download dir so the error cleanup path is exercised.
        if let Some(dir) = get_flag(&args, "--download-dir") {
            let _ = fs::create_dir_all(Path::new(&dir));
        }
        eprintln!("mock-mithril-client-fail: simulating download failure");
        std::process::exit(1);
    }
}
