// Test helper: mock mithril-client that returns a certified immutable count
// below the behind_threshold (20) so the probe always returns "not needed".
// Certified = 10 → behind = 10 - 0 = 10 < 20 → MithrilNotNeeded.

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|a| a == "show") {
        println!(r#"{{"beacon":{{"immutable_file_number":10}}}}"#);
    }
    // download / other subcommands: do nothing (shouldn't be called in not-needed tests)
}
