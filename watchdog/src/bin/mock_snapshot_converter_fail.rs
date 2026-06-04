// Test helper: mock snapshot-converter that always exits with a non-zero status.
// Used to test the PARTIAL_SYNC_CONVERSION_FAILED error path.

fn main() {
    eprintln!("mock-snapshot-converter-fail: simulating conversion failure");
    std::process::exit(1);
}
