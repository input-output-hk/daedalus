// Test helper: mock cardano-wallet that exits immediately with code 1.
// Used to exercise the wallet restart / circuit-breaker path in integration tests.
fn main() {
    std::process::exit(1);
}
