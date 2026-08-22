// Test helper: mock cardano-node that exits immediately with code 1.
// Used to exercise the "node crash before socket ready" path in integration tests.
fn main() {
    std::process::exit(1);
}
