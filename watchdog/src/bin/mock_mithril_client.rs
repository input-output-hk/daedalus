// Test helper: mock mithril-client.
//
// Handles two subcommands that the watchdog uses:
//
//   cardano-db snapshot show latest
//     → prints snapshot metadata JSON to stdout and exits 0.
//
//   cardano-db download latest --download-dir <dir> [--include-ancillary]
//     → emits JSON progress lines, creates a valid staged-DB layout under
//       <dir>, and exits 0.
//
// The snapshot's immutable_file_number is set very high (999_999) so the
// behind-ness probe always concludes that a sync is needed when force=false.

use std::env;
use std::fs;
use std::path::Path;
use std::thread::sleep;
use std::time::Duration;

fn get_flag(args: &[String], flag: &str) -> Option<String> {
    args.windows(2).find(|w| w[0] == flag).map(|w| w[1].clone())
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let is_show = args.iter().any(|a| a == "show");
    let is_download = args.iter().any(|a| a == "download");

    if is_show {
        // MOCK_CERTIFIED_IMMUTABLE overrides the returned immutable file number;
        // defaults to 999999 so the behind-ness probe always triggers a sync.
        let certified: u64 = env::var("MOCK_CERTIFIED_IMMUTABLE")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(999_999);
        println!(r#"{{"beacon":{{"immutable_file_number":{certified}}}}}"#);
        return;
    }

    if is_download {
        let download_dir = get_flag(&args, "--download-dir")
            .expect("mock-mithril-client download requires --download-dir");

        // Emit JSON progress lines (watchdog reads these from both stdout/stderr).
        for i in 1u64..=5 {
            println!(
                r#"{{"files_downloaded":{i},"files_total":5,"bytes_downloaded":{b},"bytes_total":500,"seconds_elapsed":{s}.0,"step_num":1,"total_steps":4}}"#,
                b = i * 100,
                s = i,
            );
            sleep(Duration::from_millis(20));
        }
        // Emit a verifying-phase progress line.
        println!(
            r#"{{"files_downloaded":5,"files_total":5,"bytes_downloaded":500,"bytes_total":500,"seconds_elapsed":5.0,"step_num":2,"total_steps":4}}"#
        );

        // mithril-client creates a `db/` subdirectory inside --download-dir.
        // Replicate that layout so validate_staged() finds the expected files:
        //   <dir>/db/clean              (file)
        //   <dir>/db/immutable/         (dir)
        //   <dir>/db/ledger/<slot>/     (dir, slot number used by run_converter)
        //   <dir>/db/protocolMagicId   (file)
        let db = Path::new(&download_dir).join("db");
        fs::create_dir_all(db.join("immutable")).unwrap();
        fs::create_dir_all(db.join("ledger").join("12345")).unwrap();
        fs::write(db.join("clean"), b"").unwrap();
        fs::write(db.join("protocolMagicId"), b"764824073").unwrap();
    }
}
