use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "drt", about = "Daedalus release tool", version)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Hash installers, collect existing .asc signatures, upload to S3,
    /// and push daedalus-latest-version.json.
    ///
    /// Expects an INSTALLERS_DIR containing:
    ///   - one or more installer files (.bin, .pkg, .exe)
    ///   - a 'meta.json' file (written by `drt fetch-installers`, or manually)
    ///   - (optional) pre-existing .asc signature files from `drt sign`
    Release {
        /// Directory containing installer files and a 'meta.json' file.
        #[arg(default_value = "installers")]
        installers_dir: PathBuf,

        /// S3 bucket name (e.g. update-cardano-mainnet.iohk.io).
        #[arg(long)]
        bucket: String,

        /// Public base URL for the bucket, without protocol
        /// (e.g. update-cardano-mainnet.iohk.io).
        #[arg(long)]
        bucket_url: String,

        /// Release notes text to embed in daedalus-latest-version.json.
        #[arg(long)]
        release_notes: Option<String>,

        /// Dry-run: hash but do NOT upload to S3.
        /// Prints the version JSON that would be uploaded.
        #[arg(long)]
        dry_run: bool,

        /// Test mode: also upload newsfeed stub and verification file to S3.
        /// In production, newsfeeds are managed via GitHub — omit this flag.
        #[arg(long)]
        test: bool,
    },

    /// Code-sign macOS (.pkg) and/or Windows (.exe) installers via SSH
    /// signing hosts, and/or GPG-sign all installers.
    ///
    /// Signing hosts are read from environment variables:
    ///   OSX_SIGN_HOST — SSH host for macOS .pkg signing
    ///   WIN_SIGN_HOST — SSH host for Windows .exe signing (uses HSM)
    ///
    /// The unsigned original is renamed to include 'unsigned' in the name:
    ///   daedalus-7.3.0-mainnet.pkg → daedalus-7.3.0-mainnet-unsigned.pkg
    ///
    /// GPG signing is triggered by --gpg-user or the GPG_USER env var.
    Sign {
        /// Directory containing installer files and a 'meta.json' file.
        #[arg(default_value = "installers")]
        installers_dir: PathBuf,

        /// GPG key ID / e-mail for signing (overrides GPG_USER env var).
        /// Omit to use the default GPG key.
        #[arg(long)]
        gpg_user: Option<String>,

        /// Skip GPG signing entirely.
        #[arg(long)]
        skip_gpg: bool,

        /// Skip macOS / Windows code signing (via SSH).
        #[arg(long)]
        skip_code_sign: bool,

        /// Skip uploading the installer to the signing host (assumes it is
        /// already present from a previous run).  Errors if the remote file
        /// does not exist.  Useful for re-testing signing fixes without
        /// re-uploading the full installer.
        #[arg(long)]
        skip_upload: bool,

        /// Skip code signing the aarch64-darwin (Apple Silicon) installer.
        #[arg(long)]
        skip_darwin: bool,

        /// Skip code signing the x86_64-darwin (Intel) installer.
        #[arg(long)]
        skip_darwin_legacy: bool,

        /// Skip code signing the Windows installer.
        #[arg(long)]
        skip_windows: bool,

        /// Show stderr from signing processes (nix run, ssh, scp).
        /// By default stderr is suppressed and only shown on failure.
        #[arg(short, long)]
        verbose: bool,
    },

    /// Download unsigned installer artifacts from a Hydra eval into a local
    /// directory, ready for `drt sign` / `drt release`.
    ///
    /// Fetches all `installer.<system>.<env>` builds, downloads the .bin /
    /// .pkg / .exe files, verifies SHA-256, and writes a `meta.json` file.
    FetchInstallers {
        /// Hydra eval URL (e.g. https://ci.iog.io/eval/107478).
        #[arg(long)]
        url: String,

        /// Cluster / environment to fetch (e.g. mainnet, preprod, preview).
        #[arg(long)]
        env: String,

        /// Directory to write installers and meta.json into.
        #[arg(long, short)]
        out_dir: PathBuf,
    },

    /// Run a local HTTP server that emulates the S3 update bucket plus
    /// the newsfeed endpoints, for end-to-end tester workflows.
    ///
    /// All URLs printed at startup can be dropped straight into
    /// launcher-config.yaml overrides.
    Serve {
        /// Port to listen on.
        #[arg(short, long, default_value_t = 8080)]
        port: u16,

        /// Address to bind to.
        #[arg(long, default_value = "127.0.0.1")]
        host: String,

        /// Directory with installer files and a 'meta.json' file.
        #[arg(long, default_value = "installers")]
        installers: PathBuf,

        /// Release notes text to embed in the served version JSON.
        #[arg(long)]
        release_notes: Option<String>,
    },
}
