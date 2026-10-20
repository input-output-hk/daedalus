# Arch publisher deployment

The implementation runs on the deployment server using its existing GPG signing
configuration and package directory. The development environment does not need
the IOHK secret key or root access.

## Run on the deployment server

Build and run the updated Rust publisher through Nix, with the usual deployment
credentials loaded:

```bash
nix develop .#release-cli
source ops/.envrc.local
cargo run --manifest-path release-cli/Cargo.toml -- publish-linux-repos \
  <installers-directory> \
  --bucket "$REPO_BUCKET" --bucket-url "$REPO_BUCKET_URL" \
  --no-acl --skip-apt --skip-yum
```

The publisher resolves the signing identity from --gpg-user or GPG_USER, using
the existing IOHK signing subkey by default. It verifies both required IOHK
fingerprints. It reuses verified binary package signatures or dearmors existing
.asc signatures, stages immutable package bytes, generates both databases with
repo-add, signs the final archives, and publishes actual contents at all aliases.
Missing package signatures are created with the deployment server's signing key.

All package files and signatures are uploaded before repository metadata. Every
Arch object receives no-store cache headers. Before each APT, YUM or Arch upload,
the publisher checks object size and checksum using HEAD. Matching SHA-256
metadata skips the upload; older single-part MD5 ETags can establish equality
without downloading or re-uploading packages. Equal contents needing checksum
metadata or header updates use a conditional server-side copy. Ambiguous ETags
require one upload to establish SHA-256 metadata. Changed content is uploaded.
Separate S3 writes are not atomic:
no-store prevents new caching, but cannot purge previously cached responses or
guarantee a cutover with no database/signature mismatch window. Strict atomic
cutover requires coordination at the serving layer.

## Validation

Local checks: 15 active tests and Clippy with --all-targets -- --deny warnings pass.
Tests use an isolated temporary signing key and a public IOHK signature fixture;
they need neither the deployment key nor sudo. Coverage includes signature reuse,
changed-byte rejection, fingerprint checks, signed database aliases and file lists.

An additional ignored test runs pacman in a temporary installation root using
sudo. It is optional deployment/client validation, not a prerequisite for building
or running the publisher. After deployment, verify public signatures and run the
requested clean Arch client acceptance commands with SigLevel = Required.

## Prior manual actions

Before the user clarified that this task was code-only, the assistant manually
added the package .sig and daedalus.db.sig to the live repository on 2026-10-20.
Those additions reused verified existing IOHK signatures; package and database
bytes were unchanged. They did not run through the updated publisher. No further
deployment actions are part of this local code task.
