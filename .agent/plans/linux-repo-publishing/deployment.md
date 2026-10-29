# Linux repository deployment and automatic collection

The implementation runs on the deployment server using its existing GPG signing
configuration and persistent repository inventory. Production Mainnet and the
unified pilot are separate native package identities in each repository. Pilot's
runtime environments are not individual publishing/retention channels. Final pilot
package naming comes from the sibling packaging worktree.

The development environment does not need the IOHK secret key or root access.
These instructions describe deployment; this implementation session made no live
repository changes.

## Bootstrap once

Choose a durable state directory outside ephemeral installer/build directories,
for example `/var/lib/daedalus-repos`. All publishers for this bucket/prefix must
run on the same host and use this directory. The filesystem lock does not
coordinate independent state directories or hosts. Back up `inventory.json` and
the entire `packages/` directory together while no publication is running. Never
remove or replace `publish.lock` while a publisher might be running.

Prepare a flat bootstrap directory containing **all existing packages that must
remain available**, with their adjacent `.asc`/`.sig` files. The publisher does
not automatically download/adopt old bucket contents. An incomplete bootstrap
would omit existing products from regenerated indexes. Unknown old bucket objects
are never deleted by inventory GC. Legacy per-environment package migration must
be decided separately from adoption of the unified pilot identity.

Preview initialization without importing, signing or accessing the bucket:

```bash
nix develop .#release-cli
export REPO_STATE_DIR=/var/lib/daedalus-repos
cargo run --manifest-path release-cli/Cargo.toml -- publish-linux-repos \
  <complete-bootstrap-directory> \
  --bucket "$REPO_BUCKET" --bucket-url "$REPO_BUCKET_URL" \
  --initialize --dry-run --no-acl
```

After checking the retained/withdrawn list, run the same command without
`--dry-run`, with deployment credentials loaded. Keep `--initialize` for this
first real run only. A successfully persisted import initializes state even when
subsequent signing/upload fails; omit `--initialize` on that retry. State is bound
to its bucket/prefix and effective endpoint and rejects another target.

## Run on the deployment server

After bootstrap, input directories contain only the incoming release. Publishing
pilot alone preserves production and vice versa:

```bash
nix develop .#release-cli
source ops/.envrc.local
export REPO_STATE_DIR=/var/lib/daedalus-repos
cargo run --manifest-path release-cli/Cargo.toml -- publish-linux-repos \
  <installers-directory> \
  --bucket "$REPO_BUCKET" --bucket-url "$REPO_BUCKET_URL" \
  --no-acl
```

Use `--dry-run` to preview imports, retained versions, withdrawals and collection
without changing the inventory or bucket. `--skip-apt`, `--skip-yum` and
`--skip-arch` exclude those formats from import, indexing, retirement and GC.
An existing empty installer directory can trigger reindexing and GC from the
inventory without supplying another release. Repeated identical imports are safe;
changed bytes under the same filename or native identity are rejected.

## Retention and automatic GC

Every deployment retains the newest two native package versions (current and
previous) independently per package name, format and architecture. Version order
uses the package manager's comparator, including epochs and release revisions.
Separate package revisions count separately. APT/YUM index both retained versions.
Arch indexes the current version of each package in both databases; its previous
package and signature remain directly downloadable for rollback.

After preparing all enabled signed repositories, the publisher uploads package
files/signatures before metadata and verifies metadata checksums at the storage
origin. Only successful verification starts the grace clock for newly withdrawn
versions. Retained copies survive deletion of the incoming installer directory.

The default grace is seven days after withdrawal. A later successful deployment
automatically deletes expired package objects, adjacent signatures and local
copies. Override with `--gc-grace-days N`; zero explicitly permits collection of
newly withdrawn versions during that deployment. There is no background collector:
if no deployments occur, expired files remain until the next invocation.

Failed imports/signing/uploads/verification do not start new retirement clocks
or run GC. A failed deletion stops the command and leaves a retryable retired
record; repeating deployment retries deletion, including objects already absent.
No broad bucket sweep or prefix deletion is performed. Preserve the inventory
backup rather than attempting to repair lifecycle state by deleting JSON entries.

## Signing and object storage

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

Local validation uses Cargo tests and Clippy with
`--all-targets -- --deny warnings` in `nix develop .#release-cli`.
Tests use an isolated temporary signing key and a public IOHK signature fixture;
they need neither the deployment key nor sudo. Coverage includes signature reuse,
changed-byte rejection, fingerprint checks, signed database aliases and file lists.
Retention tests use real DEB/RPM/Arch fixtures, isolated signing keys and local
storage mocks. They cover independent production/pilot histories, native ordering,
Arch current-version indexes, signed APT/YUM generations, immutable imports,
locking, dry runs, publication failures, grace and retryable collection. S3 SDK
tests cover origin checksums and prefix-aware deletion against a local endpoint.

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
