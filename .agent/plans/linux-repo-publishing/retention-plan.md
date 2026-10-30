# Persistent repository implementation plan

Input: [retention-prd.md](retention-prd.md). Implement in the existing Rust
`release-cli` and `sl/linux-repo-publishing` worktree. Preserve the unrelated
working change in `release-cli/src/newsfeed_cmd.rs`.

## Constraints and design review

Use the current source and Nix `.#release-cli` shell. No deployment credentials,
live bucket writes, commits or changes to the sibling packaging worktree are
needed. The established `.agent/plans/linux-repo-publishing` artifacts are reused
instead of bootstrapping a second project-wide specification system. No existing
`.specify` constitution or hooks are present. Rust, Nix, existing signing rules,
and the user's failure-recovery agreement apply before and after design.

Current source accepts multiple packages but replaces indexes from one flat
input directory. `arch_repo::prepare` already stages/validates signatures and
builds databases using `repo-add`. `S3Client::upload_repo_bytes` already compares
remote bytes and maintains checksums. Native names are metadata-driven, so the
unified pilot needs no per-environment mapping in the publisher.

## Data and execution

Add `linux_repo_state.rs` with a versioned JSON manifest, target binding,
native package identities, immutable SHA-256/size records for package artifacts,
and optional retirement timestamps. Keep flat package snapshots under
`STATE/packages/`. Use an exclusive `File::try_lock` guard and atomic, synced
manifest replacement. Validate persisted paths before joining them to state or
building remote keys. Uncommitted staging files are never GC targets.

Read metadata using dpkg-deb, rpm and bsdtar. Compare using dpkg's comparator,
RPM's native `rpm.vercmp` via its Lua macro, and Arch `vercmp`; invoke commands
directly without a shell and validate interpolation into RPM's macro. Group by
format/name/architecture. Order retained Arch inputs oldest first so repo-add
selects the current version, while staging both retained versions and signatures.

Extend `publish-linux-repos` with required `--state-dir` (`REPO_STATE_DIR`),
`--initialize`, and `--gc-grace-days` (default 7). Existing skip switches apply
throughout the lifecycle. Import incrementally; preview operates on a hypothetical
merged manifest. Existing empty input directories can trigger reindex/GC from
state. Persist successful imports before attempting publication so a failed upload
can be retried without the release folder.

Prepare signed APT/YUM/Arch trees once, then upload their package objects before
metadata. Verify exact metadata checksums using S3 HEAD before committing newly
retired timestamps. Only then collect grace-expired objects (including new
withdrawals when grace is explicitly zero).
Retirements are durable before deletion; partial deletion remains retryable.
Do not scan the bucket and infer ownership from suffixes.

## Validation and rollout

Tests use temporary state, real native package tools, isolated GPG and a local S3
fixture. Cover independent product histories, numeric/prerelease/epoch ordering,
old imports, identity conflicts, dry runs, corruption, locking, skips, grace,
failed publication and retryable deletion. Verify Arch databases actually retain
the newest package identity even when filenames sort in the opposite order.

Run Cargo tests, Rust formatting/treefmt and Clippy through `nix develop
.#release-cli`. No frontend behavior changes or Git commits are planned.
Document first-run adoption of all currently published packages and signatures,
backups of state, the sole publishing host rule and a dry-run rollout. Exact pilot
package IDs and legacy-package migration remain coordinated with the sibling
packaging work; synthetic fixture IDs are not deployed names.

## Implementation evidence — 2026-10-29

Implemented locally in `linux_repo_state.rs`, `linux_repo_publish.rs`, the shared
repository writers, S3 helpers and CLI. Nix check inputs include dpkg and RPM for
native package fixtures. Ops-shell usage and deployment instructions include the
required state directory and first-run initialization.

Final Cargo result: **42 passed, 0 failed, 1 ignored**. The pre-existing privileged
pacman check remains optional. Clippy with `--all-targets -- --deny warnings` and
treefmt checks pass for changed implementation files. CLI help confirms required
state, initialization and configurable grace options.

The first concurrent test run exposed implicit lock release waiting for duplicate
file descriptors. Inventory now explicitly unlocks on drop; an inherited-handle
regression test and the full concurrent suite pass. A test-only Clippy borrow
finding was also corrected without changing the publication design.

No live bucket operations or deployment credentials were used. Storage-origin
checks and deletes were tested through the real AWS SDK at a local HTTP fixture;
signed APT/YUM generations and Arch current-version selection use native tools.
