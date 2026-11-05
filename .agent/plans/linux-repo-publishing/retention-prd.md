# Persistent Linux repositories and automatic retention

**Branch:** `sl/linux-repo-publishing`

**Date:** 2026-10-29

**Status:** Implemented and validated locally; deployment pending

**Plan:** [retention-plan.md](retention-plan.md)

**Tasks:** [retention-tasks.json](retention-tasks.json)

Continue the existing publisher in this worktree. These Spec Kit specification,
plan and task stages extend its established planning workspace; no new numbered
feature or branch is allocated.

## Outcome and scope

Each APT, YUM and Arch repository carries two independent products: production
Mainnet and the unified pilot distribution. The pilot includes several runtime
environments (Preview, Preprod and Mainnet Flight in the sibling packaging spec);
these are not separate retention channels. A pilot deployment cannot evict a
production release, even when both include access to the mainnet chain.

Deployment runs the Rust publisher on the signing/deployment host against a
persistent local inventory, then publishes static objects to S3/R2. Automatically
retain the current and previous package release independently per product,
format and architecture. Withdraw older releases from indexes and collect their
objects after a grace period. Never infer product identity from a filename or
the runtime blockchain/network magic.

Package names come from native package metadata. Production and pilot must have
distinct stable native package names; final pilot naming belongs to the sibling
`shared-test-environment-packaging` worktree and is not yet fixed by its spec.
Tests may use `daedalus-pilot` as a synthetic identity, not a packaging decision.
The publisher does not generate or modify the unified installer itself.

## User stories and acceptance

### US1 — Independent releases (P1)

Given production packages already published, when a deployment imports only a
pilot release, all enabled repository indexes still contain production and pilot.
Updating either product leaves the other's current and previous releases intact.
Given incoming files disappear from the release directory, retained copies remain
usable in later deployments. Repeating an identical import is idempotent;
replacing the same filename or native package identity with different bytes fails.

### US2 — Automatic collection during deployment (P1)

Given three production versions and two pilot versions, when deployment succeeds,
production's newest two versions and both pilot versions remain retained.
Native package version ordering determines current/previous, including epochs,
release revisions and prereleases; arrival order and lexical sorting do not.
Importing an older release does not demote a newer current release.

APT and YUM index both retained releases. Arch indexes the current release per
package name, retaining the previous package and signature as a direct rollback
download. Both Arch database kinds must identify the same current versions.

An expired package is removed from indexes before its grace clock starts.
The default grace is seven days after a successful, verified publication that
excludes it. A later successful deployment automatically deletes eligible package
objects, adjacent signatures, and local inventory copies. The grace is configurable
in days, including zero for explicit immediate collection.

### US3 — Preview and recover safely (P1)

Dry run reports retained, newly retired, waiting and deletion-eligible packages
without importing files, changing inventory state, signing or contacting S3.
It shows the effective policy. The publication lock may be acquired for a stable
snapshot. Two publishers sharing state cannot run concurrently.

If import, signing, publication or verification fails, no new retirement clocks
start and no collection runs. If deletion fails, its pending record survives and
a later deployment retries it. Interrupted imports never create partial committed
package files. Skipped formats are neither imported, retired nor collected.

## Functional requirements

- **FR-001:** Require an explicit persistent state directory for deployment;
  bind it to its bucket/prefix and endpoint, rejecting reuse for another target.
- **FR-002:** Require explicit initialization from a complete existing package
  inventory on first use. No automatic adoption from an incomplete release folder.
  Document bootstrap and backup; ordinary deployment input is incremental.
- **FR-003:** Snapshot immutable package bytes and available `.asc`/`.sig`
  sidecars, record SHA-256 and size, read native name/version/architecture, and
  reject corrupt inventory, conflicting identities and unsupported architectures.
- **FR-004:** Select exactly the newest two distinct native versions for each
  native package name, format and architecture; one is allowed before a second
  exists. No per-runtime-environment retention counters or cross-product quota.
- **FR-005:** Serialize import, selection, publication, retirement and collection
  using one deployment-host filesystem lock. Release the lock on process exit.
- **FR-006:** Prepare and sign complete enabled repository trees before uploading;
  upload packages/signatures before indexes; verify the resulting metadata at the
  object-storage origin before recording retirement or deleting any object.
- **FR-007:** Never delete active packages, unknown bucket keys, repository public
  keys or metadata as part of package GC. Derive deletion keys from validated
  inventory, honor bucket prefixes, and retain retry state until deletion finishes.
- **FR-008:** Reuse signature validation and checksum-aware upload behavior.
  Rebuild indexes from inventory, not the incoming release directory.
- **FR-009:** Dry run has no inventory/package/bucket mutations; report the
  selection and automatic-GC actions that a real deployment would perform.
- **FR-010:** Preserve explicitly unsigned local `serve` behavior; persistent
  retention is a deployment concern, not a change to local update emulation.

## Edge cases and limits

Handle missing inventory files, unsafe filenames, modified bytes, equal native
versions with different spellings, incomplete formats, lock contention, failed
signing, partial uploads, clock rollback and partial deletion. Existing legacy
per-environment packages require an explicit migration decision; do not silently
map their versions into the new pilot identity or delete unknown legacy objects.

Supported deployment architecture remains the existing Linux x86_64 target.
Separate package versions/revisions count as separate releases for retention.
Cross-format all-or-nothing releases, manual rollback promotion, pinning and
remote inventory recovery are outside this first implementation.

S3 writes are not an atomic generation cutover. The existing index/signature
mismatch window remains; a filesystem lock coordinates one host, not arbitrary
writers on multiple machines. All deployments for a target must use this state
directory on the designated host. Origin verification does not purge CDN caches.

## Success criteria and requirements checklist

- [x] Production/pilot isolation and unified-pilot ownership are explicit.
- [x] Current/previous means native version order, not input or filename order.
- [x] Arch index semantics and retained rollback files are distinguished.
- [x] GC ordering, seven-day default, preview and failure recovery are specified.
- [x] Native package naming is an integration contract, not invented here.
- [x] Automated sequential-deployment tests preserve both products in all formats.
- [x] Automated failure/GC tests prove active and grace-period files survive.
- [x] Rust tests, formatting and Clippy pass in the release-cli Nix shell.
- [x] Documented bootstrap and repeat deployment commands match implemented CLI.
