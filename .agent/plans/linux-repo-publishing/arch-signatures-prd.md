# Required Arch repository signatures

Continue the existing `sl/linux-repo-publishing` feature in its current worktree.

## Specification

At https://daedalus.se7enlabs.org/arch, a clean Arch client trusting IOHK's
31B3F22FA0CCE2E8382C04A2807F2ACD0171BC90 primary key must synchronize,
install daedalus-mainnet and synchronize file databases with `SigLevel = Required`.
Use signing subkey CD1F21F62477EB42D7C4F3DEB3AD9541F4DBD0FE.

- Every database package filename resolves to the unchanged package bytes and a
  binary detached OpenPGP `.sig`. Reuse and verify existing `.sig` or dearmored
  `.asc` before considering a fresh signature. Never accept a signature for a
  different package or a different signing identity.
- Generate both package and file databases. Sign their final compressed bytes;
  serve actual bytes at `.db`, `.files`, archive filenames and all `.sig` aliases.
- Keep the corresponding public key at `daedalus-release.gpg`.
- Prepare and verify the entire generation before uploading. Upload packages and
  signatures before metadata. Set no-store on mutable metadata and signatures.
- Local signed repository generation must produce the same Arch artifacts.
  Explicit unsigned local development remains supported.

Acceptance: all referenced packages, package signatures, database aliases and
signatures return HTTP 200; all signatures verify with the published pinned key;
an isolated Arch client runs `pacman -Syu`, `pacman -S daedalus-mainnet` and
`pacman -Fy` with required signatures and locally trusted IOHK key.

## Plan

Use the existing Rust release CLI and Nix shells. Replace hand-built Arch metadata
with pacman's `repo-add`, which generates the package and file databases together
and embeds adjacent package signatures. Stage immutable package copies so the
bytes verified are the bytes uploaded. Use GPG to dearmor existing `.asc`, create
binary signatures and verify exact staged files. Verify in a separate keyring and
enforce both IOHK fingerprints for publication; local development can use its
explicitly selected test key. Do not change APT/YUM signing.

Add `pacman` to release/ops shells. Share staging between disk and object-store
publication, dereferencing aliases on output. Add regression coverage for package
tampering, identity mismatch, binary signature reuse, armor conversion, database
and file database aliases, file lists and pacman synchronization.

The S3 API cannot atomically replace a database and its separate signature. A
strict zero-mismatch cutover therefore needs deployment-side atomic generation
routing or a maintenance window. `no-store` prevents caching new metadata but
does not purge existing CDN objects or make separate writes atomic. Verify live
cache policy and report this limitation explicitly; do not claim atomicity from
upload ordering. Retain old packages for clients with older databases.

## Initial deployment evidence

2026-10-20: `daedalus.db` and `daedalus-release.gpg` return 200; `.db.sig`,
`.files` and `.files.sig` return 404. The sole package is
`daedalus-11.5.0-87825-mainnet-22440487d-x86_64-linux.pkg.tar.zst`;
its URL and `.asc` return 200, its `.sig` returns 404. The database already
contains `%PGPSIG%`. The published public key contains both required fingerprints.
Database headers show `Cache-Control: no-store`, `CF-Cache-Status: DYNAMIC`.

Deployment credentials exist in `ops/.envrc.local`. The user confirms the existing
signing configuration is sufficient and requests focusing on Arch output.
