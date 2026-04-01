# Mithril & Node Sync Issues — Windows & macOS Installer Research

**Date:** 2026-04-01  
**Branch:** `feature/mithril-ux-messages-update`  
**Affected platforms:** Windows (confirmed), macOS (code-level confirmed), Linux (not affected)

---

## Executive Summary

Testing the Daedalus Windows installer revealed two independent root causes preventing Mithril snapshot bootstrap and cardano-node sync:

1. **`mithril-client` binary missing from platform installers** — The binary is built by Nix and staged, but never included in the Windows NSIS installer or the macOS `.app` bundle. It simply doesn't exist on the user's machine after installation, causing `ENOENT` when Daedalus tries to spawn it.

2. **`peer-snapshot.json` format mismatch** — The preprod peer-snapshot file from the upstream `cardano-playground` flake input originally contained relay entries using `"domain"` keys where cardano-node 10.6.2 expects `"address"`, causing all big ledger peers to fail to load. This affected all platforms until the input was moved to `node-10.6.2-config`.

---

## Issue 1: `mithril-client` Binary Not Bundled

### Symptoms
- Mithril snapshot UI shows no data (no estimated size, no snapshot selector)
- `mithril-bootstrap.log` is empty or minimal
- The underlying error is `ENOENT` — the binary doesn't exist on disk

### Root Cause Analysis

The Nix build pipeline has multiple stages, and `mithril-client` was missing from late-stage packaging on both Windows and macOS:

#### Build Pipeline Flow
```
flake.nix (mithril input)
  → common.nix (mithril-client package reference)
    → cardano-bridge.nix (copies binary to bridge/bin/)
      → Platform-specific packaging (MISSING mithril-client)
```

#### Windows Pipeline
| Stage | File | Status |
|-------|------|--------|
| Nix package | `common.nix:96` | ✅ `mithril-client-cli` resolved |
| Bridge copy | `cardano-bridge.nix:27` | ✅ `copy_glob "mithril-client"` |
| Staging | `x86_64-windows.nix:582` | ✅ `cp -vr ${bridge}/bin/* .` copies all |
| NSIS file list | `WindowsInstaller.hs:224-230` | ❌ **MISSING** `file [] "mithril-client.exe"` |
| Nix binary list | `launcher-config.nix:336-342` | ❌ **MISSING** from `installerWinBinaries` |

#### macOS Pipeline
| Stage | File | Status |
|-------|------|--------|
| Nix package | `common.nix:96` | ✅ `mithril-client-cli` resolved |
| Bridge copy | `cardano-bridge.nix:27` | ✅ `copy_glob "mithril-client"` |
| Bundle definition | `any-darwin.nix` | ❌ **MISSING** `bundle-mithril-client` |
| App copy | `any-darwin.nix` (package build) | ❌ **MISSING** copy into `Contents/MacOS/` |
| aarch64 codesign | `cardano-bridge.nix` (bottom) | ❌ **MISSING** from ad-hoc signing loop |

#### Linux Pipeline
| Stage | File | Status |
|-------|------|--------|
| Bridge copy | `cardano-bridge.nix:27` | ✅ |
| Bundle link | `x86_64-linux.nix` (`newPackage`) | ✅ Uses `daedalus-bridge/bin/*` wildcard |

Linux bundles the entire `daedalus-bridge` directory and symlinks all `bin/*` entries, so `mithril-client` is automatically included.

### Additional Issue: Binary Path Resolution

Even if the binary were present, `mithrilCommandRunner.ts` spawned `'mithril-client'` by bare name, relying on `PATH` lookup. While `source/main/index.ts` prepends `DAEDALUS_INSTALL_DIRECTORY` to `PATH`, other Cardano binaries use absolute paths via the `mkBinPath` helper in `launcher-config.nix`. Using absolute paths is more reliable and provides better diagnostic information on failure.

### Fixes Applied

**5 files changed, 24 insertions(+), 2 deletions(-):**

1. **`installers/common/WindowsInstaller.hs`** — Added `file [] "mithril-client.exe"` to NSIS file list
2. **`nix/internal/launcher-config.nix`** — Added `"mithril-client.exe"` to `installerWinBinaries`
3. **`nix/internal/any-darwin.nix`** — Added `bundle-mithril-client` definition and copy into `.app` bundle
4. **`nix/internal/cardano-bridge.nix`** — Added `mithril-client` to aarch64-darwin ad-hoc codesigning
5. **`source/main/mithril/mithrilCommandRunner.ts`** — Absolute path resolution using `DAEDALUS_INSTALL_DIRECTORY` + diagnostic logging (binary path, install dir, cwd, PATH)

### Follow-up Test Coverage

The PR review follow-up added focused unit coverage in `source/main/mithril/mithrilCommandRunner.spec.ts` to lock the runtime resolution behavior down across the packaging-sensitive branches:

- install directory unset on non-Windows spawns `mithril-client`
- install directory set on non-Windows spawns the absolute installed path
- install directory unset on Windows spawns `mithril-client.exe`
- install directory set on Windows spawns the installed `.exe` path

### Build Validation Follow-up (Windows)

Running `nix build -L .#installer-preprod-x86_64-windows` first surfaced a packaging mismatch after the NSIS inclusion fix:

- NSIS expected `mithril-client.exe` (from `WindowsInstaller.hs`)
- Bridge staging produced `mithril-client` (no `.exe` suffix) from the mithril package output
- Build failed with: `File: "mithril-client.exe" -> no files found.`

Additional fix applied:

6. **`nix/internal/cardano-bridge.nix`** — On `x86_64-windows`, if `mithril-client` exists and `mithril-client.exe` does not, copy to `mithril-client.exe` so NSIS and runtime naming stay consistent.

Verification result:

- The same target now builds successfully after the fix.
- `mithril-client.exe` is staged, signed, and included in the final preprod installer.
- The build output completes with a final installer artifact in the Nix store.

---

## Issue 2: `peer-snapshot.json` Format Mismatch (Preprod)

### Symptoms
- cardano-node shows 0% sync progress for all metrics
- Node eventually transitions to main app page while still at 0%
- Log error: `readPeerSnapshotFile: Error in $.relays[0]: key "address" not found`

### Root Cause

The preprod `peer-snapshot.json` sourced from `cardano-playground` (branch `node-10.6.2-pre-config`, rev `c9f15e5b`) contains relay entries with mixed key formats:

```json
{
  "relays": [
    { "domain": "relay.preprod.staging.wingriders.com", "port": 3001 },
    { "address": "65.109.179.26", "port": 30000 }
  ]
}
```

Cardano-node 10.6.2 parses `$.relays[0]` and requires the `"address"` key. The first entry uses `"domain"` instead, triggering a parse failure. This causes:

- All **big ledger peers** fail to load (peer-snapshot is their source)
- Preprod uses **GenesisMode** which relies heavily on big ledger peers for initial sync
- Node falls back to empty bootstrap peers and cannot make sync progress

### Why Mainnet Works (Eventually)

Mainnet uses **PraosMode** with `UseBootstrapPeers` enabled, providing 54 DNS-resolved bootstrap peers. While mainnet's `peer-snapshot.json` may have similar issues, the node can still sync via bootstrap peers. The log showed successful IPv4 connections to multiple peers.

### Affected Platforms
All platforms — `peer-snapshot.json` is sourced from the same `cardano-playground` flake input via `launcher-config.nix` and copied into the config bundle for every platform.

### Fixes Applied

This was an upstream configuration issue rather than a Daedalus runtime bug. The `cardano-playground` flake input was moved from `node-10.6.2-pre-config` to `node-10.6.2-config` and the lock was refreshed, pulling in the corrected `peer-snapshot.json` with consistent `"address"` keys.

**Why this tag:**
- Stays on the `10.6.2` line instead of pulling in later `10.7.0` pre-release changes
- Fixes the specific `peer-snapshot.json` schema problem seen in preprod
- Sampled `preprod` and `mainnet` `config.json` files remain aligned with Daedalus expectations
- `MinNodeVersion` moves from `10.4.0` to `10.6.2`, matching the node version in this repo

**Why not current `main`:**
- Updating from `c9f15e5b...` to current `main` spans 66 commits and 393 changed files upstream
- That range includes `10.7.0` pre-release changes — much broader risk surface than needed

### Flake Input Reference
```nix
# flake.nix
cardano-playground.url = "github:input-output-hk/cardano-playground/node-10.6.2-config";
# Pinned to rev 488c21bbeb1426fcf0b41a1c1a832308ac284080
```

### Lock Update Investigation

`nix flake lock --update-input cardano-playground` was originally a no-op because `node-10.6.2-pre-config` resolved as an immutable git tag, not a branch:

- `git ls-remote --refs` returned `refs/tags/node-10.6.2-pre-config → c9f15e5b...`
- Since tags are immutable, lock updates are a no-op while the input ref stays on that tag
- Running with `--refresh` still produced no lock diff, confirming this behavior

After switching the input ref to `node-10.6.2-config`, the lock moved to `488c21bbeb1426fcf0b41a1c1a832308ac284080`.

### Files Daedalus Consumes from `cardano-playground`

Loaded from `cardano-playground/docs/environments/<env>/` via `launcher-config.nix`:

- `config.json`
- `topology.json`
- `peer-snapshot.json`
- `checkpoints.json` (when present)
- Genesis files referenced by `config.json`

---

## Issue 3: Node Sync 0% Behavior

### Symptoms
- Syncing status page shows 0% for all values
- App transitions to main page while blockchain download spinner shows 0%
- Manual restart of cardano-node via diagnostics once resolved it, but couldn't reproduce

### Analysis

This was a downstream symptom of Issue 2 (peer-snapshot parse failure) for preprod. With the config pin moved to `node-10.6.2-config`, the root config issue is resolved. For mainnet, eventual sync should work once bootstrap peers connect, but initial connection can still be slow. The UI timeout/transition behavior when node isn't syncing may need separate investigation if it persists after the config fix.

---

## Testing Recommendations

After applying these fixes, verify on each platform:

### Windows
1. Confirm `mithril-client.exe` exists in the install directory after installation
2. Check `mithril-bootstrap.log` for the new diagnostic log line showing resolved binary path
3. Verify Mithril snapshot UI loads snapshot information

### macOS
1. Confirm `mithril-client` and its `mithril-client-lib/` dylib directory exist in `Contents/MacOS/`
2. On Apple Silicon, verify the binary is ad-hoc signed (no crash on launch)
3. Verify Mithril snapshot UI works end-to-end

### Linux
1. Verify Mithril works (should already work — binary was included via bridge wildcard)
2. Confirm diagnostic logging appears in logs

### Current Build Result

- `nix build -L .#installer-preprod-x86_64-windows` completes successfully after the `mithril-client.exe` packaging fix.
- The final installer includes the staged `mithril-client.exe` file and no longer fails in NSIS packaging.

### All Platforms
1. Verify preprod cardano-node sync progresses past 0% with updated `peer-snapshot.json`
2. Check node logs for successful big ledger peer connections

---

## File Reference

| File | Changes |
|------|---------|
| `installers/common/WindowsInstaller.hs` | Added `mithril-client.exe` to NSIS |
| `nix/internal/launcher-config.nix` | Added to `installerWinBinaries` |
| `nix/internal/any-darwin.nix` | Added bundle + copy for macOS |
| `nix/internal/cardano-bridge.nix` | Added to aarch64 codesigning |
| `source/main/mithril/mithrilCommandRunner.ts` | Absolute path + logging |
