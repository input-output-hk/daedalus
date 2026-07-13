# task-ux-706 · CAT-G — Windows storage-picker separator + node-stop→download race

Tier 2 (medium). Two independent code fixes; the second is investigate-then-guard and may spill to a
post-merge follow-up if the correct fix isn't bounded. Do **not** rearchitect the coordinator (704
deferred that).

## Step G1 — Windows storage-picker mixed separator (Extra #5)

**Root cause (display-only):** the renderer's `path` is webpack-mapped to `path-browserify` (POSIX) via
`resolve.fallback` (`source/renderer/webpack.config.js:115` — plan-review nit: a fallback, not an
alias; functionally identical here). `getManagedChainDisplayPath` builds the preview via
`path.join(customChainPath, MANAGED_CHAIN_DIRECTORY)` at `chainStorageUtils.ts:106`
(`MANAGED_CHAIN_DIRECTORY = 'chain'`, `:7`). On Windows the parent from the native dir-picker keeps `\`,
but `path-browserify` appends the `chain` subdir with `/` → `D:\Cardano\wallet/chain` (mixed). The
main-side resolver `getManagedChainPath` (`source/main/utils/chainStoragePathResolver.ts:19-20`) uses the
real Node `path` and is correct — **do not touch it**; the actual on-disk path is fine, only the
displayed preview is wrong.

**Where it shows:** `ChainStorageLocationPicker.tsx:197-199` (`displayedPath = getManagedChainDisplayPath
(effectiveSelection.path, defaultChainPath)`) → rendered at `:396` (`value={displayedPath}`). Second
caller (plan-review): `MithrilDecisionView.tsx:52` also calls `getManagedChainDisplayPath`
(display-only; benefits from the fix for free).

**Fix:** make the display join separator-aware in `getManagedChainDisplayPath` /
`chainStorageUtils.ts:106` — detect the parent's separator and use it for the appended subdir, e.g. use
`\` when `customChainPath` contains a backslash (or no forward slash), else `/`; strip any trailing
separator first. Keep it display-only and pure; the full function (`chainStorageUtils.ts:98-110`) has a
default-path fallback branch the fix must **not** touch. POSIX input (`/mnt/current-chain` →
`/mnt/current-chain/chain`) must be unchanged so `ChainStorageLocationPicker.spec.tsx:76` still passes;
**add** a win32 case (`D:\Cardano\wallet` → `D:\Cardano\wallet\chain`). Blast radius (plan-review): also
run `MithrilBootstrap.spec.tsx` — `:126,170` assert `'/mnt/current-chain/chain'` through the second
caller.

## Step G2 — Node-stop → Mithril-download race (code review, medium)

> **Scope locked 2026-07-09: time-boxed reproduce-first, minimal guard, defer the deep fix.** Attempt a
> quick reproduction (Work item 1). If it reproduces, land the **minimal defensive live-node-state
> re-query** before the download spawns (Work item 2, first half) and move any deeper immutable-file /
> ImmutableDB-lock-release settle to a **tracked post-merge follow-up** rather than doing it pre-merge.
> If it does **not** reproduce within the time box, downgrade the whole of G2 to a documented follow-up
> with the recon notes. G1 ships pre-merge regardless. This is the doc's spill clause promoted to the
> decision — do not attempt the full lock-release fix pre-merge.

**Symptom (code review + manual QA):** clicking the Mithril button while the Cardano node is still stopping/cleaning up
lets the snapshot download begin before node cleanup finishes — a possible immutable-file conflict.

**Trace (corrected 2026-07-10, plan-review):** `startPartialSync` → `MithrilController.startPartialSync()`
(`MithrilController.ts:396-403`) → `chainStorageCoordinator.startPartialSync(...)`; **inside** the
mutation lock, `_ensureNodeStoppedForPartialSyncAction` (`chainStorageCoordinator.ts:451-471`) runs; the
lock closure then returns at `chainStorageCoordinator.ts:260`, and `handlers.start()` — which spawns the
`cardano-db download … --allow-override` child (`MithrilPartialSyncService.ts:623-633`) — runs at
`:264` in a separate **post-lock** IIFE. The download therefore spawns **outside** the mutation lock
(the original trace had it inside). Two soft spots:
- `chainStorageCoordinator.ts:456-458` returns immediately when the **click-time** `nodeState` snapshot is
  `null`/`STOPPED` — no re-query, no wait — so a node that just flipped to STOPPED (handles still
  releasing) is treated as safe.
- `cardanoNode.stop()` / `_isDead()` (`CardanoNode.ts:494-499, 935-936`) confirm only IPC-disconnect +
  process-exit, **not** immutable-file/ledger handle release. No post-exit settle/verification precedes the
  download.

**Work:**
1. **Reproduce first.** Start a node, trigger stop, immediately start Mithril Sync; confirm the download
   child spawns before the immutable dir is released (watch for the conflict). If it cannot be reproduced
   reliably, downgrade to a documented follow-up rather than adding speculative synchronization.
2. **If reproduced, add the smallest correct guard.** Placement corrected (plan-review): since
   `handlers.start()` runs *after* the lock releases, "guard before `handlers.start()` … inside the
   existing mutation lock" was self-contradictory — put the guard **inside the lock closure, in/after
   `_ensureNodeStoppedForPartialSync`**. The unguarded path is the `:456-458` early-return on a
   click-time `null`/`STOPPED` snapshot (a RUNNING/STOPPING click *is* serialized via
   `await nodeStopHandler()` at `:469`, though only to process-exit). The "minimal defensive re-query"
   needs plumbing: `PartialSyncDependencies` (`:35-39`) has no node-state getter — add a `getNodeState`
   callback (live getter exists: `MithrilController.ts:170-172`). **Know its limit:** a re-queried
   state can already read `STOPPED` while file handles are still releasing (`CardanoNode._isDead`,
   `:935-936`, checks IPC-disconnect + process-exit only) — the re-query narrows the race window, it
   does **not** close it. It is a placeholder pending the deep lock-release fix; record that in the
   follow-up so triage doesn't mistake it for the real guard. A bounded settle verifying the immutable
   directory / ImmutableDB lock is actually released is **follow-up-only material** — the locked scope
   defers the deep lock-release fix; do not attempt it pre-merge. Do not restructure the coordinator or
   the start handler.
3. Add/extend a coordinator spec covering the actually-unguarded path: a **stale click-time
   `null`/`STOPPED` snapshot** while the node's handles are still releasing → the guard re-queries the
   live state and the download waits. (A STOPPING-state click is already serialized via
   `await nodeStopHandler()` at `:469` — a spec anchored only on "still stopping" would exercise
   existing behavior and miss the guard.) Feasible in the existing coordinator spec —
   `chainStorageCoordinator.spec.ts` already covers `startPartialSync` with
   `nodeState: 'running'/'stopping'/'stopped'` (`:429,451`).

**Spill clause:** if the correct guard proves non-trivial (needs a new lock-release signal from
`CardanoNode`), land the reproduction notes + a minimal defensive re-query and move the deeper fix to a
tracked follow-up — log it, don't silently drop (Tier 2, medium priority).

## Acceptance

- G1: storage-picker preview uses a single, platform-correct separator on Windows; POSIX unchanged; spec
  covers both.
- G2: either the race is guarded with a spec proving download waits for immutable-file release, or a
  documented follow-up with reproduction notes + a minimal defensive re-query is filed — with the
  re-query explicitly recorded as a window-narrowing placeholder, not the fix.
- `yarn compile` + lint + scoped jest clean; no change to the correct main-side path resolver.
