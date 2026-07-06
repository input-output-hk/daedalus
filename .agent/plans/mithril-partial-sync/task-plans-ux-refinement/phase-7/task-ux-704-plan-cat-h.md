# task-ux-704 CAT-H — Comment cleanup across the partial-sync UX refinement branch

## Position in the wave

CAT-H is the **last** commit of the task-ux-704 wave (master: `task-ux-704.md`, seam S4). CATs
A–G land first and delete or move code that this inventory annotates, so the line anchors below
describe pre-wave code. Entries are re-located by quoted text (already this doc's rule — now
**mandatory**, not a fallback; see Verification gates). An entry whose annotated code was deleted
by CATs A–G is **MOOT**: skip it without counting it against the totals or the density gate.

Bare-number entries — most of Part C's DELETE lists — carry no quoted text to re-locate by. For
those the executor must (a) re-read the file at and around the stated line **after** CATs A–G
have landed, (b) confirm the target is in fact a comment and classify it against the rubric
before deleting (the when-in-doubt rule under "Inventory" applies; in test files the default
disposition is DELETE, and the comment-only rule bounds a mis-anchor's damage to a comment), and
(c) explicitly re-anchor every bare-number entry in the three spec files CATs A/B rework —
`MithrilPartialSyncService.spec.ts`, `mithrilPartialSyncChannel.spec.ts`,
`mithrilPartialSyncNodeStartup.spec.ts` — where line drift is largest; never trust their stated
lines.

### Mooted by earlier CATs (known at planning time)

Verified against the code at planning time, the master doc's traceability table, and the per-CAT
plan docs once authored:

- `MithrilPartialSyncService.ts:1374-1377` [REWRITE] — **MOOT.** CAT-A (finding A4a) deletes the
  write-only `_logStream` field and its `onLogStream` registration, taking this comment with
  them. Consolidated hazard #2 (`_logStream` write-only tripwire) is moot in full.
- `MithrilPartialSyncService.ts:769-776` [REWRITE] — **still applies**, minus its `_logStream`
  clause. The proposed replacement text already omits `_logStream` deliberately; only this
  entry's note that the tripwire "moves to its single natural home at `:1374-1377`" is stale —
  after CAT-A there is no `_logStream` to keep a tripwire for.
- `MithrilPartialSyncService.ts:1369-1370` [DELETE] — **NOT mooted; still applies.** CAT-A does
  not remove the `trackAsCancelable` flag (the A3 runner-injection refactor is explicitly
  deferred in the master doc), so the destructure and its redundant comment survive A–G.
- `MithrilPartialSyncService.ts:157-159`, `:979-981`, `:1032-1034` and spec `:2380-2381`
  [REWRITE ×4] — **handled by CAT-C** (Step 1.4): removing the `getConfig` fork falsifies the
  fork-citing clauses, so CAT-C rewrites all four in its own commit. This inventory's
  replacement texts for `:157-159`, `:979-981` and the spec entry still cite the fork —
  applying them after CAT-C would re-falsify what it fixed; for `:1032-1034` CAT-C adopts this
  inventory's replacement text verbatim. Verify the post-CAT-C comments against the convention
  and skip; do not apply this inventory's rewrite texts on top.
- `MithrilPartialSyncService.ts:386-388` [DELETE] — **handled by CAT-B** (B-5.2): the cancel
  double-log fix rewrites this comment when it deletes the in-branch duplicate log line, so the
  quoted "UNCONDITIONAL cancel-entry line…" text will be gone. Verify the post-CAT-B comment
  against the convention and skip; do not delete it.
- Service `listSnapshots()`/`showSnapshot()` public methods (`:1066-1074`) — deleted by CAT-A
  (A4d), but **no inventory entry anchors inside them** (the nearest service entries are
  `:1055-1057` and `:1162-1163`), so nothing moots here.
- Spec blocks CAT-A edits — verified: **no inventory entries fall inside them**:
  - `mithrilPartialSyncChannel.spec.ts`: CAT-A trims the mixed dead/live test ("keeps
    compatibility exports as controller proxies", ~`:145-174`) surgically and KEEPS it — it is
    not deleted with the four shims; the retitle ('configures the partial sync runtime through
    the controller') happens later, in CAT-B (B-1.4(2)), not CAT-A. The sole inventory
    entry `:261` [KEEP] sits in the finalize-channel test, which survives — but its line number
    drifts as lines above it are deleted: re-locate by text.
  - `MithrilPartialSyncService.spec.ts`: CAT-A retargets the log-file test (~`:305-325`) from
    `service.listSnapshots()` to `service._listSnapshotsRaw()` (kept, not deleted) and deletes
    the wrapper-mapping test (~`:327-363`, incl. the `showSnapshot` cases) whole; the inventory
    has no entries between `:53-54` and `:524`, so none fall in the edited or deleted ranges.
- `MithrilPartialSyncService.spec.ts:1666-1669` [DELETE] — CAT-A's A-5 spec edit rewrites this
  comment's text (dropping the `onLogStream` clause) before CAT-H runs; re-locate by the
  post-A text or skip if gone.
- `MithrilPartialSyncService.spec.ts:1028-1029` / `:1060-1061` [DELETE] — CAT-B's B-5.2 spec
  edits delete `:1028-1029` outright with the stale in-branch assertion (**MOOT**) and trim
  only `:1060-1061`'s "(the case the in-branch line can never log)" parenthetical; the trimmed
  `:1060-1061` comment survives, so its DELETE **still applies** — re-locate by the post-B text.
- `MithrilBootstrapService.ts:200`/`:216` [DELETE/REWRITE] — **NOT mooted.** CAT-B's work near
  that file is log-line hygiene (B9), not these step-label comments; the accuracy fix at `:216`
  stands ("Mark conversion complete" following *installation* is still wrong). CAT-B renumbers
  nothing there — re-locate by text regardless.
- `mithril-partial-sync.types.ts:90-93` [REWRITE] — **handled by CAT-D** (D9): tightening
  `certifiedEpoch` to drop the stale `| null` arm also deletes the stale rollout comment
  ("OPTIONAL so the renderer type-checks before the backend produces the value…"). Verify the
  post-CAT-D comment matches the convention and skip unless it doesn't.
- `MithrilPartialSyncOverlay.stories.tsx:60-65` [REWRITE] — **handled by CAT-F** (F-1 replaces
  the block when it imports `DOWNLOAD_PROGRESS_ANCHOR_ID`). Verify the post-CAT-F comment
  matches the convention and skip; do not apply this inventory's rewrite text on top.
- `MithrilPartialSyncOverlay.stories.tsx:136-139` [REWRITE] — **handled by CAT-D**: union-typing
  the error `code` field (D6) reworks those fixture codes and their comment (four story-invented
  codes would fail tsc otherwise). Verify the post-CAT-D text against the convention and skip.
- `Diagnostics.stories.tsx:76-77`, `:105-108` [DELETE] — **MOOT.** Both blocks are deleted by
  CAT-E with the story rework (Step 1.6: (c) replaces the confirmation story wholesale, taking
  `:105-108`; (d) deletes the `:76-77` fixture comment), so the Part C row carrying both is
  fully mooted.

Totals note: the Part A/B totals and estimated-lines figures shrink by the mooted entries above;
do not re-balance the inventory tables — mooted entries simply don't count.

## Why

The `feat/mithril-partial-sync-ux-refinement` branch (diff base `48b557a02`) added **1,012 comment lines** across ~80 source/spec/story files at a density and verbosity far outside repo norms:

| Scope | Prose-comment density | 4+-line comment blocks |
|---|---|---|
| This branch (diff) | 4.2% of added non-blank lines | 23% of blocks (80/348), up to 34 lines |
| Predecessor PR #3333 (`mithril_partial_sync`) | 0.1% (2 prose comments in ~7,800 lines) | none |
| Repo baseline (untouched files) | ~2.6% prose (3.5% incl. `@ts-ignore` boilerplate) | 5.9% of blocks; norm is 1–3 lines |

The excess is not the invariants themselves — much of the content is load-bearing (cancellation windows, kill semantics, cache invalidation, fail-closed disk logic). The excess is the **wrapper**: reviewer-directed narration, review-checkpoint labels (`CP-A`…`CP-D`, 17 occurrences), ALL-CAPS emphasis, and prose restating adjacent code. This task strips the wrapper and keeps the constraint.

## Target convention (applies to this cleanup and all future comments)

1. **Default is no comment.** Add one only when the logic or constraint is not self-evident from the code and cannot be made self-evident by better naming.
2. When warranted: **1–3 lines**, plain sentence case, stating the invariant / constraint / reason — never the what, never the change history.
3. Never reference process artifacts: task IDs, review findings (`CAT-*`), checkpoint labels (`CP-*`), PR numbers, plan names.
4. Never defend the change ("this fixes…", "preserves … coverage", "so a re-repro observes…") — that is PR-description material.
5. No ALL-CAPS emphasis (`MUST NOT`, `UNCONDITIONAL`, `DECLARATION ONLY`). If a constraint needs weight, the sentence carries it.
6. In spec files the bar is higher: the `describe`/`it` name carries intent. A comment is warranted only for a non-obvious fixture/mock constraint (e.g. required timer-flush ordering).

## Classification rubric used in the inventory

- **DELETE** — nothing non-obvious remains once narration is removed (restates code, narrates test intent, defends correctness, cites process artifacts).
- **REWRITE** — a genuine invariant is buried in verbosity; replace the block with the proposed 1–3 line text.
- **KEEP** — already matches the convention (or is a pre-existing/moved comment not introduced by this branch).

## Inventory

Every comment block this branch added, classified. Line numbers are **current working-tree lines at the time of authoring** (branch `feat/mithril-partial-sync-ux-refinement`); re-locate by the quoted text if the file has drifted. Entries marked "pre-existing/moved" are comments the diff shows as added only because code moved or was re-indented — they are out of scope.

Blocks not listed under a file are KEEP by omission only if they match the convention; when in doubt during execution, classify against the rubric.

### Part A — main process (`source/main/**`)

87 blocks: 6 DELETE, 44 REWRITE, 37 KEEP · est. ~108 comment lines removed.

#### source/main/mithril/MithrilPartialSyncService.ts (51 blocks; the narration hotspot)

- `:76-82` [REWRITE] "Disk preflight. snapshot.size is the FULL certified…" — fail-closed disk invariant buried in 7 lines + ALL-CAPS. The "deliberately not the delta" clause must survive — it is what stops a future "optimization" to a delta-proportional margin.
  → `// Required space = missing bytes (snapshot − local chain, floored at 0) plus a margin sized on the full`
  `//  snapshot — deliberately not the delta, as headroom for chain growth, conversion and FS slack — floored at`
  `//  DISK_SPACE_REQUIRED so a zero/missing size still fails closed; unmeasurable local size ⇒ whole-snapshot bound.`
- `:86-88` [REWRITE] "Behind-ness threshold in IMMUTABLE FILES…" — "calibrate down during QA" is process narration.
  → `// Behind-ness threshold in immutable files; backend-owned, overridable via launcher config.`
- `:89` [REWRITE] derivation of the magic 20 worth keeping, arithmetic compressed (not dropped — the constant is 20, so restating "~20 files" alone would be circular).
  → `// ≈ 1 epoch: ~6h per immutable file ⇒ ~20 files per 5-day epoch; QA-calibratable`
- `:90` [REWRITE] **STALE — correctness fix, not just compression**: says "only the aggregator query is cached" but the local immutable read is now cached under the same TTL (`_localImmutableCache`).
  → `// 5 min TTL for the behind-ness input caches`
- `:92-98` [REWRITE] "Slot-lifecycle: service-LOCAL _runCommand options…" — slot-clobber invariant wrapped in "this flag fixes" narration.
  → `// Per-call opt-in to register a child into the cancelable _currentProcess slot. Stripped before the`
  `//  runner options, and deliberately not inferred from _activeWorkDir — the behind-ness probe runs with`
  `//  _activeWorkDir set, so inferring would clobber the active download's slot.`
- `:103-105` [REWRITE] type shape near-self-evident; keep only why the flag rides the options object.
  → `// Options threaded through the metadata-read helpers, which call _runCommand without the workDir positional.`
- `:127-130` [REWRITE] cancellation-sentinel control flow, verbose.
  → `// Thrown by _throwIfCancelled() to unwind start()'s stage machine on cancel. Caught by start()'s`
  `//  'if (this._isCancelled) return', so a cancel never emits 'failed' and settles into finalizeCancel().`
- `:157-159` [REWRITE] cache rationale, minor caps.
  → `// Caches the per-probe local immutable read (getManagedChainPath fork + immutable/ readdir) under the`
  `//  same TTL as the aggregator cache; invalidated via _invalidateBehindnessCaches().`
- `:191-192` [KEEP] "The local immutable position is about to change…"
- `:215-217` [REWRITE] drop "preserves cancel-at-preparing coverage" reviewer narration.
  → `// Tracked as cancelable so a cancel during 'preparing' can still kill an in-flight metadata child.`
- `:239` [DELETE] `); // preflight` — `_assertSufficientDiskSpace` already names it.
- `:241-242` [REWRITE] drop the `CP-A` review label, keep the cancellation-window fact.
  → `// Cancel here unwinds before the latest-drift re-resolve and the download spawn, while status is cancelling.`
- `:261` [KEEP] "The latest-drift re-resolve is also part of the cancelable run."
- `:286-287` [REWRITE] drop `CP-C` label.
  → `// Cancel here unwinds before the 'verifying' emit and the 'converting' stage.`
- `:303-307` [REWRITE] drop `CP-D` / "Boundary-A" labels; keep the cancel-never-reaches-cutover invariant.
  → `// Last cancel checkpoint before cutover: a cancel during 'converting' must never reach live-chain install.`
  `//  Defense-in-depth for the race where the conversion completes just as the kill lands.`
- `:349` [KEEP] "Invalidate behind-ness input caches at cancel entry…"
- `:357-360` [REWRITE] real UI invariant; ALL-CAPS MUST/TRUE/NOT.
  → `// No active run to cancel, but re-emit the current status so the renderer doesn't stick on its optimistic`
  `//  stopping-node frame — re-emit the real status verbatim rather than fabricating 'cancelled'.`
- `:386-388` [DELETE] "UNCONDITIONAL cancel-entry line — fires even when the slot is EMPTY…" — repro narration defending a log line.
- `:405-406` [DELETE] restates what `killProcessTree` (self-named, documented) does at the call site.
- `:432-436` [REWRITE] genuine fail-safe rationale, 5 → 2 lines.
  → `// Best-effort: a locked staging dir must not strand the user on a failed screen. No cutover has`
  `//  happened, so nothing installed is at risk; the next start's staging prep reclaims the orphan.`
- `:460` [KEEP] "Group/tree SIGKILL (killProcessTree no-ops on an empty slot)."
- `:469-476` [REWRITE] load-bearing (detached child survives process.exit; sync-mode reason) but 8 lines + caps.
  → `// SIGKILLs the tracked download/conversion child on quit — cancel()/forceKill() don't run on a plain`
  `//  quit, so a detached POSIX child would survive process.exit(). sync: true because safeExit calls`
  `//  process.exit() inside a stream-end callback, where an async Windows taskkill would never launch.`
- `:518-524` [REWRITE] cross-session recovery-boundary invariant, compressed.
  → `// Recovery actions may target a boundary startup emitted in a previous session; this service starts`
  `//  each session idle, so its allowed-action assertion would reject them. Adopt the controller's broadcast`
  `//  snapshot as the local boundary — only from idle, and only when it is a real recovery boundary.`
- `:553-554` [KEEP] "Marker-first staging resolution (see _cleanupPartialSyncArtifacts)…"
- `:570-572` [KEEP] "Dismiss-driven. Idempotent: safe even when already idle…"
- `:610-613` [REWRITE] drop `CP-B` label + reviewer narration.
  → `// Cancel here (before the 'downloading' re-emit and download spawn) prevents re-showing the sync`
  `//  screen and re-spawning the download after a cancel during 'preparing'.`
- `:691-692` [KEEP] "The download IS the cancelable run…" (optionally lowercase the IS).
- `:755-761` [REWRITE] conversion-child tracking, 7 → 3 lines.
  → `// Track the conversion child in the cancelable slot so cancel()/forceKill() can kill an in-flight`
  `//  conversion; the runner nulls it again via onProcess(null) on close. _runBinary is conversion-only,`
  `//  so this unconditional onProcess needs no trackAsCancelable flag.`
- `:769-776` [REWRITE] **the** slot-clobber safety invariant — must survive here. The `_logStream` write-only tripwire moves to its single natural home at `:1374-1377` (the assignment site), so it is deliberately absent from this replacement.
  → `// Only cancelable runs register here (download, the two start()-phase metadata reads, conversion).`
  `//  Untracked metadata reads get no onProcess, so they can neither overwrite nor null the slot mid-run.`
- `:780-783` [REWRITE] late-child race, trimmed.
  → `// A cancel can land after the stage is shown but before the child reaches the slot (e.g. _runCommand's`
  `//  async env prep). Without this, cancel()/forceKill() would miss it and the join would fall to`
  `//  abandonCancel() while the download keeps running.`
- `:792` [DELETE] restates the `killProcessTree` call.
- `:854-856` [KEEP] "Resolve the staging root from the durable marker first…"
- `:864-865` [KEEP] "Covers restart-normal/wipe/finalize-wipe/finalize-completed…"
- `:880-882` [REWRITE] drop the `CP-A..CP-D` enumeration.
  → `// Cancel checkpoint at each 'await' seam in start(): a no-op on the success path, throws the`
  `//  cancellation sentinel once cancel() has set _isCancelled.`
- `:903-907` [REWRITE] cutover-correctness invariant (same volume, never inside live subtree).
  → `// Colocate staging as a sibling of the managed chain so cutover is an intra-volume rename, never inside`
  `//  the live chain subtree. _stagingChainDir is the realpath captured at start(); the stateDirectoryPath`
  `//  fallback covers a session with no sync yet.`
- `:914-916` [REWRITE] third restatement of `trackAsCancelable`; keep only the probe-default fact.
  → `// trackAsCancelable is true only for the two start()-phase reads; the behind-ness probe uses the untracked default.`
- `:961-962` [REWRITE] caps NEVER.
  → `// Untracked: the behind-ness probe runs concurrently with an active download and must not occupy the cancelable slot.`
- `:966` [KEEP] "Carry the certified epoch from the same beacon".
- `:972-974` [REWRITE] keep the read-ordering constraint, drop caps.
  → `// Certified epoch carried on the aggregator cache; null when unpopulated or the beacon had no epoch.`
  `//  Read after _getCachedLatestCertifiedImmutableNumber has populated the cache.`
- `:979-981` [REWRITE] overlaps the `:157-159` field comment; keep only what a hit skips.
  → `// On a cache hit, skip getManagedChainPath (which forks checkDiskSpace) and the immutable/ readdir.`
- `:1004-1006` [REWRITE] drop the call-site enumeration (find-usages covers it).
  → `// Drop both behind-ness input caches so the next probe re-resolves fresh inputs.`
- `:1012-1013` [KEEP] "A chain-directory change swaps the chain store under the probe…"
- `:1032-1034` [REWRITE] the `Promise.all` makes concurrency obvious.
  → `// The aggregator query and local read are independent, so run them concurrently.`
- `:1039` [KEEP] "Same beacon → consistent epoch; null/undefined ⇒ omitted below."
- `:1043` [KEEP] "local >= latest ⇒ no certified range to restore ⇒ never nudge"
- `:1055-1057` [KEEP] "Aggregator unreachable, no immutable dir yet, or any failure ⇒ degrade…"
- `:1162-1163` [KEEP] "Local size unmeasurable — require the conservative whole-snapshot bound…"
- `:1178` [REWRITE] keep the fail-open decision; drop exact line numbers (stale-prone) and caps.
  → `// Could not measure free space — fail open rather than false-block (repo precedent: handleDiskSpace.ts).`
- `:1369-1370` [DELETE] the destructure `const { trackAsCancelable, ...runnerOptions }` literally shows the strip.
- `:1374-1377` [REWRITE] `_logStream` write-only invariant, duplicates `:769-776`; compress.
  → `// Always registered — every run writes to the shared log file. Overwriting the shared _logStream`
  `//  mid-run is safe only while it stays write-only.`
- `:1382-1386` [REWRITE] the clobber fix itself — must survive here.
  → `// Register onProcess only for cancelable runs. Omitting it for untracked reads keeps them from`
  `//  overwriting the slot on spawn or nulling it on close — the clobber that left a cancelled download unkillable.`

#### source/main/mithril/MithrilController.ts (5 blocks)

- `:169-177` [REWRITE] 9-line concurrency guard essay. If trimmed further during execution, the sentence to keep is the concurrent-spawn one (the degrade-is-safe clause is re-derivable from `getPartialSyncBehindness`).
  → `// Skip the behind-ness probe while a sync is working or terminal-'cancelled': it would spawn a`
  `//  concurrent mithril-client metadata child mid-run. Degrading to not-behind is safe — the prompt is`
  `//  session-suppressed once a sync starts, and the probe degrades to not-behind on any failure anyway.`
- `:212-218` [REWRITE] overlaps the service's shutdown-reap doc; caps ONCE.
  → `// Best-effort shutdown reap, called once from safeExit(): cancel()/forceKill() don't run on a plain`
  `//  quit, so this is all that stops a quit-mid-download orphaning a detached mithril-client. The`
  `//  isPartialSyncActive() guard is broader than needed but harmless. Fully try/caught so it can't block shutdown.`
- `:414-415`, `:470-471`, `:488-490` [KEEP] — plain 2–3 line invariants.

#### source/main/mithril/killProcessTree.ts (1 block)

- `:8-28` [REWRITE] 21-line JSDoc → ~9 lines; public helper warrants a doc, compressed. Both directions of the sync/async constraint must survive: why async is the default (a sync taskkill can freeze the main thread at interactive kill sites) and why the shutdown path must be sync (it is `safeExitWithCode` — not the reap — that reaches `process.exit()`).
  → `/**`
  ` * Kill a runner-spawned mithril child and its descendants.`
  ` * POSIX: process.kill(-pid) group-kills the tree — relies on the runner's detached spawn making the child a group leader.`
  ` * Windows: taskkill /t /f — async by default so interactive kill sites never block the main thread;`
  ` *   sync when opts.sync, because the shutdown reap runs as safeExitWithCode reaches process.exit() inside`
  ` *   a stream-end callback, where an async taskkill would never launch.`
  ` * signal is a no-op on Windows (taskkill is always a hard kill). Null/missing pid no-ops; a failed group kill`
  ` *   falls back to child.kill() and never throws.`
  ` */`

#### source/main/mithril/mithrilPartialSyncNodeStartup.ts (6 blocks)

- `:65-67` [REWRITE] drop the "Boundary C2" design label.
  → `// This marker state means a prior run already proved one successful node start on the installed DB.`
  `//  Reclaim leftover staging (close-without-dismiss or crash) and resume a normal boot.`
- `:72-74`, `:118-119`, `:130-131` [KEEP] — plain best-effort/quit rationale.
- `:89-92` [REWRITE] drop "was removed here"/"two-competing-surfaces" history.
  → `// This dialog is the single startup-interrupted recovery surface: startup-owned recovery must work`
  `//  before the renderer is ready and can't depend on diagnostics UI or a running node. In-session failures`
  `//  still use the React overlay (startInstalledNode catch).`
- `:197-199` [REWRITE] drop "Boundary C2" + caps DEFERRED.
  → `// Stamp node-start-verified and emit completed; the marker clear is deferred to the dismiss-driven`
  `//  finalize (finalizeCompletedPartialSync). Carry stagingRootPath forward so finalize can remove the exact dir.`

#### source/main/mithril/mithrilSnapshotMetadata.ts (3 blocks)

- `:6-8` [REWRITE] drop jargon ("horizon-free, early-resolving").
  → `// Optional certified-beacon epoch; null when the beacon carries no epoch. The immutable number is`
  `//  the sole resolution signal, so this is never gated on.`
- `:94-98` [REWRITE] compress the schema citation.
  → `// Extract the certified-beacon epoch, mirroring extractLatestCertifiedImmutableNumber. 'beacon.epoch'`
  `//  is the upstream key (CardanoDbBeacon, snake_case); 'cardano_db_beacon.epoch' matches the listing shape.`
- `:169` [KEEP] "Best-effort epoch; a present immutable number with a null epoch must still resolve."

#### source/main/mithril/mithrilCommandRunner.ts (2 blocks)

- `:134-136` [KEEP] shared-pipeline function-purpose comment.
- `:169-172` [REWRITE] load-bearing (detached spawn enables group-kill), 4 → 3 lines.
  → `// Detach on POSIX so the child leads its own process group and killProcessTree's process.kill(-pid)`
  `//  reaps the whole tree. Off on Windows (documented launcher breakage — see CardanoSelfnodeLauncher.ts);`
  `//  stdio stays piped and the child is not unref()'d.`

#### source/main/mithril/MithrilBootstrapService.ts (3 blocks, edits of pre-existing style)

- `:185` [KEEP] "Convert snapshot from in-memory format to LSM…" — matches the file's pre-existing style.
- `:200` [DELETE] "Mark conversion complete; begin installation" — restates adjacent calls.
- `:216` [REWRITE] duplicate of `:200`'s text but follows *installation*, so it is inaccurate as written; correcting it (rather than deleting both) keeps the file's pre-existing step-label style consistent.
  → `// Mark installation complete; begin cleanup`

#### Remaining main-process files

- `mithrilSnapshotConverter.ts:62` [KEEP]; `chainStorageValidation.ts:121`, `:153-155` [KEEP]; `chainStorageManagerShared.ts:291-292` [KEEP]; `safeExitWithCode.ts:12-13` [KEEP] (Electron 41 SIGABRT rationale); `windows/main.ts:159`, `:171` [KEEP]; `environment.ts:73-74` [KEEP]; `mainErrorHandler.ts:27` [KEEP]; `deviceDetection.ts:30-31` [KEEP]; `deviceTracker.ts:30` [KEEP]; `main/webpack.config.js:33` [KEEP].
- `CardanoNode.ts:808-813` [REWRITE] double-STOPPED → SIGABRT rationale, 6 → 3 lines:
  → `// If stop() is driving a controlled shutdown, let it own the STOPPED sequence and return. Otherwise`
  `//  both stop() and _handleCardanoNodeExit call _changeToState(STOPPED), and the second broadcast targets`
  `//  a renderer being destroyed → Chromium SIGABRT on Electron 41.`
  `CardanoNode.ts:856` [KEEP].
- `main/index.ts:91-94` [REWRITE]:
  → `// Reap any live Mithril partial-sync child before the exit branches: safeExit only stops cardanoNode,`
  `//  so a quit mid-download would otherwise orphan the detached mithril-client past process.exit().`
  `//  Best-effort and fully try/caught, so it can't block exit.`
- `mithrilBootstrapChannel.ts:97-98` [REWRITE] **STALE — names a method that no longer exists** (`sendStatusUpdate`; it is `setBootstrapStatusSender`):
  → `// Rebind the status sender to the latest window each call, so status targets the current webContents`
  `//  after window recreation.`

### Part B — renderer + common (`source/renderer/**`, `source/common/**`)

109 blocks: 4 DELETE, 50 REWRITE, 55 KEEP · est. ~220 comment lines removed.

#### source/renderer/app/stores/MithrilPartialSyncStore.ts (25 blocks)

- `:33-36` [REWRITE] timeout-unpins-guard invariant, verbose.
  → `// Bounds each availability request so a wedged main process cannot pin _isRefreshingAvailability true forever and strand future refreshes.`
- `:38-39` [REWRITE] ALL-CAPS SLOWS/STOPS.
  → `// Slower poll cadence once availability is settled-stable; the poll slows to this but never stops.`
- `:41-43` [REWRITE] anti-premature-backoff invariant.
  → `// Consecutive stable reads required before backing off, so a premature not-behind read cannot slow the poll before it self-corrects.`
- `:59-66` [REWRITE] 8-line re-pop-guard essay → 2 lines.
  → `// Session-scoped re-pop guard: set once an attempt begins so the proactive prompt never re-offers regardless of outcome;`
  `//  reset only when a start rejection resyncs to idle (the attempt never took hold).`
- `:70-71` [KEEP] not-behind vs unknown distinction.
- `:73-77` [REWRITE] drop "DECLARATION ONLY" + `_applyAvailability` narration.
  → `// Mithril certified-beacon epoch: early-resolving fallback anchor for the late networkTip.epoch.`
  `//  Undefined until the backend supplies it; then computeBehindByEpochs degrades to networkTip-only.`
- `:82-85` [REWRITE] the counting semantics restate the field name, but "not observable" explains why this field alone is un-decorated amid `@observable` siblings.
  → `// Plain poll-state; deliberately not @observable (nothing reactive reads it).`
- `:96-110` [REWRITE] 15-line design essay duplicating the constants' comments.
  → `// Poll starts fast and self-rearms between fast and back-off inside _refreshAvailability; the interval never stops.`
  `//  Ticks while active/cancelled return early there, but idle/failed/completed keep probing so Diagnostics self-corrects on first load.`
- `:114-116` [REWRITE] keep the no-two-intervals invariant.
  → `// (Re)arms the availability poll, clearing any existing interval first so a fast/back-off cadence switch never leaves two intervals running.`
- `:194-198` [REWRITE] elapsed-anchor lifecycle.
  → `// Renderer-side elapsed anchor: re-anchored on the first frame of a fresh working run (honoring backend`
  `//  elapsedSeconds when re-attaching to an in-flight op), and released only at idle so terminal overlays keep a frozen elapsed value.`
- `:230-232` [REWRITE] definition ≈ the one-line body.
  → `// Availability is stable when partial sync is disabled or the node is caught up.`
- `:243-248` [REWRITE] drop "Behavior change from…" history; keep the no-concurrent-child reason.
  → `// Skip the probe while work is active or terminal-cancelled: it would spawn a concurrent mithril-client`
  `//  metadata child (fork/readdir cost) for a figure only Diagnostics could show mid-run.`
- `:254-257` [REWRITE] guard-pinning rationale already at the constant.
  → `// Timeout is always cleared in finally so a settled request leaves no dangling timer.`
- `:271-277` [REWRITE] narrates the adjacent branch; keep only the re-arm-fast note.
  → `// Re-arm the fast cadence on the first unstable read after a back-off so a node that falls behind again is re-detected promptly.`
- `:312-314` [DELETE] duplicates the `certifiedEpoch` field comment (`:73-77`).
- `:323-328` [REWRITE] mostly change-history.
  → `// Await finalize before flipping the dismiss flag; resync on any outcome.`
- `:331-339` [REWRITE] two load-bearing invariants in 9 ALL-CAPS lines — **the runInAction/strict-mode constraint must survive**.
  → `// Flip only on finalize success so the success frame never hides before the backend finalizes. Post-await`
  `//  under strict mode this must run in its own action context or the mutation would throw.`
- `:348-355` [REWRITE] keep no-blind-flip; deliberately drop the speculative backend-ordering/.lock detail (out-of-scope backend behavior).
  → `// On failure do not flip; the resync below reflects the true backend state, so the overlay persists if the backend is still 'completed'.`
- `:366-367` [KEEP] retry == start path, no dedicated channel.
- `:371-372` [DELETE] restates the assignment; rationale lives at the field (`:59-66`).
- `:396-397`, `:429-430`, `:435-436`, `:446-447` [KEEP] — plain catch/resync contracts.
- `:409-412` [REWRITE]
  → `// Start rejected back to idle: the attempt never took hold, so re-arm the prompt guard. Post-await,`
  `//  this needs its own action context under strict mode.`

#### source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx (14 blocks)

- `:43-46` [REWRITE] → `// Linger the success frame long enough to read before the finalize IPC fires automatically.`
- `:48-50` [REWRITE] → `// Delay before the single silent retry of a rejected finalize: long enough for a transient hiccup to clear, short enough not to linger.`
- `:72-75` [REWRITE] → `// Switches to the error view after the finalize attempt and its retry both fail; component-local because 'completed' otherwise renders through the progress view.`
- `:77-79` [REWRITE] → `// Guards state updates so a late finalize outcome never sets state after the overlay has unmounted.`
- `:88-92` [REWRITE] → `// On 'completed', auto-fire the finalize IPC via onDismissCompleted once the success frame has lingered.`
- `:97-101` [REWRITE] → `// Retry a rejected finalize once, then surface the error view — never swallow it, so a failure neither strands the success frame nor becomes an unhandled rejection.`
- `:120-123` [REWRITE] → `// Manual finalize retry from the error view; renderer-local, not a backend recovery action.`
- `:128-129` [KEEP]; `:134` [DELETE] restates `setFinalizeFailed(true)`.
- `:141-143` [REWRITE] → `// 'completed' is a progress status, so this flag forces the error view; gating on 'completed' keeps a stale flag from leaking into other statuses.`
- `:156-157` [KEEP] membership invariant.
- `:196-198` [REWRITE] → `// Quit renders only when no recovery actions exist, so a failure is never an unclosable dead-end; renderer-only, not a backend recovery action.`
- `:209-214` [REWRITE] → `// Reorders this overlay's actions to secondary-first, primary-last (a stable reorder that never adds or drops actions; membership still comes from allowedRecoveryActions).`
- `:220-221` [KEEP] deliberate bypass of recovery-action membership.

#### source/renderer/app/containers/loading/MithrilProactivePromptContainer.tsx (3 blocks)

- `:9-42` [REWRITE] the 34-line ALL-CAPS design essay → 5-line docblock; each gate is self-evident from `isGated` + its trailing comments.
  → `/**`
  ` * App-level owner of the proactive Mithril prompt; mounted from App.tsx (not the`
  ` * loading screen) so it survives the loading -> Wallet Summary route change. The`
  ` * render gate below decides when it is offered.`
  ` */`
- `:56-59` [REWRITE] → `// Broadens the networkTip-only isBehindnessKnown so a known-behind state is recognised early via the certified epoch too.`
- `:76-78` [REWRITE] → `// Computed after the gate so it runs only when the prompt renders; undefined here also hides the prompt (node at/ahead of the anchor).`

#### source/renderer/app/utils/mithrilBehindness.ts (2 blocks)

- `:3-16` [REWRITE] 14 lines → 5; **keep the epochs-only / not-a-threshold invariant** (encodes the project vocabulary rule).
  → `/**`
  ` * Availability gate, not a significance threshold: behind-ness is "known" only`
  ` * once both local and network tips have finite epochs. Backend isSignificantlyBehind`
  ` * stays the sole offer signal; this surfaces epochs only, never % or immutable counts.`
  ` */`
- `:23-43` [REWRITE] 21 lines → 5; drop old-clamp history and consumer enumeration.
  → `/**`
  ` * Display-only epochs-behind figure. Returns undefined at or above the anchor,`
  ` * so callers hide "behind" copy near the tip rather than showing a misleading 1.`
  ` * Anchor prefers networkTip.epoch when finite, else the earlier-resolving certifiedEpoch.`
  ` */`

#### Remaining renderer/common files

- `MithrilProgressView.tsx:66-70` [REWRITE] → `// Shared completion frame for the node stop/start hand-offs and the completed transition; they differ only in spinner position and whether a detail line is shown.` · `:127-130` [REWRITE] → `// Partial-sync turns 'completed' into a loading-style hand-off frame while the finalize auto-timeout runs; bootstrap keeps its own completed frame.` · `:133`, `:145` pre-existing/moved · `:167-168`, `:272-274` [KEEP] (the disabled-`<button>`-swallows-hover note guards the `<span>` wrapper — do not drop).
- `SyncingConnectingMithrilPrompt.tsx:8-10` [REWRITE] → `// Shared summary sentence, imported so the confirm view and the diagnostics modal stay identical; do not redeclare the id locally.` · `:129-132` [REWRITE] → `// The choice-view button only reveals the confirm view; handleStart is the single caller of onStart, so confirmation always precedes start.` · `:137-141` [REWRITE] → `// On success the store flips to a working status and the overlay takes over, unmounting this prompt; isStarting disables the button so a lingering mount cannot start twice.`
- `mithrilErrorMessage.ts:7-9` [KEEP] · `:25-28` [REWRITE] → `// Start rejections carry backend codes, not user copy: resolve a known code to localized copy, else the shared fallback, so the raw message never reaches the user.` · `:40-43` [REWRITE] → `// Store-side variant (no intl context): pass a real Error through, else wrap the extracted message; an empty message signals the component to use the shared fallback.`
- `mithril-partial-sync.types.ts:72-73`, `:85-87`, `:143-144` [KEEP] · `:90-93` [REWRITE] → `// Mithril certified-beacon epoch: early-resolving fallback anchor for the late networkTip.epoch. Optional; when absent the figure degrades to networkTip-only.`
- `MithrilSyncProcessSummary.messages.ts:4-12` [REWRITE] → 4-line docblock: `Canonical shutdown/restore/restart summary sentence, kept behind one i18n key and shared by the partial-sync confirmation modal and the prompt confirm view; import this module rather than redeclaring the id.`
- `partialSyncErrorCopy.ts:53`, `:71-73`, `:103-104`, `:108-110` [KEEP] — tier-precedence and never-surface-raw invariants.
- `MithrilErrorView.tsx:30-34` [REWRITE] → `// Opt-in right-alignment of the actions footer; default keeps it left-aligned in caller order.` · `:119-122` [REWRITE] → `// Actions render verbatim in caller order; any reordering is the caller's responsibility.`
- `renderer/webpack.config.js:48`, `:49-51`, `:161-165` [KEEP] — MobX 5/SWC and @trezor/transport build constraints; `:111` pre-existing.
- `urlsConfig.ts:7-9`, `:34`, `:42`, `:45` [KEEP].
- `NetworkStatusStore.ts:875-879` [REWRITE] → `// Reactive availability signal for the proactive prompt: true once both tips have finite epochs, so consumers re-render when the network tip arrives. Not a threshold; backend isSignificantlyBehind owns the offer.`
- `AppUpdateStore.ts:44-45` [KEEP]; `:190-191` [KEEP] (borderline — trimmable if aggressive).
- `renderer/app/index.tsx:25-28` [KEEP] — `enforceActions: 'observed'` rationale.
- `SyncingConnectingPage.tsx:37-40` [DELETE] — documents an absence / change history; if reviewers want a breadcrumb at the old mount site, retain a single line: `// Proactive Mithril prompt now mounts app-level in App.tsx to survive the loading -> Wallet Summary transition.`
- `MithrilStepIndicator.tsx:166-168`, `:184` [KEEP].
- `MithrilPartialSyncSection.tsx:134-136` [KEEP] — variant-precedence rationale the if/else order alone doesn't justify.
- `App.tsx:118-119` [KEEP] · `:138-141` [REWRITE] → `{/* Mounted app-level so it survives the loading -> Wallet Summary route change; self-gates to idle so it never co-renders with the overlay above. */}`
- `hardware-wallets.types.ts:9`, `:11` [KEEP].
- Pre-existing/moved only (no action): `HardwareWalletsStore.ts` (all moved), `StakingStore.ts:574`, `currencyConfig.ts:493`, `DaedalusDiagnostics.tsx:901`, `createTheme.ts:587`. Genuinely new and fine: `News.ts:87` [KEEP].

### Part C — specs, storybook, e2e

~218 blocks: ~133 DELETE, ~33 REWRITE, ~52 KEEP · est. ~300 comment lines removed (deletions ~230; rewrites compress ~120 → ~40). In test files the default is DELETE: the `describe`/`it` name carries intent.

#### source/main/mithril/MithrilPartialSyncService.spec.ts (~57 blocks; ~62 lines removed)

REWRITE (keep a real fixture/mock constraint, trimmed):
- `:53-54` → `// Mock killProcessTree so no test issues a real process-group kill against a fake pid.`
- `:537-538` → `// Root-path edge case: managedChainPath '/' keeps staging under '/', so isPathWithin fires.`
- `:1044-1046` → `// An active work dir is required here: an empty slot AND empty workDir would short-circuit on the no-active-sync early return.`
- `:1085-1088` → `// sync: true is required: safeExitWithCode calls process.exit() inside a stream-end callback, so an async taskkill issued that late would never launch.`
- `:1410-1411` → `// Flip the cancel flag in the disk-space stub — the last await before the first checkpoint (no live process yet).`
- `:2275-2276` → `// The fs-extra mock lacks lstat/realpath, so getManagedChainPath is mocked directly and the local read is driven through the immutable-directory readdir.`
- `:2380-2381` → `// getManagedChainPath (which forks checkDiskSpace via getConfig) is the proxy for the cached local read: a cache hit must skip it.`

KEEP: `:1763-1764` (no setImmediate in this jest env; zero-timeout flush), `:2427`, `:2453-2454` (deliberately no Date.now advance — isolates invalidation from TTL), `:2478-2480` (microtask-flush concurrency constraint), and the five disk-margin arithmetic anchors `:1994`, `:2019`, `:2033`, `:2054`, `:2074` (firm KEEP — they anchor magic fixture numbers to the margin/floor/fallback formulas stated nowhere else in the spec; hazard #6).

TEST RENAME (executable-code exception, explicitly authorized): `:1440` — the it() title itself contains a review label: `'a cancel during converting unwinds at CP-D before install — no cutover marker, no install'`. Rename to `'a cancel during converting unwinds at the last checkpoint before install — no cutover marker, no install'`. This is the one artifact the comment-only rule cannot reach and the reason gate 4 scans all added lines.

DELETE (assertion/fixture narration; test names carry intent): `:524`, `:839`, `:855`, `:857`, `:886`, `:1016-1017`, `:1028-1029`, `:1060-1061`, `:1070`, `:1349-1352` (describe-level narration citing CP-A..CP-D), `:1423-1425`, `:1428`, `:1434`, `:1445-1446`, `:1467-1470`, `:1515-1516`, `:1526`, `:1550-1551`, `:1562`, `:1599`, `:1629`, `:1634`, `:1647`, `:1656`, `:1666-1669`, `:1688`, `:1691-1692`, `:1757-1758`, `:1772`, `:1782`, `:1827-1828`, `:1836-1837`, `:1855`, `:1860-1861`, `:1906`, `:1939`, `:2097`, `:2126`, `:2134`, `:2252`, `:2424`.

#### source/renderer/app/stores/MithrilPartialSyncStore.spec.ts (~37 blocks; ~48 lines removed)

REWRITE:
- `:8-11` → `// The real logger writes to the electronLog global, absent under jsdom; stub it so the store's catch-branch logger.warn calls stay assertable and don't crash.`
- `:174-176` → `// dismissCompletedOverlay resyncs via syncStatus() in its finally, so back the status channel with a post-finalize idle snapshot.`
- `:239-241` → `// Keep the resync status on 'completed'; an idle resync would clear the flag via _updateStatus and mask the flip under test.`
- `:258-268` → `// Enable MobX strict mode (the renderer's enforceActions: 'observed'): the post-await flag flip runs outside the enclosing @action, so it must be wrapped in runInAction. The MobX 5 default enforces nothing and hides this regression; restored in finally so it doesn't leak.`
- `:302-305` → `// The backend runs _resetToIdleStatus() before the failing fs.remove, so a finalize failure still resyncs to idle and hides the overlay via status, not the dismiss flag (which stays false).`
- `:763-767` → `// Use an unstable (behind) read so the known-stable back-off never engages and the fast 30s cadence stays fixed for the idle-poll assertion.`
- `:779-784` → `// Await the prior probe plus a microtask flush so _refreshAvailability's finally clears the re-entrancy guard before the next tick (the bounded request resolves one microtask after its own promise).`
- `:821-824` → `// Advance past AVAILABILITY_REQUEST_TIMEOUT_MS so the bounded race rejects and the finally clears the guard; flush the rejection microtasks since the never-settling request itself can't be awaited.`
- `:933-937` → `// The gate exists because a probe mid-run would spawn a concurrent mithril-client metadata child.`
- `:1137-1141` → `// setup() calls syncStatus(); the outer beforeEach's resetAllMocks() wipes the status mock, so re-mock it here (after the reset) to return idle and keep syncStatus() off the jsdom-unavailable logger.warn path.`

KEEP: `:272` (observer read arms the strict-mode guard), `:971` (fixture isolation), `:1114-1115` (session-scoped vs resettable flag contrast). Borderline: `:1003-1004` (mild repeat of `:971` — trim one of the pair).

DELETE: `:278`, `:286-288`, `:291`, `:359-360`, `:726-727`, `:805-807`, `:817`, `:836`, `:852-853`, `:862`, `:866-867`, `:874`, `:879-880`, `:901`, `:911`, `:919`, `:923`, `:957-959`, `:1017`, `:1030`, `:1033-1034`.

#### source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx (19 blocks; ~22 lines removed)

KEEP: `:18-20` and `:28-30` (react-polymorph Link/PopOver stub rationale — identical text also at `MithrilProgressView.spec.tsx:9-11`; trim all three identically or none), `:23` (eslint directive — load-bearing), `:182` (macrotask for unhandled-rejection surfacing).
REWRITE: `:110-114` → `// Drive from the post-cutover 'failed' state, not 'cancelled': post-cutover cancel is forbidden and the pre-cutover cancelled dialogue no longer offers wipe. With canRetry/canRestartNormally false, wipe resolves to the primary variant.` · `:163-165` → `// A finalize rejection must never surface as an unhandled promise rejection.`
DELETE: `:141-142`, `:148`, `:155`, `:292`, `:313-314`, `:333`, `:343`, `:347-348`, `:359`, `:381`, `:388`, `:418`, `:432-434`.

#### storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx (9 blocks; ~24 lines removed)

REWRITE: `:60-65` → `// The active sub-item id must be step-3 (MithrilStepIndicator's DOWNLOAD_PROGRESS_ANCHOR_ID); any other id fails the anchor check and silently drops the progress bar.` · `:136-139` → `// finalizing's code is intentionally absent from COPY_BY_CODE/COPY_BY_STAGE, so this fixture exercises the generic FAILED fallthrough (the other stages map to bespoke copy).`
DELETE: `:52-53`, `:90-92`, `:290-291`, `:299-301`, `:309-310`, `:314-316`, `:353-357`.

#### source/renderer/app/containers/loading/MithrilProactivePromptContainer.spec.tsx (9 blocks; ~16 lines removed)

REWRITE:
- `:9-11` → `// Renders the real SyncingConnectingMithrilPrompt so the gating matrix is asserted against displayed copy, not an internal flag.`
- `:25-29` → `// The shared fixture keeps tips finite (network 100, local 97 → 3 epochs) so only isBehindnessKnown — not a re-derived behindByEpochs — can gate the prompt; that's what the anti-flash tests isolate.`
- `:118-119` → `// Equal epochs → computeBehindByEpochs returns undefined (near-tip hide) even though isBehindnessKnown stays true.`
- `:138-141` → `// networkTip unresolved (isBehindnessKnown false) but a finite certifiedEpoch > localTip.epoch makes behind-ness known via the beacon, so the prompt appears with the certified figure (105 − 97 = 8).`
- `:170-174` → `// Tips are finite (a local re-derivation would fire) but isBehindnessKnown is false: the prompt must stay hidden — the anti-flash guard consumes only the computed signal.`
KEEP: `:41-43`, `:157-158` (hybrid-anchor precedence with fixture math).
DELETE: `:70`, `:107-108`.

#### source/main/utils/chainStorageCoordinator.spec.ts (9 blocks; ~20 lines removed)

KEEP: `:841` (flush ordering before timer advance).
DELETE: `:749`, `:781`, `:791-797` (cites CP-A/CP-B), `:812`, `:820-827`, `:848`, `:863`, `:873`.

#### source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts (10 blocks; ~9 lines removed)

KEEP: `:287` (why the startup delay is overridden to 0).
DELETE: section dividers `:5`, `:45`, `:99`, `:284`; inline fixture labels `:134`, `:331`; assertion narration `:207`, `:209`. Divider `:195` carries a real claim — fold into the describe/it name ("unsafe cutover: the native dialog is the single recovery surface") before removing.

#### Remaining spec/story files

- `MithrilController.spec.ts:55-57` [KEEP] · `:80` [DELETE] · `:195-198` [REWRITE] → `// The service is class-mocked here, so this only asserts delegation to forceKillForShutdown(); the sync-mode SIGKILL itself is pinned in MithrilPartialSyncService.spec.ts.`
- `Diagnostics.stories.tsx:76-77`, `:105-108` [DELETE] — both mooted by CAT-E (see "Mooted by
  earlier CATs").
- `MithrilPartialSyncDialogue.stories.tsx:7-8` [REWRITE] → `// onStart must return a Promise so the confirm-view "Start now" await resolves like the real store call.` · `:37-40` [DELETE].
- `MithrilStepIndicator.spec.tsx:62-66` [REWRITE] → `// Under the svg-jest transform, react-svg-inline renders a bare <svg> without its wrapper className, so the active-step spinner is asserted via the .iconContainer svg, not the .iconSpinner token.` · `:81` [DELETE].
- `MithrilBootstrap.spec.tsx:239-242` [REWRITE] → `// With no actions prop, MithrilErrorView renders its default order: destructive primary "Wipe chain & retry" first (left), "Sync from genesis" second.` · `:248-249`, `:260-261` [KEEP] (render-order vs flex-end visual-order).
- `chainStorageValidation.spec.ts:521` [KEEP, borderline] · `:548-550` [REWRITE] → `// Path basename is "chain" (so isDirectChainSelection=true), yet its parent /tmp/state/chain/db still sits inside the managed chain dir — the edge this asserts.` (`:62` is pre-existing.)
- `MithrilProgressView.spec.tsx:9-11` [KEEP] (match the overlay spec's PopOver note).
- `mithrilCommandRunner.spec.ts:285`, `:299`, `:314` [DELETE] (`:118` is pre-existing and fine).
- `handleDiskSpace.spec.ts:754`, `:770` [DELETE] — fold `:770` into the test name: "does not emit idle while Mithril is still in control".
- `MithrilPartialSyncConfirmation.spec.tsx:35`, `:61`, `:109` [DELETE] (change-narration; fold `:61` into name: "does not show a sync percentage in the confirmation") · `:63` [KEEP] (never expose the internal "immutable files" unit — product vocabulary invariant).
- `MithrilPartialSyncSection.spec.tsx:23-24` [KEEP] · `:97` [DELETE].
- `DaedalusDiagnostics.spec.tsx:78-79` [REWRITE] → `// PopOver is mocked to render only its children, so the hover-only tooltip copy is intentionally absent from these assertions.` · `:83` [DELETE].
- `mithrilBehindness.spec.ts:34`, `:74` [KEEP, borderline] — fold-into-name candidates.
- `killProcessTree.spec.ts:10-11` [KEEP]; `mithrilSnapshotMetadata.spec.ts:44` [KEEP]; `mithrilPartialSyncChannel.spec.ts:261` [KEEP] (justifies a magic index); `mithrilPartialSyncMarker.spec.ts:127` [DELETE]; `SyncingConnectingMithrilPrompt.spec.tsx:113` [DELETE].
- `ChainStorageLocationPicker.spec.tsx:512` [DELETE] `// Should display the raw rejected path, NOT /mnt/bad-chain/chain/db/chain` — assertion narration with ALL-CAPS; the it() name carries it.
- `storybook/main.ts` [KEEP — all blocks, ~11 lines] webpack stub/SWC-decorator rationale (child_process/dgram/fs stubs, MobX 5 legacy-decorator constraint) — build-critical, hazard #10 category; never strip in a mechanical pass.
- Pre-existing/moved (no action): `tests/wallets/e2e/steps/import-and-migrate-wallets.ts:352,359,381,388`, `tests/wallets/e2e/steps/wallets.ts:26`, `tests/delegation/e2e/steps/delegation-pending.ts:75` (`@ts-ignore` — functional), `storybook/stories/index.ts:5,7`.

## Totals

| Part | Blocks | DELETE | REWRITE | KEEP | Est. lines removed |
|---|---|---|---|---|---|
| A — main process | 87 | 6 | 44 | 37 | ~108 |
| B — renderer/common | 109 | 4 | 50 | 55 | ~220 |
| C — specs/stories/e2e | ~218 | ~133 | ~33 | ~52 | ~300 |
| **Total** | **~414** | **~143** | **~127** | **~144** | **~630 of 1,012 (≈62%)** |

Projected end state: ~380–450 added comment lines remaining, prose density of the branch diff drops to roughly the repo's ~2.6% baseline, and no comment block over ~8 lines (the two compressed public JSDocs are the largest survivors).

## Consolidated hazards — invariants that must survive the cleanup

1. **Slot-clobber safety** (`MithrilPartialSyncService.ts:92-98`, `:769-776`, `:1382-1386` + echoes at `:215-217`, `:691-692`, `:755-761`, `:961-962`): "only cancelable runs register into `_currentProcess`; untracked metadata reads omit `onProcess`" is the actual bug fix (a cancelled download left unkillable). It must survive at `:769-776` and `:1382-1386` at minimum — do not let both collapse.
2. **`_logStream` write-only** (`:769-776`, `:1374-1377`): **moot in full** — CAT-A (A-5) deletes the write-only `_logStream` field and the `:1374-1377` assignment site before CAT-H runs (see "Mooted by earlier CATs"), so there is no stream left to keep a tripwire for; the `:769-776` replacement deliberately omits it.
3. **Cancel never reaches cutover** (`:303-307`, `:610-613`): keep the invariant while dropping the CP-x labels.
4. **Detached-child shutdown chain** (service `:469-476`, controller `:212-218`, `main/index.ts:91-94`, `killProcessTree.ts:8-28`, `mithrilCommandRunner.ts:169-172`): compress each site, but the "detached POSIX child survives process.exit(); the reap must be sync because safeExit exits inside a stream-end callback" reason must survive in `killProcessTree` and `forceKillForShutdown`.
5. **Staging is a same-volume sibling, never inside the live chain subtree** (`:903-907`): cutover correctness.
6. **Disk fail-closed floor** (`:76-82`) and the spec arithmetic anchors (`Service.spec:1994/2019/2033/2054/2074`, firm KEEP): the margin/floor/fallback formulas — including that the margin is deliberately sized on the full snapshot, not the delta — are stated nowhere else.
7. **MobX strict-mode / runInAction** (`Store.ts:331-339`, `:409-412`; `Store.spec.ts:258-268`, `:272`; `index.tsx:25-28`): under `enforceActions: 'observed'` a post-await mutation outside an action throws and gets swallowed. Keep in both source and the spec that pins it.
8. **Electron 41 SIGABRT** (`CardanoNode.ts:808-813`, `safeExitWithCode.ts:12-13`): crash-prevention rationale; keep both.
9. **Vocabulary rule** (`mithrilBehindness.ts:3-16`, `MithrilPartialSyncConfirmation.spec.tsx:63`): epochs-only, never % or immutable-file counts in user-facing copy.
10. **Build constraints** (`renderer/webpack.config.js:49-51`, `:161-165`; `index.tsx:25-28`; `storybook/main.ts` — all blocks): MobX 5/SWC and @trezor/transport workarounds and webpack stubs — never strip in a mechanical pass.
11. **Functional directives that look like comments**: `MithrilPartialSyncOverlay.spec.tsx:23` (`eslint-disable-next-line`), `delegation-pending.ts:75` (`@ts-ignore`) — do not delete.
12. **UI-DOM constraint** (`MithrilProgressView.tsx:272-274`): disabled `<button>` swallows hover events — the comment protects the `<span>` wrapper from a future "simplification".
13. **Accuracy fixes bundled in** (do these even if scope is cut): `Service.ts:90` (stale "only aggregator cached"), `mithrilBootstrapChannel.ts:97-98` (stale method name), `MithrilBootstrapService.ts:200/:216` (duplicate/contradictory step labels).


## Execution order

CAT-H runs after CATs A–G have landed; per the master doc it lands as **one commit** (subject-only
message, no task IDs in any source comment) — the three phases below are the internal work order,
not separate commits. Line anchors have drifted further after A–G, so re-locating every entry by
its quoted text is mandatory (bare-number Part C entries follow the re-anchor procedure in
"Position in the wave"); entries mooted by A–G (see "Mooted by earlier CATs") are skipped.

Work file-by-file, largest first:

1. **Phase A — main process**: `MithrilPartialSyncService.ts`, `MithrilController.ts`, `killProcessTree.ts`, `mithrilPartialSyncNodeStartup.ts`, remaining `source/main/**` files.
2. **Phase B — renderer + common**: `MithrilPartialSyncStore.ts`, `MithrilPartialSyncOverlay.tsx`, `MithrilProactivePromptContainer.tsx`, `mithrilBehindness.ts`, remaining `source/renderer/**` and `source/common/**` files.
3. **Phase C — specs, stories, e2e**: all `*.spec.ts(x)`, `storybook/**`, `tests/**`. Where a deleted comment carried intent the test name lacks, rename the test instead (suggestions inline in the inventory).

Rules during execution:
- Comment-only edits: never change executable code while deleting/compressing a comment, except test renames explicitly listed.
- Do not reformat `toHaveBeenCalledWith(...)` call expressions in specs — prettier 2.1.2 oscillates on the `('string', {object})` shape; deleting an adjacent comment line is fine, touching the call's line-wrapping is not.
- If a REWRITE's proposed text feels wrong at the call site, shorter wins; when in doubt between DELETE and a 1-line REWRITE, prefer the 1-line REWRITE only if the invariant is genuinely non-obvious.
- Rollback: CAT-H is a single comment-only commit (plus the explicitly listed test renames), so backing out is `git revert` of that commit — no code paths change.

## Verification gates (per phase and final)

Both the grep gate and the density gate now run against the branch diff **after CATs A–G have
landed** — their deletions shrink the ~1,012-line comment baseline (and the ≤ ~450 target) by
whatever comment lines they removed with their code, and mooted entries do not count. A–G also
**add** a few comment lines of their own (CAT-B's fallback-stage constant comment, CAT-C's
rewrites, CAT-D's union/copy-map comments, CAT-E's revalidation and story-wrapper comments,
CAT-F's selector comment and three-line story replacement) — immaterial at the ≤ ~450
tolerance, but they sit on the added side of the post-A–G diff, so the gate arithmetic is
approximate in both directions.

1. `yarn lint` and `yarn compile` — note: on Node v24 the renderer typecheck needs the `.scss.d.ts` files regenerated via `typed-scss-modules` first; apply the known jest `identity-obj-proxy` sidecar before treating jest failures as regressions.
2. `yarn test:jest --listTests`-scoped run over every touched spec file — comment edits must not change pass/fail counts.
3. `yarn prettier:check` on touched files only; classify any failure against pre-existing HEAD drift first (`git show HEAD:<file> | prettier --stdin-filepath <file>`) before attributing it to the cleanup — ~15 mithril files carry drift at HEAD.
4. Grep gate — after each phase, this must return nothing. It scans **all added lines, not just comment lines** — a review label can hide in a test name (see the `Service.spec.ts:1440` rename in Part C); verified zero false positives on the current diff:
   `git diff <base> -- source/ tests/ storybook/ | grep '^+' | grep -iE 'CP-[A-D]|CAT-[A-Z]|task-ux|task-[0-9]|PR #|finding|remediat'`
5. Density gate — re-run the block-length metric on the final diff: target ≤ ~8% of comment blocks at 4+ lines (repo norm 5.9%), no block over ~9 lines (the compressed killProcessTree JSDoc is the largest survivor), total added comment lines ~1,012 → ≤ ~450 per the inventory estimate.

## Out of scope

- `.agent/` docs (task IDs and narration belong there).
- i18n `defaultMessage`/`description` fields in `.messages.ts` files (translator-facing data, not comments).
- Pre-existing comments in files this branch merely moved or re-indented.
- Any behavior change, refactor, or test-coverage change.
