# task-ux-102 — Research Note (durable findings)

## Decisions
- **Option A (compute in service, merge in controller).** Behind-ness is computed in
  `MithrilPartialSyncService.getPartialSyncBehindness()` (it owns `resolveLatestSnapshotMetadata`, the
  aggregator helpers, `_createStageError`, and a `_chainStorageManager`); `MithrilController` merges
  `isEnabled` (from the coordinator) + behind-ness (from the service). Option B (inject a provider into
  the coordinator) was rejected as pure plumbing — the coordinator cannot reach the partial-sync
  service. See [[task-ux-101-research]] for the inherited seam and the now-resolved sync→async question.
- **The availability read is async end-to-end** (`MithrilController.getPartialSyncAvailability(): Promise<...>`).
  This resolves the task-ux-101 open question. The IPC handler already `await`s, so the channel, the
  `MithrilPartialSyncAvailability` type, and the renderer wrapper stayed FROZEN.
- **Gap by direct subtraction** (`gap = latest - local`), NOT the throwing `derivePartialSyncRange`:
  same math (`derivePartialSyncRange` is `start=local+1`, `end=latest` ⇒ length `latest-local`), but a
  read-only probe must never raise `PARTIAL_SYNC_NO_CERTIFIED_RANGE`. `gap<=0` and any failure degrade
  to `{ isSignificantlyBehind: false }` (PRD D2 truthfulness — never nudge toward a dead-end).
- **Threshold default PINNED to 20 immutable files.** Mainnet immutable chunk = 10·k = 21,600 slots
  ≈ 6h ⇒ one epoch (432,000 slots = 5 days) ≈ 20 immutable files = PRD D2's "≈ 1 epoch-equivalent."
  Backend-owned; overridable via `launcherConfig.mithrilPartialSyncThresholdImmutables`. QA-calibratable
  downward. The originally-proposed `2160` was the security param `k` (≈ 540 days as a file count ⇒
  would never fire) and was correctly rejected.
- **Cache = TTL-only (5 min), aggregator query only.** The slow-changing `latestImm` is cached; the
  local FS read runs every call so the gap tracks local progress immediately. No tight poll
  (Non-Functional req). On error the throw precedes the cache write, so a garbage value is never cached.

## Gotchas / evidence (verified against live code at planning + review time)
- **`getManagedChainPath()` is lock-free** (`chainStorageManager.ts:194-197`; not wrapped in
  `_withMutationLock`, unlike `setDirectory`/`ensureManagedChainLayout`/`installSnapshot`). The probe
  therefore never serializes behind an in-flight sync. Do NOT add `_awaitPendingMutations()`.
- **`getConfig()` is disk-backed/stateless** (`getConfig()` → `getChainStorageConfig(this)`; derives
  `customPath` from the on-disk symlink via `_captureChainPathState` → `fs.lstat`/`realpath`). So the
  service's separately-constructed `new ChainStorageManager()` (controller builds the service with no
  arg) still resolves the AUTHORITATIVE managed chain path, including relocated/symlinked chains.
- **nix→LauncherConfig pass-through has no field whitelist:** `readLauncherConfig` returns the whole
  parsed YAML, so `mithrilPartialSyncThresholdImmutables` flows through exactly like
  `mithrilPartialSyncEnabled`.
- **Async ripple is safe:** the only production caller of `controller.getPartialSyncAvailability()` is
  the already-`async` IPC handler (`mithrilPartialSyncChannel.ts:116-118`); all other references are
  test mocks. The 3 task-ux-101 availability tests survive unchanged (they already `await ...resolves`).
- **Test setup:** the fs-extra mock lacks `lstat`/`realpath`, so tests spy
  `service._chainStorageManager.getManagedChainPath` and drive `local` through the immutable-dir
  `readdir` mock; `Date.now` is spied to make the TTL cache test deterministic.

## Residual gaps / hand-off
- Threshold (20) + TTL (5 min) are QA-calibratable; revisit in phase-7. If QA shows aggregator-cache
  staleness, add invalidation on successful sync / directory change (TTL-only chosen for now).
- Phase-3 (task-ux-301/302/303) consumes `isSignificantlyBehind` + `behindByImmutables` from this
  read model to gate the recommendation/CTA + proactive prompt and to render the behind-ness copy line.

## Conflicts found between PRD / research / tasks JSON / live repo
- None material. The tasks JSON listed `mithrilPartialSyncPreflight.ts` as a target, but the cleanest
  change CONSUMES it (`resolveLocalImmutableNumber`) without modifying it, and adds `MithrilController.ts`
  (outside the JSON targetPaths) as the merge point — consistent with the task-ux-101 precedent and the
  prompt's "targetPaths is guidance, smallest truthful change" guidance.
