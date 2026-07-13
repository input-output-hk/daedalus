# task-ux-102 — Compute the certified-immutable-gap behind-ness signal (cached, no restore)

- Sprint: Mithril Partial Sync UX Refinement — phase-1 (Cross-Process Availability Contract And Behind-ness Signal)
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved`
- Build status: `completed`
- Interaction mode: `autonomous`
- Priority: critical · Estimated: 4h · Dependencies: task-ux-101 (completed)

## Why now
task-ux-101 shipped the availability contract with behind-ness STUBBED (`isSignificantlyBehind: false`,
`behindByImmutables: undefined`). This task fills it: the backend computes the certified-immutable gap
so the renderer can later (phase-3) gate the recommendation/CTA and the proactive prompt on
`isSignificantlyBehind` WITHOUT ever computing a threshold. Closes blocker #2's backend+contract half
(research-19 row 129; §4 — "significantly behind" is unconditional copy today, not logic). Implements
PRD D2.

## Interaction mode justification
`autonomous`: backend-only read-model computation reusing existing seams; no user-facing copy, no
product decision (the threshold default is a tunable backend constant, calibrated in QA — phase-7), no
destructive/irreversible action. The signal is read-only and never starts a sync.

## Scope
1. Add a backend behind-ness computation on `MithrilPartialSyncService` that reuses the EXISTING
   latest-snapshot resolver + local-immutable reader to produce `{ isSignificantlyBehind,
   behindByImmutables? }`.
2. Cache the aggregator query (the expensive network part) with a TTL so repeated availability requests
   do NOT tight-poll the aggregator (Non-Functional req).
3. Make `THRESHOLD_IMMUTABLES` backend-owned via a default constant + a launcher-config override.
4. Merge the behind-ness into the availability read model from task-ux-101 (in `MithrilController`,
   which owns both the coordinator (`isEnabled`) and `_partialSyncService` (behind-ness)).

## Non-goals
- Do NOT touch the channel, the `MithrilPartialSyncAvailability` type, or the renderer wrapper — FROZEN
  by task-ux-101.
- Do NOT add renderer gating/copy (phase-3 / task-ux-301, task-ux-303).
- Do NOT run a restore, take the mutation lock, add snapshot-selection, or tight-poll the aggregator.
- Do NOT route any synthetic throughput/remaining-time/% over IPC — only an integer gap + boolean.
- Do NOT expose or let the renderer compute the threshold.

## Dependencies
task-ux-101 (completed): availability type + channel + `controller.getPartialSyncAvailability()` →
`chainStorageCoordinator.getPartialSyncAvailability()` (kill-switch source).

## Research / docs / workflows / skills consulted
- PRD D2 (`...-prd.md:97-128`) — the exact gap formula; Non-Functional caching (`...-prd.md:619-628`);
  Technical Design main-process (`...-prd.md:640-644`).
- Tasks JSON `task-ux-102` (`...-tasks.json:78-110`) — acceptance + testCases carried verbatim below.
- Research-19 blocker #2 (row 129), §4 issue-#10 intent gap (lines 166-185).
- task-ux-101 canonical doc + research note (the inherited seam + the sync→async open question this
  task resolves).

## DESIGN DECISION (RESOLVED) — Option A: service computes, controller merges; the read becomes async

**Chosen: Option A.** Compute behind-ness in `MithrilPartialSyncService` (it already owns
`resolveLatestSnapshotMetadata()`, the aggregator helpers `_showSnapshotRaw`/`_listSnapshotsRaw`,
`_createStageError`, and a `_chainStorageManager`), and MERGE in `MithrilController.getPartialSyncAvailability()`
(it already owns both `chainStorageCoordinator` and `_partialSyncService`, and is the existing routing
hop). This resolves the task-ux-101 open question: **`getPartialSyncAvailability` becomes async
end-to-end.** The IPC handler is already `async () => controller.getPartialSyncAvailability()`
(`mithrilPartialSyncChannel.ts:116-117`) and already awaits, so the channel/type/renderer wrapper stay
FROZEN.

**Rejected: Option B** (inject a behind-ness provider into `chainStorageCoordinator` so it stays the
merger). The coordinator cannot reach `_partialSyncService`; injecting a provider is pure extra
plumbing with no benefit over merging in the controller, which already holds both collaborators.

**Verified seams that make Option A safe (checked against live code at planning time):**
- `MithrilPartialSyncService` constructor defaults `_chainStorageManager = new ChainStorageManager()`
  (`MithrilPartialSyncService.ts:96-101`); the controller builds the service with no arg
  (`MithrilController.ts:84`). A FRESH `ChainStorageManager` is fine here because
  `getManagedChainPath()` → `getConfig()` → `getChainStorageConfig(this)` is **disk-backed/stateless**
  (resolves from `_stateDirectoryPath` (the shared default) + the persisted `customPath`), so a separate
  instance resolves the SAME authoritative managed chain path, including relocated/symlinked chains.
- `chainStorageManager.getManagedChainPath()` (`chainStorageManager.ts:194-197`) is **lock-free** (NOT
  wrapped in `_withMutationLock`, unlike `setDirectory`/`ensureManagedChainLayout`), so the probe never
  serializes behind an in-flight sync.

## Files expected to change (exact paths)
1. `source/main/mithril/MithrilPartialSyncService.ts` — add the cached behind-ness computation + cache
   field + threshold helper + default constant.
2. `source/main/mithril/MithrilController.ts` — make `getPartialSyncAvailability()` async + merge.
3. `source/main/config.ts` — add `mithrilPartialSyncThresholdImmutables?: number` to `LauncherConfig`.
4. `nix/internal/launcher-config.nix` — add the threshold field (branch-scoped, set to the default) so
   it is visibly tunable.
5. `source/main/utils/chainStorageCoordinator.ts` — update the comment on the (still kill-switch-only)
   `getPartialSyncAvailability()` so the stub note no longer says "task-ux-102 replaces here" (behind-ness
   now lives in the controller+service); the method body stays the isEnabled source.
6. `source/main/mithril/MithrilPartialSyncService.spec.ts` — add the 4 behind-ness test cases.
7. `source/main/mithril/mithrilPartialSyncPreflight.ts` — CONSUMED ONLY (import
   `resolveLocalImmutableNumber`); no modification expected.
8. `source/main/ipc/mithrilPartialSyncChannel.spec.ts` — verify the existing 3 availability tests still
   pass now that the controller method is async (the mock returns a value the handler awaits — fine; make
   the mock return a Promise only if needed for fidelity).

## Implementation approach — ordered, mechanical steps

### Step 1 — Default threshold constant (value PINNED by the Critiquer — see Risks)
File: `source/main/mithril/MithrilPartialSyncService.ts`, near the other module constants
(`PARTIAL_SYNC_STAGING_DIRECTORY_NAME` at :57, `PARTIAL_SYNC_LOG_FILE_NAME`).
```ts
// Behind-ness threshold in IMMUTABLE FILES. Backend-owned; overridable via launcher config.
// Conservative starting point (≈ 1 epoch-equivalent of immutable files per PRD D2); calibrate down
// during QA so the prompt actually fires on a node that is days behind.
const DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES = 20; // ≈ 1 epoch (10·k = 21,600 slots ≈ 6h/file ⇒ ~20 files/5-day epoch); QA-calibratable
const PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS = 5 * 60 * 1000; // 5 min; only the aggregator query is cached
```

### Step 2 — Cache field
Add a class field beside the others (`MithrilPartialSyncService.ts:85-95`):
```ts
_latestCertifiedImmutableCache: { value: number; fetchedAt: number } | null = null;
```

### Step 3 — Cached aggregator read
Add a private method that wraps `resolveLatestSnapshotMetadata()` (`:571`, returns
`ResolvedLatestSnapshot` whose `latestCertifiedImmutableNumber: number` comes from
`mithrilSnapshotMetadata.ts:5`):
```ts
async _getCachedLatestCertifiedImmutableNumber(): Promise<number> {
  const now = Date.now();
  if (
    this._latestCertifiedImmutableCache &&
    now - this._latestCertifiedImmutableCache.fetchedAt < PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS
  ) {
    return this._latestCertifiedImmutableCache.value;
  }
  const snapshot = await this.resolveLatestSnapshotMetadata();
  this._latestCertifiedImmutableCache = {
    value: snapshot.latestCertifiedImmutableNumber,
    fetchedAt: now,
  };
  return snapshot.latestCertifiedImmutableNumber;
}
```
`Date.now()` is allowed (main-process Node). Only the network query is cached; the local FS read in
Step 5 runs every call so the local position is always fresh.

### Step 4 — Threshold resolver
```ts
_getBehindnessThresholdImmutables(): number {
  const configured = launcherConfig.mithrilPartialSyncThresholdImmutables;
  return typeof configured === 'number' && configured > 0
    ? configured
    : DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES;
}
```
(`launcherConfig` is already imported at `MithrilPartialSyncService.ts:6`.)

### Step 5 — The behind-ness computation (gap by DIRECT subtraction; degrade to false)
Reuse `resolveLocalImmutableNumber(managedChainPath, createStageError)`
(`mithrilPartialSyncPreflight.ts:63-140`, imported at `MithrilPartialSyncService.ts:40`). Compute the
gap with the SAME definition as `derivePartialSyncRange` (`preflight:143-160`: `start = local+1`,
`end = latest` ⇒ length `= latest - local`) but WITHOUT its throw — a probe must never raise
`PARTIAL_SYNC_NO_CERTIFIED_RANGE`:
```ts
async getPartialSyncBehindness(): Promise<{
  isSignificantlyBehind: boolean;
  behindByImmutables?: number;
}> {
  try {
    const latest = await this._getCachedLatestCertifiedImmutableNumber();
    const managedChainPath = await this._chainStorageManager.getManagedChainPath();
    const localImmutableNumber = await resolveLocalImmutableNumber(
      managedChainPath,
      (stage, message, code) => this._createStageError(stage, message, code)
    );
    const gap = latest - localImmutableNumber;
    if (gap <= 0) {
      // local >= latest ⇒ no certified range to restore ⇒ never nudge (PRD D2 truthfulness)
      return { isSignificantlyBehind: false };
    }
    return {
      isSignificantlyBehind: gap >= this._getBehindnessThresholdImmutables(),
      behindByImmutables: gap,
    };
  } catch (error) {
    // Aggregator unreachable, no immutable dir yet, or any failure ⇒ degrade to not-behind.
    logger.warn(
      'MithrilPartialSyncService: behind-ness probe failed; treating as not significantly behind',
      { error }
    );
    return { isSignificantlyBehind: false };
  }
}
```
Confirm the exact arity of `resolveLocalImmutableNumber`'s `createStageError` factory and the
`_createStageError` signature against the live code and adapt the lambda if the parameter order differs
(it is `(stage, message, code?)` per `:672` usage). Lock-free + no restore: this only lists/show
snapshots (cached) and reads the immutable directory.

### Step 6 — Controller merge (becomes async)
File: `source/main/mithril/MithrilController.ts`, the `getPartialSyncAvailability()` passthrough added
in task-ux-101 (~:167). Replace the sync passthrough with an async merge:
```ts
async getPartialSyncAvailability(): Promise<MithrilPartialSyncAvailability> {
  const { isEnabled } = chainStorageCoordinator.getPartialSyncAvailability();
  if (!isEnabled) {
    return { isEnabled: false, isSignificantlyBehind: false };
  }
  const behindness = await this._partialSyncService.getPartialSyncBehindness();
  return { isEnabled, ...behindness };
}
```
Short-circuiting on `!isEnabled` avoids the aggregator query entirely when the kill switch is off.
`MithrilPartialSyncAvailability` is already imported here (task-ux-101).

### Step 7 — Coordinator comment narrowing
File: `source/main/utils/chainStorageCoordinator.ts`, the `getPartialSyncAvailability()` from
task-ux-101. Keep the body (it is the kill-switch source of truth and the disabled-path shape), but
update the inline comments so they no longer claim 102 replaces them here:
```ts
getPartialSyncAvailability(): MithrilPartialSyncAvailability {
  return {
    isEnabled: launcherConfig.mithrilPartialSyncEnabled === true,
    // Behind-ness is computed by MithrilController + MithrilPartialSyncService (task-ux-102);
    // these defaults are only the disabled-path / fallback shape.
    isSignificantlyBehind: false,
    behindByImmutables: undefined,
  };
}
```

### Step 8 — Launcher config field (TS type)
File: `source/main/config.ts`, in the `LauncherConfig` type after
`mithrilPartialSyncEnabled?: boolean;` (:77):
```ts
mithrilPartialSyncThresholdImmutables?: number;
```

### Step 9 — Launcher config (nix, branch-scoped)
File: `nix/internal/launcher-config.nix`, in the partial-sync block beside
`mithrilPartialSyncEnabled = true;` (:409):
```nix
mithrilPartialSyncThresholdImmutables = 20;  # behind-ness threshold in immutable files; calibrate in QA
```
Use the SAME value as `DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES` (= 20, pinned by critique). The
nix→LauncherConfig pass-through carries the field: `readLauncherConfig` returns the whole parsed YAML
with no field whitelist, so a sibling numeric field flows through exactly like `mithrilPartialSyncEnabled`.

## Locked invariants this task honors (inline)
- Renderer NEVER computes/sees the threshold; backend owns it (default constant + launcher-config
  override). Renderer consumes only `isSignificantlyBehind` + `behindByImmutables`.
- No auto-trigger; this is a read-only signal, never a start.
- Latest-snapshot-only; the probe reuses the SAME `resolveLatestSnapshotMetadata` used at start. No
  snapshot-selection.
- Aggregator query cached + low-frequency (TTL); no tight poll.
- No synthetic throughput/remaining-time/% over IPC.
- Lock-free, no restore, no live-chain mutation.
- When no certified range exists (or any failure), `isSignificantlyBehind` is `false` (PRD D2
  truthfulness — never nudge toward a `PARTIAL_SYNC_NO_CERTIFIED_RANGE` dead-end).

## Acceptance criteria (carried verbatim from tasks JSON)
1. Backend computes `isSignificantlyBehind` from the certified-immutable gap reusing the existing
   latest-snapshot resolver.
2. `THRESHOLD_IMMUTABLES` is launcher-config-tunable and backend-owned.
3. The availability read model carries `isSignificantlyBehind` and `behindByImmutables`.
4. The behind-ness Mithril query is cached and low-frequency.

## Verification plan (exact commands)
From repo root:
- `yarn compile` — validates the async signature change (controller + channel) and the new config field.
- `yarn lint` — clean on touched files.
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts` — existing + 4 new behind-ness
  tests pass.
- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts` — the 3 task-ux-101 availability
  tests still pass with the now-async controller method (6/6).
- `yarn test:jest source/main/utils/chainStorageCoordinator.spec.ts` — no regression.

New test cases (in `MithrilPartialSyncService.spec.ts`, mirroring its existing
`createLatestSnapshot(latestCertifiedImmutableNumber = 25)` fixture at :55-61 and the
`resolveLatestSnapshotMetadata` resolve/reject patterns).
**Test-setup note (REQUIRED, per critique):** the existing `fs-extra` mock lacks `lstat`/`realpath`, so
the real `getManagedChainPath()` path resolver would hit unmocked disk logic and be flaky. In each test
mock it directly: `jest.spyOn(service._chainStorageManager, 'getManagedChainPath').mockResolvedValue('/tmp/chain')`,
and stub the local immutable read (mock `resolveLocalImmutableNumber`, or the immutable-dir read it
relies on) to a fixed `localImmutableNumber`. Stub `resolveLatestSnapshotMetadata` to control `latest`.
1. **gap ≥ threshold → true + `behindByImmutables` set** — set `latest`/`local` so `gap ≥ threshold`;
   assert `{ isSignificantlyBehind: true, behindByImmutables: gap }`.
2. **gap < threshold → false (but `behindByImmutables` still set)** — `gap` positive but `< threshold`;
   per Step 5 (`gap > 0` always sets `behindByImmutables`), assert
   `{ isSignificantlyBehind: false, behindByImmutables: <smallGap> }`.
3. **no certified range → false (no throw)** — two sub-cases: (a) `local >= latest`; (b)
   `resolveLatestSnapshotMetadata` rejects. Both resolve to `{ isSignificantlyBehind: false }` and do
   NOT throw.
4. **aggregator cached, not tight-polled** — spy on `resolveLatestSnapshotMetadata`; call
   `getPartialSyncBehindness()` twice within the TTL; assert it was invoked ONCE. To make the TTL
   deterministic, `jest.spyOn(Date, 'now')` (or `jest.useFakeTimers`) and advance past the TTL for an
   optional third call that re-queries (asserts the cache expires).

## Risks / open questions
- **Threshold default PINNED to 20 immutable files (RESOLVED by critique).** Reasoning: a mainnet
  immutable chunk = 10·k = 21,600 slots ≈ 6h, so one epoch (432,000 slots = 5 days) ≈ 20 immutable
  files — exactly PRD D2's "≈ 1 epoch-equivalent." Conservative against over-prompting yet reliably
  fires for a node several days behind (~4 files/day). The originally-proposed `2160` was the security
  param `k`, not a file count (≈ 540 days behind ⇒ would never fire). QA-calibratable downward; not
  load-bearing for correctness. Use `20` in both Step 1 and Step 9.
- **`getPartialSyncBehindness` arity.** Confirm `resolveLocalImmutableNumber`'s `createStageError`
  signature and `this._createStageError` before wiring the lambda; adapt if the parameter order differs.
- **Cache invalidation strategy (non-blocking).** TTL-only is chosen: only the slow-changing `latestImm`
  is cached; the local position is read fresh each call, so the gap tracks local progress immediately.
  Revisit (invalidate on successful sync / directory change) only if QA shows staleness.
- **Lock-free read confirmed.** `getManagedChainPath()` and the snapshot helpers do not take the
  mutation lock; the probe will not serialize behind an in-flight sync. Implementer must NOT add
  `_awaitPendingMutations()`.

## Required doc / research updates
- Record in `task-ux-102-research.md`: the Option A decision, the disk-backed `ChainStorageManager`
  finding, the resolved sync→async question (and update the cross-reference in `task-ux-101-research.md`
  if useful), and the PINNED threshold + rationale.
- No PRD edit required; D2 already specifies the formula. Optionally back-reference the chosen default +
  config key in the PRD Technical Design (polish, not required).

## Review-log paths
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-1/task-ux-102-plan-review.md` (append-only).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-1/task-ux-102-impl-review.md` (created at impl time).

## Final outcome (completed 2026-06-22)
Implemented exactly as planned (Option A). Added `getPartialSyncBehindness()` to
`MithrilPartialSyncService` (gap = `latest - local`; `gap<=0` and every failure degrade to
`{ isSignificantlyBehind: false }`; `behindByImmutables` set when gap>0), backed by a 5-min TTL cache
of the aggregator query (`_getCachedLatestCertifiedImmutableNumber`) and a backend-owned threshold
(`DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES = 20`, overridable via
`launcherConfig.mithrilPartialSyncThresholdImmutables`). `MithrilController.getPartialSyncAvailability()`
became async and merges `isEnabled` (coordinator) + behind-ness (service), short-circuiting the
aggregator query when the kill switch is off. Added the config field (`config.ts`) + nix value (20).
Coordinator change was comment-only (kept as the kill-switch source). The channel/type/renderer wrapper
from task-ux-101 stayed FROZEN. Read-only + lock-free (no `_awaitPendingMutations`, no restore).
Verification: `yarn compile` PASS; `MithrilPartialSyncService.spec.ts` 30/30 (5 new behind-ness tests);
`mithrilPartialSyncChannel.spec.ts` 6/6 (101 availability tests survive the async change);
`chainStorageCoordinator.spec.ts` 37/37; lint clean on touched files. Plan critique: approved (threshold
pinned to 20). Code review: approved, independently re-verified (async ripple confirmed safe — sole
caller is the awaiting IPC handler). No deviations. The pre-existing `M .gitignore` is unrelated and
excluded from this task's commit.

## Status
- Planning status: `approved`
- Build status: `completed`
