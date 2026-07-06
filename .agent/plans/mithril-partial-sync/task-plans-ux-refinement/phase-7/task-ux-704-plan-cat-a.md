# task-ux-704 — CAT-A plan: Dead IPC facade + dead main-process plumbing (pure deletions)

> Per-CAT implementation doc for the task-ux-704 remediation wave. **Self-contained —
> implementable from this doc alone by an implementer who makes zero decisions.** Parent:
> `task-ux-704.md` (traceability rows B2/D13, A4a–d, A6-subset). Every step is a pure deletion
> or a canonical-import retarget; no behavior, no IPC channel name, no emitted status shape,
> and no user-visible copy changes. If this doc ever disagrees with live code, prefer live
> code, locate anchors by the QUOTED snippets (never by line number), and reconcile here.
> All line numbers below were verified against the working tree on 2026-07-04 and are
> provided only as a reading aid.

## Sequencing position

CAT-A is the **first** commit of the wave (order A → B → C → D → E → F → G → H). Seam
contract **S1** binds this CAT to CAT-B:

- **S1 (`mithrilPartialSyncChannel.ts`):** delete ONLY the four dead partial-sync shims
  (Step A-1). `emitMithrilPartialSyncStatus` and `getMithrilPartialSyncStatus` MUST survive
  this commit — `mithrilPartialSyncNodeStartup.ts` still imports them until CAT-B rewires
  that import to injected dependencies. CAT-B owns their deletion (iff orphaned after the
  rewire). Do not "finish the job" here.

## Conventions (locked for every step)

- **One commit** for the whole CAT. Suggested subject (subject line only — no body, no
  trailers): `refactor(mithril): delete dead ipc facade exports and dead main-process plumbing`
- Never cite task/finding IDs (CAT-A, A-5, B2/D13, PR numbers) or review labels in source
  comments or test titles.
- Never reformat `toHaveBeenCalledWith('string', { obj })` call shapes — prettier 2.1.2
  oscillates on them. New/edited assertions keep the existing `expect.objectContaining` style.
- Deletion edits must not change jest pass/fail counts except where a step below explicitly
  deletes or retargets a spec block along with its dead subject.

## i18n

**No i18n work in CAT-A.** Nothing here touches renderer copy, `en-US.json` / `ja-JP.json`,
or defaultMessages.

---

# Step A-1 — Four dead partial-sync channel shims

File: `source/main/ipc/mithrilPartialSyncChannel.ts`

Delete these four exports in full (lines ~69–87). They form a contiguous block between
`getMithrilPartialSyncStatus` and `let mithrilPartialSyncRequestsInitialized`:

```ts
export const isMithrilPartialSyncActive = () =>
  getMithrilController().isPartialSyncActive();

export const setMithrilPartialSyncActiveProvider = (
  _provider: () => boolean
) => {};

export const setMithrilPartialSyncStatus = (
  status: MithrilPartialSyncStatusSnapshot
) => {
  getMithrilController().setPartialSyncStatus(status);
  return status;
};

export const onMithrilPartialSyncStatus = (
  handler: (status: MithrilPartialSyncStatusSnapshot) => void
) => {
  return getMithrilController().onPartialSyncStatus(handler);
};
```

Verified caller inventory (2026-07-04): the only references outside this file are in its own
spec (`mithrilPartialSyncChannel.spec.ts`). `setMithrilPartialSyncActiveProvider` is a
literal exported no-op.

**S1 keep-list — do NOT touch:** `getMithrilPartialSyncStatus` (~:66),
`emitMithrilPartialSyncStatus` (~:132), `configureMithrilPartialSyncRuntime`,
`handleMithrilPartialSyncRequests`, and the `MithrilPartialSyncStatusSnapshot` type import
(still used by kept `emitMithrilPartialSyncStatus`'s parameter — `getMithrilPartialSyncStatus`
never references it).

### Spec: `source/main/ipc/mithrilPartialSyncChannel.spec.ts`

The test `it('keeps compatibility exports as controller proxies', ...)` (~:145–174) mixes
dead and LIVE exports — trim it surgically, do not delete it:

- Delete these lines from the test body:
  - `mithrilControllerMock.isPartialSyncActive.mockReturnValue(true);`
  - `moduleExports.setMithrilPartialSyncStatus(status);`
  - `moduleExports.onMithrilPartialSyncStatus(listener);`
  - `expect(moduleExports.isMithrilPartialSyncActive()).toBe(true);`
  - the `expect(mithrilControllerMock.setPartialSyncStatus).toHaveBeenCalledWith(status);`
    assertion (all lines of it)
  - the `expect(mithrilControllerMock.onPartialSyncStatus).toHaveBeenCalledWith(listener);`
    assertion (all lines of it)
  - the now-unused `const listener = jest.fn();` declaration
- Keep the `configureMithrilPartialSyncRuntime`, `emitMithrilPartialSyncStatus`,
  `getMithrilPartialSyncStatus`, and `broadcastPartialSyncStatus` exercises/assertions —
  those exports are live until CAT-B.
- In the `mithrilControllerMock` object (~:19–34), delete the now-unreferenced keys
  `isPartialSyncActive`, `setPartialSyncStatus`, `onPartialSyncStatus`.
- In `beforeEach` (~:67), delete
  `mithrilControllerMock.isPartialSyncActive.mockReturnValue(false);`.

# Step A-2 — Eight dead bootstrap channel wrappers

File: `source/main/ipc/mithrilBootstrapChannel.ts`

Delete these eight exports (lines ~57–92; everything between the
`export { isMithrilDecisionCancelledError, MithrilDecisionCancelledError };` line — handled
in Step A-3 — and `let mithrilBootstrapRequestsInitialized`, EXCEPT the two live wrappers
listed below):

- `getPendingMithrilBootstrapDecision`
- `getMithrilBootstrapStatus`
- `isMithrilBootstrapNodeStartBlocked`
- `onMithrilBootstrapStatus`
- `onMithrilBootstrapDecision`
- `setMithrilBootstrapStatus`
- `waitForMithrilBootstrapDecision`
- `resetMithrilDecisionState`

**Keep (live, verified 2026-07-04):**

```ts
export const getMithrilBootstrapNodeState = () =>
  getMithrilController().getNodeState();
export const setMithrilBootstrapNodeStateProvider = (
  provider: () => CardanoNodeState | null | undefined
) => {
  getMithrilController().setNodeStateProvider(provider);
};
```

(`getMithrilBootstrapNodeState` → `chainStorageChannel.ts` ~:49 and ~:63;
`setMithrilBootstrapNodeStateProvider` → `main/index.ts` ~:208.)

After the deletions the type imports `MithrilBootstrapDecision` and
`MithrilBootstrapStatusUpdate` (~:22–25) have no remaining users in this file — delete that
import block. Keep the `CardanoNodeState` type import (used by the kept provider wrapper).

### Spec: `source/main/ipc/mithrilBootstrapChannel.spec.ts`

- In `it('keeps compatibility exports as controller proxies', ...)` (~:144–166): delete the
  exercises `setMithrilBootstrapStatus({ status: 'decision' })`,
  `waitForMithrilBootstrapDecision()`,
  `resetMithrilDecisionState({ suppressStatusBroadcast: true })` and their three matching
  assertions (`setBootstrapStatus` / `waitForBootstrapDecision` /
  `resetBootstrapDecisionState`). Keep the `setMithrilBootstrapNodeStateProvider` exercise
  and its `setNodeStateProvider` assertion — that wrapper is live.
- In the `mithrilControllerMock` object (~:8–29), delete the now-unreferenced keys
  `getPendingBootstrapDecision`, `isBootstrapNodeStartBlocked`, `onBootstrapStatus`,
  `onBootstrapDecision`, `setBootstrapStatus`, `waitForBootstrapDecision`,
  `resetBootstrapDecisionState`. KEEP `getBootstrapStatus` — the live status channel
  `onRequest` test (~:111) still drives it. Also delete the `beforeEach` re-prime
  `mithrilControllerMock.getPendingBootstrapDecision.mockReturnValue(null);` (~:66) — it
  references a deleted key (every test in the suite crashes, plus TS2339, if it stays). The
  `getBootstrapStatus` / `listSnapshots` / `submitBootstrapDecision` / `startBootstrap` /
  `cancelBootstrap` primes stay (their keys survive).

### Spec: `source/main/utils/handleDiskSpace.spec.ts`

The `jest.mock('../ipc/mithrilBootstrapChannel', ...)` factory (~:90–107) carries mock keys
for the deleted wrappers. Combined with Step A-3 (which removes `handleDiskSpace.ts`'s last
import from this channel), the ENTIRE factory block becomes inert — delete the whole
`jest.mock('../ipc/mithrilBootstrapChannel', () => ({ ... }))` block, plus:

- the now-orphaned `let mithrilBootstrapStatus = { ... }` declaration (~:42–46) and its
  `beforeEach` re-seed (`mithrilBootstrapStatus = { status: 'idle', ... }` ~:222) — its only
  readers were the deleted `getMithrilBootstrapStatus`/`setMithrilBootstrapStatus` mock keys.

KEEP `resetMithrilDecisionStateMock` (~:38) and `mithrilBootstrapStatusChannelMock` (~:19) —
they are wired through the REAL controller (`jest.spyOn(mithrilController,
'resetBootstrapDecisionState')` ~:253 and `setBootstrapStatusSender` ~:259), not through the
channel mock, and are asserted at ~:460/:660 and throughout.

**Tripwire:** if deleting the factory block makes jest fail on an unmocked `electron` import
(meaning some other module in this spec's graph still loads the real channel), restore the
block with only `getMithrilBootstrapNodeState`/`setMithrilBootstrapNodeStateProvider`-shaped
keys plus `mithrilBootstrapStatusChannel`, and note the surviving import path in the impl
review — do not improvise further.

# Step A-3 — Canonical-import retarget for `isMithrilDecisionCancelledError`

Re-export chain today (verified): `mithrilDecision.ts` (~:8, canonical) →
`MithrilController.ts` (~:55, re-export) → `mithrilBootstrapChannel.ts` (~:55, re-export) →
`handleDiskSpace.ts` (~:23, consumer).

1. `source/main/utils/handleDiskSpace.ts` — replace

   ```ts
   import { isMithrilDecisionCancelledError } from '../ipc/mithrilBootstrapChannel';
   ```

   with

   ```ts
   import { isMithrilDecisionCancelledError } from '../mithril/mithrilDecision';
   ```

2. `source/main/ipc/mithrilBootstrapChannel.ts` — delete the re-export line

   ```ts
   export { isMithrilDecisionCancelledError, MithrilDecisionCancelledError };
   ```

   and remove `isMithrilDecisionCancelledError` and `MithrilDecisionCancelledError` from the
   `from '../mithril/MithrilController'` import (~:28–32) — the file has no other use of
   either; keep `getMithrilController`.

3. `source/main/mithril/MithrilController.ts` — the re-export at ~:55
   (`export { isMithrilDecisionCancelledError, MithrilDecisionCancelledError };`) had exactly
   ONE importer: the channel edited in (2). Verified 2026-07-04 — re-run
   `grep -rn "from './MithrilController'\|from '../mithril/MithrilController'" source/` and
   confirm no one else imports the pair from the controller before deleting the re-export
   line. Then drop `isMithrilDecisionCancelledError` from the
   `from './mithrilDecision'` import (~:30–33) — the controller itself never calls the
   predicate. KEEP the `MithrilDecisionCancelledError` class import — used at ~:369
   (`new MithrilDecisionCancelledError()` in `resetBootstrapDecisionState`).

4. Specs: in `mithrilBootstrapChannel.spec.ts`, the local
   `class MithrilDecisionCancelledError extends Error {}` (~:31) and the
   `MithrilDecisionCancelledError` / `isMithrilDecisionCancelledError` keys in the
   `jest.mock('../mithril/MithrilController', ...)` factory (~:44–49) existed only to feed
   the deleted re-export — remove them (keep the `getMithrilController` key). In
   `handleDiskSpace.spec.ts` no replacement mock is needed: after the retarget the spec runs
   the REAL `mithrilDecision` module, whose `instanceof` predicate shares class identity with
   the real controller this spec already drives — do NOT add a
   `jest.mock('../mithril/mithrilDecision')`.

# Step A-4 — Production-empty listener plumbing in `MithrilController`

File: `source/main/mithril/MithrilController.ts`

Their only subscriber routes were the shims deleted in A-1/A-2 (channel wrappers
`onMithrilPartialSyncStatus` / `onMithrilBootstrapStatus`); nothing else in production or
specs calls `controller.onBootstrapStatus` / `controller.onPartialSyncStatus` (the
same-named keys in `MithrilController.spec.ts` ~:62–63 belong to the STARTUP GATE mock —
different object, keep them).

Delete:

- the fields `_bootstrapStatusListeners` (~:63–65) and `_partialSyncStatusListeners`
  (~:66–68);
- the methods `onBootstrapStatus(...)` (~:239–248) and `onPartialSyncStatus(...)`
  (~:250–260);
- inside `broadcastBootstrapStatus`, the line
  `this._bootstrapStatusListeners.forEach((listener) => listener(status));` (~:278);
- inside `broadcastPartialSyncStatus`, the line
  `this._partialSyncStatusListeners.forEach((listener) => listener(status));` (~:296);
- in `source/main/utils/handleDiskSpace.spec.ts`, `resetMithrilControllerForTests`
  (~:155–156), the two lines assigning `mithrilController._bootstrapStatusListeners = [];`
  and `mithrilController._partialSyncStatusListeners = [];` — the helper's controller value
  is cast to `MithrilController` (~:129), so leaving them breaks `yarn compile` (TS2339)
  once the fields are gone. KEEP the `_decisionListeners` reset on the next line.

**Keep-list (verified live):**

- `onBootstrapDecision(...)` (~:262–271) and `_decisionListeners` (~:69) — real subscriber in
  `initialize()` (~:117–119: `this.onBootstrapDecision((decision) => {
  this._startupGate.onBootstrapDecision(decision); });`) plus `submitBootstrapDecision`
  iteration (~:345).
- The `this._startupGate.onBootstrapStatus(status)` (~:277) and
  `this._startupGate.onPartialSyncStatus(status)` (~:295) calls — gate methods, not the
  deleted listeners.
- `setPartialSyncStatus` (~:319–324) — heavily used spec seeding seam (11 uses in
  `MithrilController.spec.ts`, 4 in `handleDiskSpace.spec.ts`).
- `setBootstrapStatus` (~:309) — live via the startup gate controller contract.

# Step A-5 — Write-only `_logStream` in `MithrilPartialSyncService`

File: `source/main/mithril/MithrilPartialSyncService.ts`

Delete:

1. the field declaration `_logStream: WriteStream | null = null;` (~:142);
2. the nulling line `this._logStream = null;` inside `_clearRuntimeWorkState()` (~:892);
3. in `_runCommand` (~:1364), the whole `onLogStream` registration and its comment
   (~:1373–1381), i.e. replace

   ```ts
   const callbacks: RunCommandCallbacks = {
     // ALWAYS registered — even untracked metadata reads write to the shared partial-sync log
     // file. A metadata read's onLogStream overwrites the shared _logStream slot mid-run,
     // which is harmless ONLY because _logStream is write-only plumbing today: declared as a
     // class field, nulled in _clearRuntimeWorkState(), assigned here, never read.
     onLogStream: (logStream) => {
       this._logStream = logStream;
     },
   };
   ```

   with

   ```ts
   const callbacks: RunCommandCallbacks = {};
   ```

   (The shared partial-sync log file itself is untouched: `openLogStream` /
   `attachLogStream` / `logStream.end()` live inside `mithrilCommandRunner.ts` and never
   depended on the callback.)

4. the hedge sentence in the `_trackCurrentProcess` invariant comment (~:769–776): delete
   from `They DO` through `never read).` so the comment ends at
   `...nor null the slot mid-run (the confirmed slot-clobber).`;
5. the now-unused `import type { WriteStream } from 'fs';` (~:5).

### Spec: `source/main/mithril/MithrilPartialSyncService.spec.ts`

In the slot-clobber repro (comment ~:1666–1669, assertions ~:1679–1682):

- rewrite the comment to drop the onLogStream clause, e.g.:

  ```ts
  // The metadata invocation carried NO onProcess callback AT ALL — it can neither
  // overwrite the slot on spawn nor null it on its own clean close (the confirmed
  // slot-clobber) — and trackAsCancelable never leaks into the runner options.
  ```

- replace

  ```ts
  expect(metadataInvocation.callbacks.onLogStream).toEqual(
    expect.any(Function)
  );
  ```

  with

  ```ts
  expect(metadataInvocation.callbacks.onLogStream).toBeUndefined();
  ```

### OPTIONAL rider (same commit, explicitly skippable): `MithrilBootstrapService.ts`

The identical write-only pattern exists in `source/main/mithril/MithrilBootstrapService.ts`:
field `_logStream: WriteStream | null = null;` (~:62), nullings (~:272 in `start`'s
`finally`, ~:314 at the end of `cancel()`), and the `onLogStream: (logStream) => {
this._logStream = logStream; },` registrations inside `_runBinary` (~:583–585) and
`_runCommand` (~:598–600). Zero spec references (verified). If taken: delete all five
anchors plus the now-unused `WriteStream` type import (~:5), and — because no registrar then
remains repo-wide — also delete `onLogStream` from `RunCommandCallbacks`
(`mithrilCommandRunner.ts` ~:28) and the two
`if (callbacks?.onLogStream) callbacks.onLogStream(logStream);` lines (~:240, ~:265). If NOT
taken, the runner's `onLogStream` plumbing stays (still has a live registrar) — do not
half-delete.

# Step A-6 — Dead `stdinInput` option in `mithrilCommandRunner`

File: `source/main/mithril/mithrilCommandRunner.ts`

Zero callers repo-wide pass `stdinInput` (verified: the only matches are this file's own
plumbing). `runCommand` already silently drops it (its destructure at ~:263 omits the key)
while `runBinary` forwards it — a contract lie either way. Delete:

1. `stdinInput?: string;` from `RunCommandOptions` (~:22);
2. `stdinInput?: string;` from `SpawnMithrilChildParams` (~:130);
3. `stdinInput,` from the `spawnMithrilChild` destructure (~:145);
4. the stdin write block in `spawnMithrilChild` (~:192–195):

   ```ts
   if (stdinInput !== undefined) {
     child.stdin?.write(stdinInput);
     child.stdin?.end();
   }
   ```

5. `stdinInput` from `runBinary`'s options destructure (~:238) and from the
   `spawnMithrilChild({ ... })` argument object (~:252).

Also update the shared-pipeline comment (~:134–136): change
`only args, env, and stdin handling differ per caller.` to
`only args and env differ per caller.`

No spec changes: `mithrilCommandRunner.spec.ts` has zero `stdin` references (verified).

# Step A-7 — Zero-caller startup-gate state getters

1. `source/main/mithril/MithrilController.ts` — delete (~:152–154):

   ```ts
   getStartupGateState(): MithrilStartupGateState {
     return this._startupGate.state;
   }
   ```

   Zero callers including specs (verified). Then remove `MithrilStartupGateState` from the
   `from './MithrilStartupGate'` import (~:24–29) — its only use was this return type. KEEP
   `MithrilStartupGate`, `MithrilStartupGateDependencies`, `MithrilStartupGateResult` in
   that import (all still used).

2. `source/main/mithril/MithrilStartupGate.ts` — delete (~:82–84):

   ```ts
   get state(): MithrilStartupGateState {
     return this._state;
   }
   ```

   Its only consumer was the getter deleted in (1).

3. `source/main/mithril/MithrilController.spec.ts` — in the
   `jest.mock('./MithrilStartupGate', ...)` factory (~:58–68), delete the now-unread
   `state: 'idle',` key.

**Keep-list:** `_state`, `_transition(...)` (~:588–596), and every `_transition` call —
the transition logs are the only startup-gate breadcrumbs pre-rollout; their removal is
explicitly deferred (master doc, deferred A2). The `MithrilStartupGateState` type export in
`MithrilStartupGate.ts` stays (still typing `_state` and `_transition`).

# Step A-8 — Spec-only public snapshot API on the partial-sync service

File: `source/main/mithril/MithrilPartialSyncService.ts`

Delete both public methods (~:1066–1074):

```ts
async listSnapshots(): Promise<Array<MithrilSnapshotItem>> {
  const snapshots = await this._listSnapshotsRaw();
  return snapshots.map(({ snapshot }) => snapshot);
}

async showSnapshot(digest: string): Promise<MithrilSnapshotItem | null> {
  const resolved = await this._showSnapshotRaw(digest);
  return resolved ? resolved.snapshot : null;
}
```

Zero production consumers (verified): the IPC snapshots path is
`mithrilBootstrapChannel` (`controller.listSnapshots()` ~:114) →
`MithrilController.listSnapshots` (~:395–397) → `chainStorageCoordinator.listSnapshots`
(~:166–169) → `MithrilBootstrapService.listSnapshots` — it never touches this service. The
private `_listSnapshotsRaw` / `_showSnapshotRaw` STAY (used by
`resolveLatestSnapshotMetadata` ~:921/:932). The `MithrilSnapshotItem` type import stays
(`_latestSnapshot` field ~:149).

The `_trackCurrentProcess` invariant comment (~:772 — the block A-5's trim shortens, which
keeps this line) still reads `ad-hoc listSnapshots/showSnapshot`; change that clause to
`ad-hoc _listSnapshotsRaw/_showSnapshotRaw` so the comment no longer names the deleted
wrappers.

### Spec: `source/main/mithril/MithrilPartialSyncService.spec.ts`

- `it('uses a distinct partial sync log file when running Mithril commands', ...)`
  (~:305–325) pins the live `mithril-partial-sync.log` file-name behavior through the dead
  wrapper — RETARGET it, do not delete: change `await service.listSnapshots();` (~:314) to
  `await service._listSnapshotsRaw();`. Everything else in the test stays byte-identical.
- `it('serves snapshot list and show reads from the shared metadata pipeline', ...)`
  (~:327–363) exercises only the two deleted wrappers (including the
  `showSnapshot('show-digest')` and `showSnapshot('   ')` cases) — delete the whole test.
  The underlying raw pipeline stays covered by the `resolveLatestSnapshotMetadata` and
  behind-ness probe suites.

---

## Verification

Environment prep first (Node v24 — do NOT misread env failures as regressions): regenerate
`.scss.d.ts` via `typed-scss-modules` and apply the gitignored `identity-obj-proxy` jest
sidecar per the repo verify-env note before judging `yarn compile` / jest output.

```bash
yarn test:jest --testPathPattern "source/main/(mithril|ipc|utils)/.*\.spec\.ts"
yarn test:jest --testPathPattern mithril
yarn lint
yarn compile
```

- Expected spec deltas — exactly: one whole test removed
  (`'serves snapshot list and show reads from the shared metadata pipeline'`); every other
  touched spec keeps its pass count (trimmed tests still pass; retargeted log-file test
  still passes).
- `yarn prettier:check` on touched files; classify failures against pre-existing HEAD drift
  first (`git show HEAD:<f> | prettier --stdin-filepath <f>`) — ~15 mithril files carry
  known drift; never "fix" `toHaveBeenCalledWith('str', { obj })` shapes.
- Behavior gate: `grep -rn "isMithrilPartialSyncActive\b\|setMithrilPartialSyncActiveProvider\|setMithrilPartialSyncStatus\b\|onMithrilPartialSyncStatus\b\|getPendingMithrilBootstrapDecision\|isMithrilBootstrapNodeStartBlocked\|waitForMithrilBootstrapDecision\|resetMithrilDecisionState\|stdinInput\|getStartupGateState" source/`
  must return zero production hits (controller methods `setPartialSyncStatus` /
  `setBootstrapStatus` remain by design).
- S1 gate: `emitMithrilPartialSyncStatus` and `getMithrilPartialSyncStatus` still exported
  from the channel and still imported by `mithrilPartialSyncNodeStartup.ts`.

## Rollback

Revert the single CAT-A commit (`git revert <sha>`). No follow-up state: the CAT is pure
deletion + one import retarget, so a revert restores HEAD behavior exactly.

## Files touched

- `source/main/ipc/mithrilPartialSyncChannel.ts` + spec — A-1
- `source/main/ipc/mithrilBootstrapChannel.ts` + spec — A-2, A-3
- `source/main/utils/handleDiskSpace.ts` + spec — A-3 (import retarget; spec mock cleanup),
  A-4 (spec: two listener-field resets in `resetMithrilControllerForTests`)
- `source/main/mithril/MithrilController.ts` — A-3, A-4, A-7
- `source/main/mithril/MithrilController.spec.ts` — A-7 (gate-mock `state` key)
- `source/main/mithril/MithrilPartialSyncService.ts` + spec — A-5, A-8
- `source/main/mithril/mithrilCommandRunner.ts` — A-6 (+ optional A-5 rider tail)
- `source/main/mithril/MithrilStartupGate.ts` — A-7
- `source/main/mithril/MithrilBootstrapService.ts` — optional A-5 rider only

No renderer files, no `common/ipc` contract files, no i18n files.

## Out of scope (do NOT do here, however tempting)

- **A1 `MithrilPartialSyncService` file split** (probe + metadata module) — deferred
  post-merge; CAT-A/B already shrink the file.
- **A3 `trackAsCancelable` → runner injection** — deferred; would invalidate the
  slot-clobber regression suite this doc carefully preserves.
- **A2 gate enum/`_transition` removal** and a dedicated `MithrilStartupGate` spec —
  deferred; the transition logs are the only startup-gate breadcrumbs (Step A-7 keep-list).
- **A6 EventEmitter rewrite / broadcast signature change** — Step A-4 is the deletion
  subset only; do not touch `broadcastBootstrapStatus`/`broadcastPartialSyncStatus`
  signatures or senders.
- Deleting `emitMithrilPartialSyncStatus` / `getMithrilPartialSyncStatus` — CAT-B (S1).
- Moving `MithrilPartialSyncStageError`, workDir params, decline dedupe, log prefixes —
  CAT-B. `getManagedChainPath` de-fork and chain-storage items — CAT-C. Error-code union
  typing — CAT-D. B6 marker sentinel type, B7 `setup.ts` twin suppression blocks, B8
  metadata key-path trim, B5 bootstrap spec split — all deferred (master doc).

## Acceptance checks

- Channel facade: 4 partial-sync shims and 8 bootstrap wrappers gone; the two S1 exports and
  the two live bootstrap wrappers intact; `handleDiskSpace.ts` imports the cancel predicate
  from `mithrilDecision`; no module imports it from the channel or controller re-exports
  (both re-export lines gone).
- Controller: no status-listener arrays/methods; decision listeners and both broadcast
  senders behaviorally unchanged; `setPartialSyncStatus` / `setBootstrapStatus` intact.
- Service/runner: no `_logStream` field in `MithrilPartialSyncService`; no `stdinInput`
  anywhere; no `getStartupGateState` / gate `state` getter; no public
  `listSnapshots`/`showSnapshot` on the partial-sync service while
  `_listSnapshotsRaw`/`_showSnapshotRaw` remain.
- Full mithril-scoped jest green with exactly the one planned test removal; lint + compile
  clean after env prep.

## Escalations

- **E1 (A-2 tripwire):** the `handleDiskSpace.spec.ts` channel-mock deletion assumes no
  other module in that spec's graph loads the real bootstrap channel. If jest disagrees,
  follow the tripwire in Step A-2 and record the import path in the impl review.
- **E2 (S1 tripwire):** if at implementation time anything besides
  `mithrilPartialSyncNodeStartup.ts` imports `emitMithrilPartialSyncStatus` /
  `getMithrilPartialSyncStatus`, stop — the S1 contract in the master doc is stale; escalate
  rather than deleting or rewiring here.
- Every quoted anchor above was verified against the working tree on 2026-07-04. If a quoted
  snippet cannot be found verbatim, reconcile against live code before editing — do not
  approximate.
