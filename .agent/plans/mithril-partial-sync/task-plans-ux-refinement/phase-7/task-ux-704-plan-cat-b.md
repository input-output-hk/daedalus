# task-ux-704 — CAT-B plan: Mithril main-process structure

> Per-CAT implementation doc for the task-ux-704 remediation wave. **Self-contained —
> implementable from this doc alone by an implementer who makes zero decisions.** Parent:
> `task-ux-704.md` (traceability rows B1, B4, B3-corrected, A2-subset, A5, B9). Every step
> is behavior-preserving: no IPC channel names, no emitted status shapes, no user-visible
> copy, no locked-boundary behavior changes (B-6 is a pure log-string change). If this doc
> ever disagrees with live code, prefer live code, locate anchors by the QUOTED snippets
> (never by line number), and reconcile here. All line numbers below were verified against
> the working tree on 2026-07-04 (pre-CAT-A) and are provided only as a reading aid — CAT-A
> shifts some of them slightly.

## Sequencing position

CAT-B is the **second** commit, strictly AFTER CAT-A. Seam contracts:

- **S1 (from CAT-A):** CAT-A kept `emitMithrilPartialSyncStatus` /
  `getMithrilPartialSyncStatus` alive in `mithrilPartialSyncChannel.ts` because
  `mithrilPartialSyncNodeStartup.ts` imports them. Step B-1 rewires that import to injected
  dependencies and THEN deletes the two exports iff orphaned.
- **S2 (to CAT-D):** Step B-2 moves `MithrilPartialSyncStageError` into `mithrilErrors.ts`.
  CAT-D will type the class's `code` as the `MithrilPartialSyncErrorCode` union AT THAT NEW
  HOME — this CAT keeps `code?: string` byte-identical. Do not tighten it here.
- CAT-B touches `handleDiskSpace.ts` only at the `MithrilPartialSyncNodeStartup`
  construction site (~:124); CAT-C owns the inert `chainEmpty` branch (~:251–308) in the
  same file — leave that region strictly untouched.

## Conventions (locked for every step)

- **One commit** for the whole CAT. Suggested subject (subject line only — no body, no
  trailers): `refactor(mithril): break ipc import cycle and consolidate main-process structure`
- Never cite task/finding IDs (CAT-B, B-4, PR numbers) or review labels in source comments
  or test titles.
- Never reformat `toHaveBeenCalledWith('string', { obj })` call shapes — prettier 2.1.2
  oscillates; keep `expect.objectContaining` style for any edited assertion.
- Refactor edits must not change jest pass/fail counts except where a step below explicitly
  edits a spec alongside its subject.

## i18n

**No i18n work in CAT-B.** Every string below is a log line or main-process internal error
message; the internal error message TEXTS are preserved byte-identical (B-2/B-4 move who
constructs them, not what they say).

---

# Step B-1 — Break the mithril→ipc→mithril import cycle

**Defect:** `source/main/mithril/mithrilPartialSyncNodeStartup.ts` (~:7–10) imports

```ts
import {
  emitMithrilPartialSyncStatus,
  getMithrilPartialSyncStatus,
} from '../ipc/mithrilPartialSyncChannel';
```

and the channel delegates straight back through `getMithrilController()` — a
mithril→ipc→mithril cycle that stays inert only because the controller is reached via the
lazy accessor (`MithrilStartupGate.ts` ~:15 imports the `MithrilPartialSyncNodeStartup`
class for type use only, so that edge carries no runtime state).

### B-1.1 — Inject the two functions via `NodeStartupDependencies`

File: `source/main/mithril/mithrilPartialSyncNodeStartup.ts`

1. Delete the channel import quoted above. Add instead (type only):

   ```ts
   import type { MithrilPartialSyncStatusSnapshot } from '../../common/types/mithril-partial-sync.types';
   ```

2. Extend the existing dependency type (locate `type NodeStartupDependencies = {`):

   ```ts
   type NodeStartupDependencies = {
     mainWindow: BrowserWindow;
     cardanoNode: CardanoNode;
     wipeChainAndSnapshots: WipeChainAndSnapshots;
     getGeneration: () => number;
     emitPartialSyncStatus: (
       status: MithrilPartialSyncStatusSnapshot
     ) => Promise<void>;
     getPartialSyncStatus: () => MithrilPartialSyncStatusSnapshot;
   };
   ```

3. Add the two fields + constructor wiring, mirroring the existing four
   (`_emitPartialSyncStatus`, `_getPartialSyncStatus`; destructure both in the constructor
   and assign).

4. Replace all six usages — three emit sites and three get sites, in `startInstalledNode`
   (~:147–148 and ~:163–164) and `finalizeInstalledNodeStart` (~:204–205):
   `emitMithrilPartialSyncStatus(` → `this._emitPartialSyncStatus(` and
   `getMithrilPartialSyncStatus()` → `this._getPartialSyncStatus()`.

### B-1.2 — Wire the sole construction site

File: `source/main/utils/handleDiskSpace.ts` (~:124–130). `getMithrilController` is already
imported at ~:29. **Note:** the local `const mithrilController = getMithrilController();`
is declared at ~:131 — AFTER this construction site. (Closures over the later-declared
`const` would be legal — the deps are only invoked at runtime — but wire lazy closures
through the module-scope accessor anyway; it avoids reordering declarations and leaning on
TDZ timing:)

```ts
const partialSyncNodeStartup = new MithrilPartialSyncNodeStartup({
  mainWindow,
  cardanoNode,
  wipeChainAndSnapshots: (reason, nodeState) =>
    chainStorageCoordinator.wipeChainAndSnapshots(reason, nodeState),
  getGeneration: () => directoryChangeGeneration,
  emitPartialSyncStatus: (status) =>
    getMithrilController().broadcastPartialSyncStatus(status),
  getPartialSyncStatus: () => getMithrilController().getPartialSyncStatus(),
});
```

(`broadcastPartialSyncStatus` is exactly what `emitMithrilPartialSyncStatus` delegated to —
same emission path, same snapshot seam.)

### B-1.3 — S1 endgame: delete the two channel exports iff orphaned

Run `grep -rn "emitMithrilPartialSyncStatus\|getMithrilPartialSyncStatus" source/`. Expected
remaining hits after B-1.1/B-1.2: the channel's own definitions + its spec (+ inert spec
mocks listed in B-1.4). The renderer's `MithrilPartialSyncStore` imports a DIFFERENT module
(`renderer/app/ipc/...`) — not a caller. If anything else surfaces, STOP and escalate (S1
contract stale). Otherwise, in `source/main/ipc/mithrilPartialSyncChannel.ts` delete:

```ts
export const getMithrilPartialSyncStatus = () =>
  getMithrilController().getPartialSyncStatus();
```

and

```ts
export const emitMithrilPartialSyncStatus = async (
  status: MithrilPartialSyncStatusSnapshot
): Promise<void> => {
  await getMithrilController().broadcastPartialSyncStatus(status);
};
```

then delete the now-unused
`import type { MithrilPartialSyncStatusSnapshot } from '../../common/types/mithril-partial-sync.types';`.

### B-1.4 — Spec updates

1. `source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts` — the module mock (~:23–32)
   becomes injected fixtures:
   - Delete the whole `jest.mock('../ipc/mithrilPartialSyncChannel', () => ({ ... }))` block
     and the `const { emitMithrilPartialSyncStatus } = require(...)` handle (~:57–61).
   - Add module-scope consts carrying the SAME defaults the mock had:

     ```ts
     const emitPartialSyncStatus = jest.fn().mockResolvedValue(undefined);
     const getPartialSyncStatus = jest.fn(() => ({
       status: 'starting-node',
       allowedRecoveryActions: [],
       transferProgress: {},
       progressItems: [],
       error: null,
     }));
     ```

   - In `makeInstance` (~:78–89), pass both into the constructor object and add them to the
     returned bag.
   - Mechanically rename every remaining `emitMithrilPartialSyncStatus` reference in the
     file to `emitPartialSyncStatus` (including the `beforeEach` re-prime
     `emitMithrilPartialSyncStatus.mockResolvedValue(undefined);` ~:96). Test titles and
     assertion shapes stay otherwise byte-identical.

2. `source/main/ipc/mithrilPartialSyncChannel.spec.ts` — in the compatibility-proxies test
   (post-CAT-A it exercises `configureMithrilPartialSyncRuntime`,
   `emitMithrilPartialSyncStatus`, `getMithrilPartialSyncStatus`): delete the emit/get
   exercises and their assertions (`getMithrilPartialSyncStatus()).toBe(status)` and the
   `broadcastPartialSyncStatus` assertion, plus the
   `mithrilControllerMock.getPartialSyncStatus.mockReturnValue(status);` prime); keep the
   `configureMithrilPartialSyncRuntime` exercise + assertion and retitle the test
   `'configures the partial sync runtime through the controller'`. Delete the now-unused
   `const status = { ...idleStatus, status: 'starting-node' as const };` declaration
   (~:147) — after the trims nothing references it (lint fail otherwise); with no `await`
   left in the body, the retitled test may drop its `async` modifier. Also delete the
   `beforeEach` prime
   `mithrilControllerMock.broadcastPartialSyncStatus.mockResolvedValue(undefined);`
   (~:76–78) — it references the deleted key. The `getPartialSyncStatus` prime (~:66)
   stays — that key is kept. In the controller mock, delete the now-unreferenced
   `broadcastPartialSyncStatus` key; KEEP `getPartialSyncStatus` (the live status channel
   `onRequest` test still uses it).

3. `source/main/utils/handleDiskSpace.spec.ts` — delete the inert
   `jest.mock('../ipc/mithrilPartialSyncChannel', () => ({ ... }))` block (~:109–118): the
   nodeStartup class is factory-mocked in this spec, so nothing in its graph loads the real
   channel (already true today; certain after B-1).

# Step B-2 — `MithrilPartialSyncStageError` moves home; factory threading deleted

**Defect:** the class is private to `MithrilPartialSyncService.ts` (~:111–125) while
`mithrilErrors.ts` already exports the parallel `MithrilBootstrapStageError` +
`createStageError`. Every exported function in `mithrilPartialSyncPreflight.ts` (5:
`statRequiredPath`, `ensureReadablePath`, `readImmutableDirectory`,
`resolveLocalImmutableNumber`, `derivePartialSyncRange`) and `mithrilPartialSyncStaging.ts`
(3: `preparePartialSyncStagingDirectory`, `validateStagedDownloadOutput`,
`validateConvertedStagedOutput`) threads a `createStageError: PartialSyncStageErrorFactory`
param, and the service feeds them six identical lambdas
`(stage, message, code) => this._createStageError(stage, message, code)` (~:225, ~:230,
~:282–284, ~:300, ~:995, ~:1141).

### B-2.1 — `source/main/mithril/mithrilErrors.ts`

Two separate insertions — do not paste the type import mid-file:

1. Add to the top-of-file import block (alongside the existing
   `mithril-bootstrap.types` type import):

   ```ts
   import type { MithrilPartialSyncErrorStage } from '../../common/types/mithril-partial-sync.types';
   ```

2. Add BELOW the bootstrap `createStageError` (class body MOVED VERBATIM from the service —
   `this.name = 'MithrilPartialSyncStageError'` must stay byte-identical, it is the
   spec-visible identity):

```ts
export class MithrilPartialSyncStageError extends Error {
  stage: MithrilPartialSyncErrorStage;
  code?: string;

  constructor(
    message: string,
    stage: MithrilPartialSyncErrorStage,
    code?: string
  ) {
    super(message);
    this.name = 'MithrilPartialSyncStageError';
    this.stage = stage;
    this.code = code;
  }
}

export function createPartialSyncStageError(
  stage: MithrilPartialSyncErrorStage,
  message: string,
  code?: string
): MithrilPartialSyncStageError {
  return new MithrilPartialSyncStageError(message, stage, code);
}
```

**S2:** `code?: string` stays a plain string here — CAT-D owns the union tightening at this
new home.

### B-2.2 — `source/main/mithril/mithrilPartialSyncPreflight.ts`

- Delete the `PartialSyncStageErrorFactory` type (~:11–15) — after this step it has zero
  users in either file.
- Import `createPartialSyncStageError` from `./mithrilErrors`.
- Remove the `createStageError: PartialSyncStageErrorFactory` parameter from all five
  exported functions and construct directly via `createPartialSyncStageError(...)` with the
  SAME stage/message/code arguments. Parameter-order note: `statRequiredPath` keeps its
  trailing defaults, becoming
  `statRequiredPath(targetPath, message, stage = 'preparing', code?)`.
- Update this file's internal call sites (`resolveLocalImmutableNumber` calls
  `statRequiredPath` ×3, `ensureReadablePath` ×2, `readImmutableDirectory` ×1) to drop the
  factory argument.

### B-2.3 — `source/main/mithril/mithrilPartialSyncStaging.ts`

- Drop `PartialSyncStageErrorFactory` from the `./mithrilPartialSyncPreflight` import (keep
  `PARTIAL_SYNC_STAGED_DB_INVALID_CODE`, `statRequiredPath`).
- Import `createPartialSyncStageError` from `./mithrilErrors`.
- Remove the factory param from the three exported functions; construct directly; drop the
  factory argument from the two `statRequiredPath(...)` calls (their trailing
  `'verifying', PARTIAL_SYNC_STAGED_DB_INVALID_CODE` arguments shift left into the new
  `stage, code` positions).

### B-2.4 — `source/main/mithril/MithrilPartialSyncService.ts`

- Delete the local class (~:111–125) and add `MithrilPartialSyncStageError` to a (value)
  import from `./mithrilErrors`.
- Keep `_createStageError` (~:1328–1334) byte-identical — its body constructs the now-
  imported class; the `instanceof MithrilPartialSyncStageError` checks in
  `_deriveAllowedRecoveryActions` (~:822) and `_buildError` (~:1340) keep working via the
  import.
- Delete the six factory lambdas by shortening the six call sites, e.g. (~:223–226):

  ```ts
  const localImmutableNumber = await resolveLocalImmutableNumber(
    context.layoutResult.managedChainPath
  );
  ```

  and likewise for `derivePartialSyncRange` (~:227–231),
  `validateStagedDownloadOutput` (~:282–284), `validateConvertedStagedOutput` (~:298–301),
  the `resolveLocalImmutableNumber` call inside `_getCachedLocalImmutableNumber`
  (~:993–996), and `preparePartialSyncStagingDirectory` inside `_prepareStagingDirectory`
  (~:1138–1142).
- The `createFailure: (message) => this._createStageError('converting', ...)` lambda inside
  `_convertStagedSnapshot` (~:719–724) is a genuinely different shape (fixed stage/code) —
  leave it.

No spec changes: no spec references `MithrilPartialSyncStageError` by name, and
preflight/staging have no dedicated spec files — their coverage rides
`MithrilPartialSyncService.spec.ts`, whose observable error names/messages/stages are
unchanged.

# Step B-3 — `workDir` parameter deadweight in `MithrilBootstrapService` (corrected scope)

File: `source/main/mithril/MithrilBootstrapService.ts`

Five members carry an explicit `workDir` parameter defaulting to
`this._activeWorkDir ?? this._workDir`: `_runBinary` (~:576), `_runCommand` (~:592),
`_downloadSnapshot` (~:616), `_resolveDbDirectory` (~:787), and
`_cleanupSnapshotArtifacts`'s `workDir?` options key (~:547, applied at ~:551). Verified
2026-07-04: NO call site (~:183, ~:194–196, ~:229, ~:258, ~:285/:310, ~:318/:333–337, ~:618,
~:729–735, spec calls `_downloadSnapshot('latest', null)`) ever passes a value different
from that default — every pass happens while `_activeWorkDir` holds exactly the passed value
(set at ~:137, cleared only in `startBootstrap()`'s `finally` ~:270 / end of `cancel()` ~:312).
Equivalence note: dropping the params moves default evaluation from method entry to use
time; the only window where that could differ (a `cancel()` plus a concurrent
`setWorkDir` retarget landing mid-await) belongs to a run that is already cancelled and
whose output is discarded — observable behavior is unchanged.

Edits:

1. `_cleanupSnapshotArtifacts` — remove `workDir?: string;` from the options type and change
   `const workDir = options.workDir ?? this._activeWorkDir ?? this._workDir;` to
   `const workDir = this._activeWorkDir ?? this._workDir;`.
2. `_runBinary(binaryName, args, workDir, options)` and `_runCommand(args, options, workDir)`
   have OPPOSITE parameter orders (~:573–577 vs ~:589–593) — remove the `workDir` param from
   BOTH (this also aligns the two wrapper signatures: both end at `options`); compute
   `const workDir = this._activeWorkDir ?? this._workDir;` in each body and pass it to
   `runBinary(...)` / `runCommand(...)` positionally as today.
3. `_downloadSnapshot(digest, snapshot)` — drop the third param; internally pass
   `{ preserveDb: false }` (~:618) and drop the third argument from the download
   `_runCommand(...)` call (~:729–735).
4. `_resolveDbDirectory(digest?)` — drop the second param; compute the same default in the
   body for the `candidates` paths.
5. Call sites: drop the trailing `workDir` argument at ~:183 and ~:194–196; drop the
   `workDir` key from the cleanup options at ~:229, ~:258, ~:310, and ~:333–337 (keep
   `strict: true` there). In `cancel()` delete the now-unused local
   `const workDir = this._activeWorkDir ?? this._workDir;` (~:285). In
   `wipeChainAndSnapshots()` KEEP the local capture (~:318) — the failure log (~:339–345)
   still reports `workDir`. In `startBootstrap` KEEP the local `const workDir =
   this._workDir;` (~:134) — still feeds `this._activeWorkDir = workDir;` (~:137) and the
   log fields at ~:190–193 and ~:228.
6. `cancel()` nulls `_currentProcess` twice: inside the kill branch (~:304) and in the
   trailing teardown group (~:313). Delete the in-branch one (~:304) — the teardown group is
   the reliable one (it also covers the kill-throws path, where ~:304 never runs).
   Equivalence note (accepted): this leaves one observable window — during cancel's
   `await this._cleanupSnapshotArtifacts(...)` (~:310), a concurrent `startBootstrap` now
   sees the killed child still in the slot and throws
   `'Mithril bootstrap already in progress'` until the child's `close` event nulls the slot
   via `onProcess(null)`. That transient guard tightening applies only to a run that is
   already cancelled and mid-teardown, and is accepted — do not keep ~:304 to avoid it.

**KEEP BOTH FIELDS — locked decision.** `_workDir` is the idle-time base for
`listSnapshots` / `showSnapshot` / `_resolveCardanoDbDownloadMode` (all call `_runCommand`
with `_activeWorkDir` null), retargeted via `setWorkDir` from
`chainStorageCoordinator._syncMithrilWorkDir` (~:162). Do NOT collapse to `_activeWorkDir`
only — that variant was analyzed and refuted in the wave review.

No spec changes: all spec calls already omit `workDir`
(`service._downloadSnapshot('latest', null)` ×12), and no spec asserts the cleanup options
object or the duplicate null.

# Step B-4 — Startup-gate decline dedupe + dead `value` param

File: `source/main/mithril/MithrilStartupGate.ts`

**Defect:** `handleMithrilFailureDecline` (~:300–325) and `handleMithrilCancelledDecline`
(~:327–357) differ only in the in-flight flag, guard placement, one extra status guard, and
the wipe-message noun; the same decline prologue is duplicated again in
`_handleEmptyChainStartup` (~:402–418) and `_handleExistingChainStartup` (~:459–475).
`_markHadNotEnoughSpaceLeft(response, value)` (~:640–648) receives `false` at all six call
sites (~:407, ~:416, ~:445, ~:464, ~:473, ~:484). Verified: the two public decline methods
have NO callers outside this file (specs included) — internal call sites are ~:136, ~:147
(`onBootstrapDecision`), ~:403, ~:412, ~:460, ~:469 (the prologues), ~:518
(`_waitForFailureDecline`). The two FIELDS carry one reference beyond the method bodies:
both are reset in `resetOnDirectoryChange()` (~:221–222) — a live path
(`MithrilController.ts` ~:417 calls it; asserted at `MithrilController.spec.ts` ~:189).

### B-4.1 — One parameterized `_handleDecline`

Replace the two boolean fields `_failureDeclineInFlight` / `_cancelledDeclineInFlight`
(~:65–66) with:

```ts
_declineInFlight: Record<'failure' | 'cancelled', boolean> = {
  failure: false,
  cancelled: false,
};
```

In `resetOnDirectoryChange()` (~:221–222), replace the two field resets

```ts
this._failureDeclineInFlight = false;
this._cancelledDeclineInFlight = false;
```

with `this._declineInFlight = { failure: false, cancelled: false };` — left behind they are
TS2339 under `yarn compile`.

Replace BOTH public methods with (wipe messages byte-identical to today's two):

```ts
async _handleDecline(
  kind: 'failure' | 'cancelled',
  source: string,
  currentGeneration: number = this._getGeneration()
): Promise<boolean> {
  const deps = this._requireDependencies();
  if (this._declineInFlight[kind]) return false;

  this._declineInFlight[kind] = true;
  try {
    if (this._controller.getPendingBootstrapDecision() !== 'decline') {
      return false;
    }
    if (
      kind === 'cancelled' &&
      this._controller.getBootstrapStatus().status !== 'cancelled'
    ) {
      return false;
    }

    await this._emitIdleStatus();
    if (currentGeneration !== deps.getGeneration()) return false;
    await chainStorageCoordinator.wipeChainAndSnapshots(
      `User declined after bootstrap ${
        kind === 'failure' ? 'failure' : 'cancel'
      } (${source}). Wiped chain directory and Mithril snapshots.`,
      deps.cardanoNode.state
    );
    if (currentGeneration !== deps.getGeneration()) return false;
    await deps.cardanoNode.start();
    this._decision = null;
    this._decisionPrompted = false;
    return true;
  } finally {
    this._declineInFlight[kind] = false;
  }
}
```

(Guard-placement note: the failure variant used to check the pending decision BEFORE setting
its flag; moving the check inside the flag window is observably identical — the flag is
cleared in `finally` on every early return.)

Update the internal callers: ~:136 →
`this._handleDecline('failure', 'decision-listener', generation)`, ~:147 →
`this._handleDecline('cancelled', 'decision-listener', generation)`, ~:518 →
`await this._handleDecline('failure', 'status-listener', generation);`. (The existing,
DIFFERENT `_handleBootstrapDecline` — the initial pre-bootstrap decline at ~:560 — is not
part of this dedupe; leave it.)

### B-4.2 — Shared prologue helper

Add (near the two chain-startup handlers):

```ts
async _handleTerminalStatusDecline<TResponse>(options: {
  status: MithrilBootstrapStatusUpdate['status'];
  source: string;
  currentGeneration: number;
  response: TResponse;
}): Promise<MithrilStartupGateResult<TResponse> | null> {
  const { status, source, currentGeneration, response } = options;
  if (status !== 'failed' && status !== 'cancelled') return null;
  await this._handleDecline(
    status === 'failed' ? 'failure' : 'cancelled',
    source,
    currentGeneration
  );
  this._markHadNotEnoughSpaceLeft(response);
  return { handled: true, response };
}
```

Replace the twin `if (status === 'failed') {...}` / `if (status === 'cancelled') {...}`
blocks in `_handleEmptyChainStartup` (~:402–418, source `'polling-chain-empty'`) and in
`_handleExistingChainStartup` (~:459–475, source `'polling-chain-present'`, driven by
`bootstrapStatus`) with:

```ts
const declineResult = await this._handleTerminalStatusDecline({
  status,
  source: 'polling-chain-empty',
  currentGeneration,
  response,
});
if (declineResult) return declineResult;
```

(and the `bootstrapStatus` / `'polling-chain-present'` equivalent).

### B-4.3 — Drop the dead `value` param

Change `_markHadNotEnoughSpaceLeft<TResponse>(response, value)` to a single-parameter
method that hard-codes the assignment to `false`; update the two surviving explicit call
sites (~:445, ~:484) to `this._markHadNotEnoughSpaceLeft(response);` (the other four moved
into B-4.2's helper).

No spec changes: `MithrilStartupGate` has no dedicated spec (a dedicated spec is an
explicitly deferred item), and `handleDiskSpace.spec.ts` drives the gate only through
statuses/decisions — none of the renamed/merged members are referenced by name anywhere in
spec code (verified).

# Step B-5 — Small legibility in `MithrilPartialSyncService`

File: `source/main/mithril/MithrilPartialSyncService.ts`

### B-5.1 — Fallback-stage identity map → membership check

Replace the identity map `_getFallbackErrorStage` (~:1287–1309) — five identity
if-branches plus the trailing `'preparing'` fallback return — with a stage-set
membership check. Add `MithrilPartialSyncStatus` to the existing type import from
`'../../common/types/mithril-partial-sync.types'`, add at module scope (near the other
constants):

```ts
// Statuses that double as their own error stage; everything else (idle, the
// terminal statuses, stopping-node, cancelling, starting-node) falls back to
// 'preparing'.
const FALLBACK_ERROR_STAGES: ReadonlyArray<
  MithrilPartialSyncErrorStage & MithrilPartialSyncStatus
> = ['downloading', 'verifying', 'converting', 'installing', 'finalizing'];
```

and replace the method body with:

```ts
_getFallbackErrorStage(): MithrilPartialSyncErrorStage {
  const { status } = this._status;
  return (
    FALLBACK_ERROR_STAGES.find((stage) => stage === status) ?? 'preparing'
  );
}
```

(The intersection-typed array keeps this cast-free; `.find` narrows the return to the stage
type. Mapping is byte-equivalent to the deleted branches.)

### B-5.2 — `cancel()` double log

Keep the unconditional entry line (~:389–393,
`'MithrilPartialSyncService: cancel entry'`); delete the in-branch duplicate (~:397–404,
`'MithrilPartialSyncService: cancelling active partial sync process'`) which logs the same
status/pid/hadChild fields — keep the `if (this._currentProcess)` guard, the group-kill
comment, and the `killProcessTree(...)` call. Trim the entry-line comment (~:386–388), which
references the deleted line, to:

```ts
// Logs even when no child process is tracked (hadChild: false), recording the
// slot state at cancel.
```

Cross-doc note: the CAT-H comment inventory's `:386–388` entry is handed off to this
rewrite (the CAT-H doc marks it handled-by-CAT-B — CAT-H does not re-touch this comment).

Spec updates (`MithrilPartialSyncService.spec.ts` — the deleted line is PINNED):

- In `it('logs an info event at the real-cancel branch before killing the tracked process
  (observability)', ...)` (~:998–1038): delete the assertion on
  `'MithrilPartialSyncService: cancelling active partial sync process'` (~:1020–1027) and
  the two-line comment above the entry-line assertion (~:1028–1029); keep the
  `killProcessTree` / `fakeChild.kill` assertions and the `'cancel entry'` assertion.
  Retitle to `'logs the cancel entry with the tracked child before killing it
  (observability)'`.
- In `it('emits the unconditional cancel-entry log with an empty slot ...', ...)`
  (~:1040–1076): delete the trailing stale assertion

  ```ts
  expect(infoLog).not.toHaveBeenCalledWith(
    'MithrilPartialSyncService: cancelling active partial sync process',
    expect.anything()
  );
  ```

  and drop the `(the case the in-branch line can never log)` parenthetical from the comment
  at ~:1060–1061.

# Step B-6 — Log-prefix hygiene in `MithrilBootstrapService` (pure log strings)

File: `source/main/mithril/MithrilBootstrapService.ts`

Current mix (verified; the merge base `48b557a02` had ZERO `[mithril]` lines in this file —
they rode in on this branch): `[mithril]` ×9 (~:190, ~:198, ~:212, ~:214, ~:228, ~:230,
~:232, ~:242, ~:773–776), `[MITHRIL]` ×1 (~:357), `MithrilBootstrapService:` ×11.

Standardize on **`MithrilBootstrapService:`** (the file's majority form and the partial-sync
service's convention):

- Rewrite each `[mithril] Xxx` / `[MITHRIL] ...` message to
  `MithrilBootstrapService: xxx` (message tail otherwise unchanged, sentence-cased to match
  the existing `MithrilBootstrapService:` lines). The interpolated one at ~:357 becomes
  `` logger.info(`MithrilBootstrapService: ${reason}`); ``.
- Collapse the finalize narration triplet (~:228–232) — cleanup announce → clearing lock →
  lock cleared — to ONE line per action:

  ```ts
  logger.info('MithrilBootstrapService: cleaning up snapshot artifacts', {
    workDir,
  });
  await this._cleanupSnapshotArtifacts({ preserveDb: true });
  await this.clearLockFile();
  logger.info('MithrilBootstrapService: cleared bootstrap lock file');
  ```

  (The cleanup-options shape shown already reflects Step B-3.)

Scope guard: this file ONLY. The `[mithril]` spawn/exit lines in `mithrilCommandRunner.ts`
and the `[MITHRIL]` lines in `MithrilController.ts` / `MithrilStartupGate.ts` are other
files' conventions — out of scope. No spec asserts any of this file's log strings
(verified).

---

## Verification

Environment prep first (Node v24): regenerate `.scss.d.ts` via `typed-scss-modules` and
apply the gitignored `identity-obj-proxy` jest sidecar per the repo verify-env note before
treating `yarn compile` / jest failures as regressions.

```bash
yarn test:jest --testPathPattern "source/main/(mithril|ipc|utils)/.*\.spec\.ts"
yarn test:jest --testPathPattern mithril
yarn lint
yarn compile
```

- Expected spec deltas — exactly: the channel spec's compat test shrinks to the
  configure-only shape (B-1.4), the two cancel-log tests lose their pinned duplicate-line
  assertions (B-5.2), and the nodeStartup spec swaps its module mock for injected fixtures
  with unchanged titles/counts. Nothing else may change pass/fail counts.
- Cycle gate: `grep -rn "ipc/mithrilPartialSyncChannel" source/main/mithril/` returns
  nothing; `grep -rn "emitMithrilPartialSyncStatus\|getMithrilPartialSyncStatus" source/`
  returns nothing.
- Behavior gate: recovery emissions from `startInstalledNode` /
  `finalizeInstalledNodeStart` reach the renderer through the SAME
  `broadcastPartialSyncStatus` path with the SAME snapshot spreads; stage-error `name` /
  `message` / `stage` / `code` values byte-identical; wipe log messages byte-identical;
  no `hadNotEnoughSpaceLeft` semantics change (it was always set to `false` on these paths).
- `yarn prettier:check` on touched files; classify against pre-existing HEAD drift first
  (`git show HEAD:<f> | prettier --stdin-filepath <f>`); never reformat
  `toHaveBeenCalledWith('str', { obj })` shapes.

## Rollback

Revert the single CAT-B commit (`git revert <sha>`). CAT-A does not depend on CAT-B, so a
lone revert is safe; if CAT-C/D have already landed, revert those first (letter order
backwards) since CAT-D's S2 typing sits on B-2's moved class.

## Files touched

- `source/main/mithril/mithrilPartialSyncNodeStartup.ts` + spec — B-1
- `source/main/utils/handleDiskSpace.ts` + spec — B-1 (construction wiring; inert mock
  removal)
- `source/main/ipc/mithrilPartialSyncChannel.ts` + spec — B-1 (S1 endgame)
- `source/main/mithril/mithrilErrors.ts` — B-2
- `source/main/mithril/mithrilPartialSyncPreflight.ts` — B-2
- `source/main/mithril/mithrilPartialSyncStaging.ts` — B-2
- `source/main/mithril/MithrilPartialSyncService.ts` + spec — B-2, B-5
- `source/main/mithril/MithrilBootstrapService.ts` — B-3, B-6
- `source/main/mithril/MithrilStartupGate.ts` — B-4

No renderer files, no `common/` type files (B-5.1 only imports an existing type), no i18n
files.

## Out of scope (do NOT do here, however tempting)

- **A1 `MithrilPartialSyncService` file split** — deferred post-merge.
- **A3 `trackAsCancelable` → runner injection** — deferred; the slot-clobber suite must
  keep passing untouched (B-1/B-2/B-5 edits around it are surgical).
- **A2 gate enum/`_transition` removal** + dedicated `MithrilStartupGate` spec — deferred;
  B-4 must not touch `_state`/`_transition` or any `_transition` call.
- **B6 marker fail-safe sentinel type, B7 `setup.ts` twin suppression blocks, B8 metadata
  key-path trim, B5 bootstrap spec split** — all deferred (master doc); B-6 here is the
  log-prefix item (B9), not the deferred B6.
- Collapsing `_workDir`/`_activeWorkDir` into one field — REFUTED variant (Step B-3
  keep-list).
- Typing `MithrilPartialSyncStageError.code` as the union — CAT-D (S2).
- `handleDiskSpace.ts` `chainEmpty` branch and everything else in that file beyond the
  construction site — CAT-C.
- Log prefixes outside `MithrilBootstrapService.ts`; any push-model availability work (D2,
  deferred).

## Acceptance checks

- **B-1:** no mithril→ipc import remains; nodeStartup emissions ride injected deps wired at
  `handleDiskSpace.ts`; the two channel exports are gone; nodeStartup spec green with
  fixture-injected mocks.
- **B-2:** `MithrilPartialSyncStageError` + `createPartialSyncStageError` exported from
  `mithrilErrors.ts`; zero `PartialSyncStageErrorFactory` references repo-wide; six lambdas
  gone; error `name`/`stage`/`code`/message behavior pinned by the existing service spec
  (no assertions edited for B-2).
- **B-3:** none of the five members takes a `workDir` param; both wrappers end at
  `options`; single `_currentProcess = null` in `cancel()`; both fields and `setWorkDir`
  intact.
- **B-4:** one `_handleDecline` + one prologue helper; both wipe messages byte-identical;
  `_markHadNotEnoughSpaceLeft(response)` single-param; six former call sites reduced to two
  explicit + helper.
- **B-5:** `_getFallbackErrorStage` is a membership check with identical mapping; exactly
  one info line logged on cancel entry (spec-pinned).
- **B-6:** one log prefix in the file; triplet collapsed; diff touches only string literals
  (plus the B-3 options shape).

## Escalations

- **E1 (B-1.3 tripwire):** any surviving importer of the two channel exports beyond the
  channel spec ⇒ the S1 contract is stale — stop and escalate; do not rewire additional
  callers ad hoc.
- **E2 (B-4 tripwire):** if implementation-time code shows any caller of
  `handleMithrilFailureDecline` / `handleMithrilCancelledDecline` outside
  `MithrilStartupGate.ts`, stop — the zero-external-caller premise is broken.
- **E3 (B-2 tripwire):** if a spec anywhere asserts on `PartialSyncStageErrorFactory` or
  constructs the stage error class directly, stop and reconcile — none existed on
  2026-07-04.
- Every quoted anchor above was verified against the working tree on 2026-07-04 (pre-CAT-A;
  expect small line drift after the CAT-A commit — locate by quotes). If a quoted snippet
  cannot be found verbatim, reconcile against live code before editing.
