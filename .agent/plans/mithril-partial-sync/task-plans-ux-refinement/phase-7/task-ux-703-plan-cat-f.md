# task-ux-703 — CAT-F per-section implementation plan
## Structure, duplication, dead code, perf (T23, T24, T25, T26, T27, T29/T30, T3)

- Status: ready-to-implement (planning for task-ux-703 overall is `approved`; DD-703-1..14 locked)
- Branch: `feat/mithril-partial-sync-ux-refinement` (PR base `develop`)
- Sequencing: CAT-F is implemented AFTER CAT-A..E. All anchors below were verified against the
  working tree on 2026-07-03 **before** earlier sections landed — always locate by the QUOTED
  snippet, never by line number. CAT-C (T13) edits `MithrilPartialSyncOverlay.tsx` and CAT-E (T16/
  T22) edits `partialSyncErrorCopy.ts`, `MithrilPartialSyncSection.tsx`, and
  `MithrilPartialSyncService.ts` before F runs.

## AMENDMENT — 2026-07-03 (T23 rewritten after implementation escalation)

The CAT-F chunk-1 implementer escalated (all partial edits cleanly reverted): the approved T23
specified `getMithrilStartErrorMessage(error, intl)` as PREFERRING the concrete extracted
rejection message, and its spec step anchored a test named
`keeps confirmation open and shows concrete start failure`. Landed CAT-E deliberately made
`MithrilPartialSyncSection.startFromConfirmation` format `partialSyncStartFailureMessage`
unconditionally, deleted the concrete branch, added the comment that the renderer must never
surface the raw rejection message, and renamed/rewrote that test to
`keeps confirmation open and shows the localized start-failure fallback` (asserting the
concrete message is NOT rendered). Post-T16, rejection messages are stable codes — preferring
the concrete message would render raw codes like `PARTIAL_SYNC_DISABLED` as user copy (banned).

**Binding orchestrator decision (encoded in §1.1; do not relitigate):** code-to-copy mapping —
exactly what CAT-E's plan rationale anticipated. The helper resolves a known code through
`COPY_BY_CODE` (via a new additive lookup export on `partialSyncErrorCopy.ts`, no copy changes)
to that entry's `title` copy and otherwise returns the shared `partialSyncStartFailureMessage`;
it never returns raw `error.message` or raw code text. Both catch seams
(`MithrilPartialSyncSection`, `SyncingConnectingMithrilPrompt`) route through it; the landed
CAT-E section test stays green UNMODIFIED; the prompt's concrete-prose test is rewritten into a
coded/un-coded pair. The T22/T23 cross-section contract is amended accordingly. Changed by this
amendment: §1.1 (T23) in full, plus only the lines it feeds — the guardrail/i18n bullets, the
Verification command list, Files touched, the T23 acceptance row, Escalations 1/5/6, and the
first Out-of-scope bullet. T24, T25, T26, T27, T29/T30, T3, and all of chunk 2 are untouched.

### Non-negotiable guardrails for every step

- **No behavior change** anywhere in CAT-F except the explicitly documented deltas in the
  T23 and T27 steps (each inert, a locked-decision unification, or the amended T23
  code-to-copy mapping; see Escalations).
- **DD-703-6 (revised)**: `detached: !environment.isWindows` is load-bearing for group kill via
  `process.kill(-pid)`. The T26 extraction must preserve it and the POSIX rationale comment.
- **Boundary 11**: the bootstrap flow must not regress. `MithrilBootstrap.scss` is shared by the
  bootstrap overlay (`MithrilBootstrap.tsx`) — the T3 removal is a visual no-op (see step) and
  bootstrap specs must stay green.
- **Vocabulary**: no new user-facing copy is created by CAT-F. The only copy consumed is the
  CAT-E-owned start-failure fallback ("Unable to start Mithril Sync.") plus the EXISTING
  `COPY_BY_CODE` `title` descriptors resolved for coded rejections (T23 Amendment) — F defines
  zero new strings. Raw error codes (`PARTIAL_SYNC_DISABLED`, …) are NOT user copy and must
  never render as body text. Never introduce "partial sync", percentages, or "immutable" into
  anything user-visible.
- **Comments / test names**: never cite task/finding/thread IDs (T23, CAT-F, DD-703-x, …) in
  source comments or test titles. Where context helps, write a plain rationale comment matching
  the surrounding comment density. (IDs are fine in this plan doc.)
- **Prettier 2.1.2**: do not reformat beyond edited hunks. In jest asserts on spawn-style calls
  use `expect.objectContaining`, never a literal `('string', { … })` second argument.
- **Git**: never commit, never push, never run `gh`, never `git add -A`/`-u`, never stage or
  modify `.gitignore` or anything under `.agent/skills/` (unrelated in-flight user changes).

---

# 1. Renderer cleanups

## 1.1 T23 — shared error-message helper (`source/renderer/app/utils/mithrilErrorMessage.ts`, NEW) — AMENDED 2026-07-03

> **Amendment note (see also the top-level Amendment section).** The originally approved T23
> made `getMithrilStartErrorMessage(error, intl)` PREFER the concrete extracted rejection
> message. That conflicted with landed CAT-E behavior: the section's `startFromConfirmation`
> now formats `partialSyncStartFailureMessage` unconditionally, and
> `partialSyncErrorCopy.ts` carries the invariant comment "the renderer must never surface the
> raw rejection message". Post-T16, start rejections carry stable codes as their `message`
> (e.g. `throw new Error(PARTIAL_SYNC_DISABLED_CODE)` in `chainStorageCoordinator.ts`; the IPC
> lib re-rejects the caught Error object — `event.sender.send(this._responseChannel, false,
> error)` in `source/common/ipc/lib/IpcChannel.ts` — so the code string arrives verbatim in
> `error.message` on the renderer side). Preferring the concrete message would render raw codes
> like `PARTIAL_SYNC_DISABLED` as user copy — banned. **Binding decision (do not relitigate):**
> a known code maps to its `COPY_BY_CODE` copy; everything else gets the shared
> `partialSyncStartFailureMessage`; raw rejection prose or code text never renders.

Three sites handle an unknown start rejection with divergent logic today:

- `SyncingConnectingMithrilPrompt.handleStart` renders `error.message` verbatim (post-T16 that
  is a raw code — banned) with the hardcoded non-intl fallback `'Unable to start Mithril sync.'`;
- `MithrilPartialSyncSection.startFromConfirmation` (CAT-E, landed) formats
  `partialSyncStartFailureMessage` unconditionally — correct fallback, but a rejection carrying
  a known code loses its specific copy;
- the store's `toStartError` wraps non-Error `{ message }` shapes and falls back to the
  hardcoded `'Unable to start Mithril partial sync.'` (banned vocabulary).

One helper absorbs all divergences: components get a localized display string (mapped code copy
or shared fallback — NEVER the raw message); the store keeps throwing an `Error` whose message
is the extracted text (typically a stable code) or `''`.

**Chosen mechanics (no implementer decisions):**

- Codes travel as the rejection's `message` string (see Amendment note). Detection = the
  trimmed extracted message is a key of `COPY_BY_CODE` in `partialSyncErrorCopy.ts`.
- The copy-map field rendered at these one-line inline seams is **`title`**, consistently for
  all codes (e.g. `PARTIAL_SYNC_DISABLED` → EN `Mithril Sync failed`). `hint` is never used
  here: the hints reference overlay-only recovery affordances ("available recovery actions",
  "Choose how to continue below") that do not exist at the start seams, and the seams render a
  single line.
- `COPY_BY_CODE` stays private to `partialSyncErrorCopy.ts`; F adds ONE additive lookup export
  there (Step 2) — no descriptor, map-entry, or copy changes (copy stays CAT-E/C/D-owned) and no
  second fallback string.
- The store cannot format intl (no intl context in MobX stores), so the store-facing
  `toMithrilStartError` produces an `Error` whose message is the extracted text or `''`; the
  catching component routes it through `getMithrilStartErrorMessage`, which maps a known code
  and otherwise (empty message, prose, anything unrecognized) formats the shared fallback.

### Step 1 — preflight (all three checks verified against the tree on 2026-07-03; re-verify, escalate on mismatch)

1a. `partialSyncErrorCopy.ts` ends with the fallback export (locate by quoted content):

```ts
// Single shared fallback for a rejected Mithril Sync start request; the
// renderer must never surface the raw rejection message.
export const partialSyncStartFailureMessage: MessageDescriptor =
  messages.partialSyncStartFailure;
```

`messages.partialSyncStartFailure` (in `MithrilBootstrap.messages.ts`) has defaultMessage
`!!!Unable to start Mithril Sync.` and the `en-US.json` value for
`loading.mithrilPartialSync.error.startFailure` is exactly `Unable to start Mithril Sync.`.
If any of this is absent or different → Escalation 1. The two-line comment above the export is
the landed invariant this whole step encodes — do NOT remove or reword it; the helper restates
it (Step 3).

1b. The same file defines the private code map (locate by
`const COPY_BY_CODE: Record<MithrilPartialSyncErrorCode, PartialSyncErrorCopy> =`) whose
`FAILED` entry's `title` is `messages.partialSyncFailedTitle` (`en-US.json`
`loading.mithrilPartialSync.error.failed.title` = `Mithril Sync failed`). If the map or that
entry is missing → Escalation 1.

1c. The landed section spec test
`'keeps confirmation open and shows the localized start-failure fallback'`
(`MithrilPartialSyncSection.spec.tsx`) stubs the rejection as
`new Error('Mithril partial sync is disabled by launcher configuration.')` — prose, NOT a
member of the `MithrilPartialSyncErrorCode` union — and asserts the fallback renders while the
prose does not. Under the helper this input still resolves to the fallback, byte-identical, so
this test MUST pass UNMODIFIED. If the stub's message now equals a known code, STOP and
escalate (the byte-identical guarantee would no longer hold).

### Step 2 — additive lookup export in `partialSyncErrorCopy.ts`

File: `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts`.
Append directly below the `partialSyncStartFailureMessage` export quoted in Step 1a (end of
file), separated by one blank line:

```ts
// Start rejections carry stable backend codes as their message. Expose a
// code-keyed lookup so the start-failure seams can reuse this copy map
// without the map itself leaking out of this module.
export const resolvePartialSyncErrorCopyByCode = (
  code: string
): PartialSyncErrorCopy | undefined =>
  COPY_BY_CODE[code as MithrilPartialSyncErrorCode];
```

(`PartialSyncErrorCopy` and the type-only `MithrilPartialSyncErrorCode` import already exist in
this file — no import changes.) NOTHING else in this file changes: descriptors, map entries,
`resolvePartialSyncErrorCopy`, the fallback export, and every comment stay byte-identical. No
edit to `partialSyncErrorCopy.spec.ts` — the helper spec (Step 7) pins the mapping through this
export.

### Step 3 — create `source/renderer/app/utils/mithrilErrorMessage.ts` (confirmed absent today)

Full file content:

```ts
import type { Intl } from '../types/i18nTypes';
import {
  partialSyncStartFailureMessage,
  resolvePartialSyncErrorCopyByCode,
} from '../components/loading/mithril-bootstrap/partialSyncErrorCopy';

// Single extraction point for messages carried by unknown thrown values, so
// the Mithril start-failure surfaces cannot drift apart again. Returns null
// when the value carries no usable message.
export const extractMithrilErrorMessage = (error: unknown): string | null => {
  if (error instanceof Error) {
    return error.message.trim() ? error.message : null;
  }
  if (
    error &&
    typeof error === 'object' &&
    typeof (error as { message?: unknown }).message === 'string' &&
    (error as { message: string }).message.trim()
  ) {
    return (error as { message: string }).message;
  }
  return null;
};

// For components: the user-facing line for a rejected start request. Start
// rejections carry stable backend codes, not user copy — a known code resolves
// to its localized copy and anything else gets the shared fallback, so the raw
// rejection message never surfaces to the user.
export const getMithrilStartErrorMessage = (
  error: unknown,
  intl: Intl
): string => {
  const message = extractMithrilErrorMessage(error);
  const copy = message
    ? resolvePartialSyncErrorCopyByCode(message.trim())
    : undefined;
  return intl.formatMessage(copy ? copy.title : partialSyncStartFailureMessage);
};

// For the store, which throws instead of rendering (stores have no intl
// context): a real Error passes through unchanged; anything else is wrapped
// around the extracted message. An empty message means "no usable message" —
// the catching component resolves it to the shared fallback.
export const toMithrilStartError = (error: unknown): Error =>
  error instanceof Error
    ? error
    : new Error(extractMithrilErrorMessage(error) ?? '');
```

(`copy.title` is a `MessageDescriptor`; `Intl.formatMessage` takes `ReactIntlMessage` — this
assignment compiles under this repo's `strict: false` tsconfig, matching how the approved plan
already passed `partialSyncStartFailureMessage`. If `yarn compile` rejects it, the fix is to
type the second parameter as react-intl's `MessageDescriptor` — do NOT weaken the descriptors.)

### Step 4 — store: delete the local `toStartError`, use the shared wrapper

File: `source/renderer/app/stores/MithrilPartialSyncStore.ts`

4a. Delete this entire top-level function (locate by `const toStartError = (error: unknown): Error => {`):

```ts
const toStartError = (error: unknown): Error => {
  if (error instanceof Error) {
    return error;
  }

  if (
    error &&
    typeof error === 'object' &&
    typeof (error as { message?: unknown }).message === 'string'
  ) {
    return new Error((error as { message: string }).message);
  }

  return new Error('Unable to start Mithril partial sync.');
};
```

4b. In `startPartialSync`, locate `throw toStartError(startError);` and replace with:

```ts
    throw toMithrilStartError(startError);
```

4c. Add the import directly below the existing line `import { logger } from '../utils/logging';`:

```ts
import { toMithrilStartError } from '../utils/mithrilErrorMessage';
```

### Step 5 — diagnostics section: route the landed fallback through the helper

File: `source/renderer/app/components/status/MithrilPartialSyncSection.tsx`

5a. In `startFromConfirmation`, locate the landed CAT-E `setState` inside the `catch` (verified
verbatim in the working tree):

```ts
      this.setState({
        startError: this.context.intl.formatMessage(
          partialSyncStartFailureMessage
        ),
      });
```

Replace with:

```ts
      this.setState({
        startError: getMithrilStartErrorMessage(error, this.context.intl),
      });
```

5b. Add the import directly below `import { logger } from '../../utils/logging';`:

```ts
import { getMithrilStartErrorMessage } from '../../utils/mithrilErrorMessage';
```

5c. Delete the now-unused import (verified: the 5a block is its only remaining use in this
file — confirm with a file-local search before deleting):

```ts
import { partialSyncStartFailureMessage } from '../loading/mithril-bootstrap/partialSyncErrorCopy';
```

For rejections without a known code the rendered output is byte-identical to landed CAT-E
behavior (both format `partialSyncStartFailureMessage`), which is why the landed spec test in
Step 1c must stay green UNMODIFIED.

### Step 6 — proactive prompt: same replacement

File: `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx`

6a. In `handleStart`, locate (verified verbatim in the working tree):

```ts
      this.setState({
        isStarting: false,
        startError:
          error instanceof Error
            ? error.message
            : 'Unable to start Mithril sync.',
      });
```

Replace with:

```ts
      this.setState({
        isStarting: false,
        startError: getMithrilStartErrorMessage(error, this.context.intl),
      });
```

6b. Add the import directly below `import { logger } from '../../../utils/logging';`:

```ts
import { getMithrilStartErrorMessage } from '../../../utils/mithrilErrorMessage';
```

### Step 7 — new helper spec

Create `source/renderer/app/utils/mithrilErrorMessage.spec.ts` (sibling precedent:
`mithrilBehindness.spec.ts`):

```ts
import type { Intl } from '../types/i18nTypes';
import {
  extractMithrilErrorMessage,
  getMithrilStartErrorMessage,
  toMithrilStartError,
} from './mithrilErrorMessage';

const makeIntl = () => {
  const formatMessage = jest.fn(
    (message: { id: string }) => `formatted:${message.id}`
  );
  return { intl: { formatMessage } as unknown as Intl, formatMessage };
};

describe('extractMithrilErrorMessage', () => {
  it('returns the message of a real Error', () => {
    expect(extractMithrilErrorMessage(new Error('boom'))).toBe('boom');
  });

  it('returns null for an Error with an empty or blank message', () => {
    expect(extractMithrilErrorMessage(new Error(''))).toBeNull();
    expect(extractMithrilErrorMessage(new Error('   '))).toBeNull();
  });

  it('returns the message carried by a plain object rejection', () => {
    expect(extractMithrilErrorMessage({ message: 'from-ipc' })).toBe(
      'from-ipc'
    );
  });

  it('returns null for values without a usable message', () => {
    expect(extractMithrilErrorMessage(undefined)).toBeNull();
    expect(extractMithrilErrorMessage(null)).toBeNull();
    expect(extractMithrilErrorMessage('plain string')).toBeNull();
    expect(extractMithrilErrorMessage({ message: 42 })).toBeNull();
    expect(extractMithrilErrorMessage({ message: '' })).toBeNull();
  });
});

describe('getMithrilStartErrorMessage', () => {
  it('maps a rejection whose message is a known backend code to its copy', () => {
    const { intl } = makeIntl();
    expect(
      getMithrilStartErrorMessage(new Error('PARTIAL_SYNC_DISABLED'), intl)
    ).toBe('formatted:loading.mithrilPartialSync.error.failed.title');
  });

  it('maps a coded plain-object rejection the same way', () => {
    const { intl } = makeIntl();
    expect(
      getMithrilStartErrorMessage(
        { message: 'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE' },
        intl
      )
    ).toBe(
      'formatted:loading.mithrilPartialSync.error.insufficientDiskSpace.title'
    );
  });

  it('formats the shared fallback for prose rejections instead of the raw message', () => {
    const { intl } = makeIntl();
    expect(
      getMithrilStartErrorMessage(new Error('raw backend prose'), intl)
    ).toBe('formatted:loading.mithrilPartialSync.error.startFailure');
  });

  it('formats the shared fallback when no message is extractable', () => {
    const { intl, formatMessage } = makeIntl();
    expect(getMithrilStartErrorMessage(undefined, intl)).toBe(
      'formatted:loading.mithrilPartialSync.error.startFailure'
    );
    expect(formatMessage).toHaveBeenCalledTimes(1);
  });
});

describe('toMithrilStartError', () => {
  it('returns a real Error unchanged', () => {
    const error = new Error('boom');
    expect(toMithrilStartError(error)).toBe(error);
  });

  it('wraps an object rejection preserving its message', () => {
    expect(toMithrilStartError({ message: 'from-ipc' }).message).toBe(
      'from-ipc'
    );
  });

  it('wraps unusable rejections with an empty message so the UI can apply its fallback', () => {
    expect(toMithrilStartError(42).message).toBe('');
    expect(toMithrilStartError(undefined).message).toBe('');
  });
});
```

### Step 8 — component/store spec changes

8a. `source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx` — first run the
Step 1c pre-check. Do NOT modify the landed test
`'keeps confirmation open and shows the localized start-failure fallback'`. ADD directly after
it:

```ts
  it('shows the mapped copy when the start rejection carries a known error code', async () => {
    const onStartMithrilPartialSync = jest
      .fn()
      .mockRejectedValue(new Error('PARTIAL_SYNC_DISABLED'));

    renderComponent({ onStartMithrilPartialSync });

    screen.getByRole('button', { name: 'Mithril Sync' }).click();
    screen.getByRole('button', { name: 'Start Mithril Sync' }).click();

    await waitFor(() => {
      expect(screen.getByText('Mithril Sync failed')).toBeInTheDocument();
    });
    expect(
      screen.queryByText('PARTIAL_SYNC_DISABLED')
    ).not.toBeInTheDocument();
  });
```

8b. `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx`
— REPLACE the entire existing test (locate by its title; it currently asserts the concrete
prose renders, which the helper now forbids):

```ts
  it('shows a start-error line when the start action rejects', async () => {
    const onStart = jest
      .fn()
      .mockRejectedValue(
        new Error('Mithril sync is disabled by launcher configuration.')
      );
    renderComponent({ behindByEpochs: 3, onStart });

    clickButton('Mithril Sync (fast)');
    clickButton('Start now');

    await waitFor(() => {
      expect(logger.warn).toHaveBeenCalledWith(
        'SyncingConnectingMithrilPrompt: Mithril sync start rejected after confirmation',
        { error: expect.any(Error) }
      );
    });

    expect(
      screen.getByText('Mithril sync is disabled by launcher configuration.')
    ).toBeInTheDocument();
  });
```

with these TWO tests (coded → mapped copy; un-coded → shared fallback, prose never renders):

```ts
  it('shows the mapped copy when the start rejection carries a known error code', async () => {
    const onStart = jest
      .fn()
      .mockRejectedValue(new Error('PARTIAL_SYNC_DISABLED'));
    renderComponent({ behindByEpochs: 3, onStart });

    clickButton('Mithril Sync (fast)');
    clickButton('Start now');

    await waitFor(() => {
      expect(logger.warn).toHaveBeenCalledWith(
        'SyncingConnectingMithrilPrompt: Mithril sync start rejected after confirmation',
        { error: expect.any(Error) }
      );
    });

    expect(screen.getByText('Mithril Sync failed')).toBeInTheDocument();
    expect(
      screen.queryByText('PARTIAL_SYNC_DISABLED')
    ).not.toBeInTheDocument();
  });

  it('shows the shared fallback when the start rejection carries no known code', async () => {
    const onStart = jest
      .fn()
      .mockRejectedValue(
        new Error('Mithril sync is disabled by launcher configuration.')
      );
    renderComponent({ behindByEpochs: 3, onStart });

    clickButton('Mithril Sync (fast)');
    clickButton('Start now');

    await waitFor(() => {
      expect(
        screen.getByText('Unable to start Mithril Sync.')
      ).toBeInTheDocument();
    });
    expect(
      screen.queryByText('Mithril sync is disabled by launcher configuration.')
    ).not.toBeInTheDocument();
  });
```

(Both specs already render inside `IntlProvider` with the full `en-US.json`, so the literals
`Mithril Sync failed` / `Unable to start Mithril Sync.` are the locale values for
`loading.mithrilPartialSync.error.failed.title` / `….error.startFailure` — verified present. If
either locale value differs at implementation time, use the landed value verbatim and note it
in the log.)

8c. `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` — add after the existing test
`'rethrows the original start failure while status is still pending'` (reuse its exact
`mockStatusRequest` payload shape):

```ts
  it('wraps a non-Error start rejection that carries a message string', async () => {
    const store = setupStore();
    mockStartRequest.mockRejectedValue({ message: 'start rejected over IPC' });
    mockStatusRequest.mockResolvedValue({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.startPartialSync()).rejects.toThrow(
      'start rejected over IPC'
    );
  });

  it('throws an empty-message error for unusable rejections so the UI applies its fallback', async () => {
    const store = setupStore();
    mockStartRequest.mockRejectedValue('not-an-error');
    mockStatusRequest.mockResolvedValue({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.startPartialSync()).rejects.toMatchObject({
      message: '',
    });
  });
```

**Documented behavior deltas (intentional, per the approved T23 item as AMENDED):** (i) a start
rejection whose message is a known `MithrilPartialSyncErrorCode` now renders that code's mapped
`title` copy at BOTH seams (previously: raw code text at the prompt, generic fallback at the
diagnostics section); (ii) un-coded rejections render the shared fallback everywhere and raw
rejection prose never renders (previously the prompt rendered `error.message` verbatim); (iii)
all three sites now extract from the non-Error `{ message }` shape (previously store-only);
(iv) the store's hardcoded `'Unable to start Mithril partial sync.'` (banned vocabulary) and
the prompt's hardcoded non-intl `'Unable to start Mithril sync.'` can no longer reach the UI.
No other consumer reads the thrown Error's message: `startPartialSync` rejections are caught
only by these two components (App.tsx `onRetry` wiring is handled by CAT-B's T15 work, which F
must not touch).

## 1.2 T24 — `isMithrilPartialSyncBlockingNodeStart` delegates to the working-status predicate

File: `source/common/types/mithril-partial-sync.types.ts`

Step 1. **Pre-check**: confirm the inline array in `isMithrilPartialSyncBlockingNodeStart` is
still element-for-element identical to `MITHRIL_PARTIAL_SYNC_WORKING_STATUSES` (today both are
exactly: `stopping-node, cancelling, preparing, downloading, verifying, converting, installing,
finalizing, starting-node`). If an earlier section changed either list, STOP and escalate.

Step 2. Locate:

```ts
export const isMithrilPartialSyncBlockingNodeStart = (
  status: MithrilPartialSyncStatus
): boolean =>
  [
    'stopping-node',
    'cancelling',
    'preparing',
    'downloading',
    'verifying',
    'converting',
    'installing',
    'finalizing',
    'starting-node',
  ].includes(status);
```

Replace with:

```ts
export const isMithrilPartialSyncBlockingNodeStart = (
  status: MithrilPartialSyncStatus
): boolean =>
  // Node start is blocked exactly while a Mithril run is doing work; the
  // working-status list is the single source of truth for that window.
  isMithrilPartialSyncWorkingStatus(status);
```

Step 3. Test update — `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`,
inside the existing `describe('isMithrilPartialSyncBlockingNodeStart', …)` block (which already
asserts `('cancelling')` is `true`), add a second `it`:

```ts
  it('matches the working-status window exactly', () => {
    expect(isMithrilPartialSyncBlockingNodeStart('downloading')).toBe(true);
    expect(isMithrilPartialSyncBlockingNodeStart('starting-node')).toBe(true);
    expect(isMithrilPartialSyncBlockingNodeStart('completed')).toBe(false);
    expect(isMithrilPartialSyncBlockingNodeStart('failed')).toBe(false);
    expect(isMithrilPartialSyncBlockingNodeStart('idle')).toBe(false);
  });
```

Main-process consumers (`MithrilStartupGate.ts:488` area, `MithrilController.ts:209` area) call
the same function and are untouched — behavior identical.

## 1.3 T25 — overlay `PROGRESS_STATUSES` hand-copy replaced by the shared predicate

File: `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`
(CAT-C's T13 edited this file earlier — locate strictly by content.)

Step 1. Delete the whole constant (locate by `const PROGRESS_STATUSES: MithrilPartialSyncStatus[] = [`):

```ts
const PROGRESS_STATUSES: MithrilPartialSyncStatus[] = [
  'stopping-node',
  'cancelling',
  'preparing',
  'downloading',
  'verifying',
  'converting',
  'installing',
  'finalizing',
  'starting-node',
  'completed',
];
```

Step 2. Locate the sole use:

```ts
  const isProgressStatus = PROGRESS_STATUSES.includes(status);
```

Replace with:

```ts
  const isProgressStatus =
    isMithrilPartialSyncWorkingStatus(status) || status === 'completed';
```

Step 3. Add a value import directly below the existing `import type { … } from
'../../../../../common/types/mithril-partial-sync.types';` block (repo convention keeps `import
type` and value imports as separate statements — see `MithrilStepIndicator.tsx`):

```ts
import { isMithrilPartialSyncWorkingStatus } from '../../../../../common/types/mithril-partial-sync.types';
```

The replacement set (working ∪ `completed`) is exactly the deleted array — zero behavior change.
No new tests; the existing overlay spec (progress vs error routing per status) must stay green.

## 1.4 T29/T30 — delete the dead `behindByImmutables` and `elapsedSeconds` observables

File: `source/renderer/app/stores/MithrilPartialSyncStore.ts`. Verified consumers: none outside
the store and its spec (grep across `source/` finds no renderer read of either observable; App.tsx
forwards `startedAt`, not `elapsedSeconds`). Survivors that MUST NOT be touched: epochs-only
display (`certifiedEpoch`, `isSignificantlyBehind`) and the `startedAt` derivation, which still
reads `update.transferProgress.elapsedSeconds` into the local `backendElapsed`.

Step 1. Delete the declaration line (locate by exact text):

```ts
  @observable elapsedSeconds: number | undefined = undefined;
```

Step 2. Delete the write line inside `_updateStatus` (locate between the `filesTotal` and
`ancillaryBytesDownloaded` assignments):

```ts
    this.elapsedSeconds = update.transferProgress.elapsedSeconds;
```

Do NOT touch the nearby `const backendElapsed = update.transferProgress.elapsedSeconds;` block —
that feeds `startedAt` and survives.

Step 3. Delete the declaration line:

```ts
  @observable behindByImmutables: number | undefined = undefined;
```

(Leave the comment block that follows it — it documents `certifiedEpoch`, which stays.)

Step 4. Delete the write line inside `_applyAvailability`:

```ts
    this.behindByImmutables = availability.behindByImmutables;
```

(Keep `isPartialSyncEnabled`, `isSignificantlyBehind`, and `certifiedEpoch` assignments and their
comments.)

Step 5. Spec updates — `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`. Delete
exactly these four assertion lines (payload *inputs* like `elapsedSeconds: 30` /
`behindByImmutables: 42` stay — the type still carries the fields and the `startedAt` tests
consume `elapsedSeconds` inputs):

- `expect(store.elapsedSeconds).toBe(30);` (in `'syncs cached backend status during setup and subscribes for pushed updates'`)
- `expect(store.elapsedSeconds).toBeUndefined();` (in `'replaces recovery actions and clears explicit fields on status update'`)
- `expect(store.behindByImmutables).toBe(42);` (in `'consumes the availability read model with a one-shot query during setup'`)
- `expect(store.behindByImmutables).toBeUndefined();` (in `'keeps partial sync hidden by default until the first availability response lands'`)

Step 6. `source/renderer/app/App.spec.tsx` — the plain-object store mocks carry the now-dead key.
Delete these two lines (mock hygiene; App.tsx never read it):

- `      elapsedSeconds: undefined,` (in `makeStores()`'s `mithrilPartialSync` mock)
- `        elapsedSeconds: 65,` (in the `'mounts the partial sync overlay and forwards all recovery callbacks'` override)

`MithrilPartialSyncAvailability.behindByImmutables` and
`MithrilPartialSyncTransferProgress.elapsedSeconds` in
`source/common/types/mithril-partial-sync.types.ts` are STILL produced/consumed by the main
process and bootstrap store — do not remove them from the common types.

## 1.5 T3 — remove the backdrop blur mixin from `MithrilBootstrap.scss` (+ typings regen)

File: `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss`

Rationale (invariant, boundary 11): `.backdrop` is opaque (`background:
var(--theme-mithril-overlay-backdrop-start); opacity: 1;` — shipped deliberately in 702), so
`backdrop-filter: blur(5px)` from the mixin has no visible effect behind a fully opaque layer.
The file is shared with the bootstrap overlay (`MithrilBootstrap.tsx` imports the same
stylesheet), so this must remain a strict no-op removal: touch ONLY the two lines below, nothing
else in the file. Class names do not change, so the bootstrap-variant completed frame stays
byte-identical.

Step 1. Delete the include line inside the `.backdrop` block (note the mixin name's historic
misspelling — copy exactly):

```scss
  @include overlay-backrop;
```

Step 2. Delete the now-unused import at the top of the file (the mixin file defines only this one
mixin, and this was its only use in this stylesheet):

```scss
@import '../../../themes/mixins/overlay-backdrop';
```

Step 3. Regenerate the typings for just this file (Node v24: the full `precompile` glob crashes —
regen only the touched file):

```bash
node_modules/.bin/typed-scss-modules source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss
```

The regenerated `MithrilBootstrap.scss.d.ts` should be content-identical (exports `backdrop`,
`card`, `component`, `content`, …). If it churns beyond whitespace, STOP and escalate. Do not
touch the other `@include overlay-backrop;` sites in the repo (AlertsOverlay, IncidentOverlay,
DaedalusDiagnosticsDialog, etc.) — they are translucent overlays where the blur is live.

---

# 2. Main-process cleanups

## 2.1 T26 — extract the duplicated spawn pipeline in `mithrilCommandRunner.ts`

File: `source/main/mithril/mithrilCommandRunner.ts`. `runBinary` (spawn block starting at
`const child = spawn(binaryPath, args, {`) and `runCommand` (`const child = spawn(binaryPath,
commandArgs, {`) duplicate the entire pipeline: binary-path resolution, `ensureDirectoryExists`,
the `[mithril] Spawning:` info log, the spawn options, the spawned/exit/close logging, stdout/
stderr accumulation, and error/close settlement. Extract ONE module-private helper; both exported
functions keep their signatures and observable behavior byte-for-byte.

Verified parameter differences the helper absorbs:

| | `runBinary` | `runCommand` |
|---|---|---|
| binary | caller-supplied `binaryName` | hardcoded `mithril-client` |
| args | as given | prefixed `['--origin-tag', 'DAEDALUS', ...args]` |
| env | `normalizeSpawnEnv(process.env)` | `normalizeSpawnEnv(await buildMithrilEnv(requireKeys))` |
| stdin | writes `options.stdinInput` when defined | never writes stdin (do NOT forward `stdinInput`) |

Step 1. Insert the helper between `attachLogStream` and `runBinary`. The 4-line POSIX
process-group rationale comment currently duplicated above both `spawn(` calls MUST be preserved
verbatim (it now appears once, above the single spawn site). **Invariant (DD-703-6): `detached:
!environment.isWindows` stays exactly as written — detachment is load-bearing for group kill via
`process.kill(-pid)`.**

```ts
type SpawnMithrilChildParams = {
  binaryName: string;
  args: string[];
  workDir: string;
  env: NodeJS.ProcessEnv;
  logStream: WriteStream;
  onStdout?: (chunk: string) => void;
  onStderr?: (chunk: string) => void;
  stdinInput?: string;
  callbacks?: RunCommandCallbacks;
};

// Shared spawn pipeline for runBinary/runCommand: binary resolution, spawn
// logging, output accumulation, and exit settlement are identical for every
// Mithril child; only args, env, and stdin handling differ per caller.
function spawnMithrilChild({
  binaryName,
  args,
  workDir,
  env,
  logStream,
  onStdout,
  onStderr,
  stdinInput,
  callbacks,
}: SpawnMithrilChildParams): Promise<RunCommandResult> {
  const pathKey = getWindowsPathKey(env);

  const resolvedBinaryName = environment.isWindows
    ? `${binaryName}.exe`
    : binaryName;
  const installDir = process.env.DAEDALUS_INSTALL_DIRECTORY;
  const binaryPath = installDir
    ? path.join(installDir, resolvedBinaryName)
    : resolvedBinaryName;

  ensureDirectoryExists(workDir);

  logger.info(`[mithril] Spawning: ${binaryPath} ${args.join(' ')}`, {
    binaryPath,
    installDir: installDir || '(not set)',
    cwd: workDir,
    pathEnv: env[pathKey] || '(not set)',
    pathKey,
  });

  return new Promise((resolve, reject) => {
    // Detach on POSIX so the child leads its own process group and
    // killProcessTree's process.kill(-pid) reaps its whole tree. Gated off Windows
    // (documented launcher breakage — see CardanoSelfnodeLauncher.ts); stdio stays
    // piped and the child is deliberately NOT unref()'d.
    const child = spawn(binaryPath, args, {
      cwd: workDir,
      env,
      detached: !environment.isWindows,
    });

    if (callbacks?.onProcess) callbacks.onProcess(child);
    attachLogStream(child, logStream);

    logger.info('[mithril] child spawned', { pid: child.pid });
    child.on('exit', (code, signal) =>
      logger.info('[mithril] child exited', {
        pid: child.pid,
        code,
        signal,
        killed: child.killed,
      })
    );

    if (stdinInput !== undefined) {
      child.stdin?.write(stdinInput);
      child.stdin?.end();
    }

    let stdout = '';
    let stderr = '';

    if (child.stdout) {
      child.stdout.on('data', (chunk) => {
        const text = chunk.toString();
        stdout += text;
        if (onStdout) onStdout(text);
      });
    }

    if (child.stderr) {
      child.stderr.on('data', (chunk) => {
        const text = chunk.toString();
        stderr += text;
        if (onStderr) onStderr(text);
      });
    }

    child.on('error', (error) => {
      if (callbacks?.onProcess) callbacks.onProcess(null);
      logStream.end();
      reject(error);
    });

    child.on('close', (exitCode) => {
      if (callbacks?.onProcess) callbacks.onProcess(null);
      logStream.end();
      logger.info('[mithril] child closed', { pid: child.pid, exitCode });
      resolve({ stdout, stderr, exitCode });
    });
  });
}
```

Step 2. Replace the ENTIRE body of `runBinary` (from `const { onStdout, onStderr, logFileName } =
options;` through the closing of its returned Promise) with:

```ts
export async function runBinary(
  binaryName: string,
  args: string[],
  workDir: string,
  options: RunCommandOptions = {},
  callbacks?: RunCommandCallbacks
): Promise<RunCommandResult> {
  const { onStdout, onStderr, stdinInput, logFileName } = options;
  const logStream = openLogStream(logFileName);
  if (callbacks?.onLogStream) callbacks.onLogStream(logStream);

  const env = normalizeSpawnEnv(process.env);

  return spawnMithrilChild({
    binaryName,
    args,
    workDir,
    env,
    logStream,
    onStdout,
    onStderr,
    stdinInput,
    callbacks,
  });
}
```

Step 3. Replace the ENTIRE body of `runCommand` (from `const { onStdout, onStderr, requireKeys =
true, logFileName } = options;` through the closing of its returned Promise) with — note
`stdinInput` is deliberately NOT forwarded (current `runCommand` ignores it; preserving that is
part of the no-behavior-change invariant), and the `// Resolve mithril-client binary path`
comment goes away with the code it described:

```ts
export async function runCommand(
  args: string[],
  workDir: string,
  options: RunCommandOptions = {},
  callbacks?: RunCommandCallbacks
): Promise<RunCommandResult> {
  const { onStdout, onStderr, requireKeys = true, logFileName } = options;
  const logStream = openLogStream(logFileName);
  if (callbacks?.onLogStream) callbacks.onLogStream(logStream);

  const env = normalizeSpawnEnv(await buildMithrilEnv(requireKeys));

  return spawnMithrilChild({
    binaryName: 'mithril-client',
    args: ['--origin-tag', 'DAEDALUS', ...args],
    workDir,
    env,
    logStream,
    onStdout,
    onStderr,
    callbacks,
  });
}
```

Equivalence notes for the implementer (verify, do not re-derive): the helper's `.exe` suffix +
`DAEDALUS_INSTALL_DIRECTORY` join reproduces both old resolutions exactly (`'mithril-client'` →
`mithril-client.exe` on Windows); log-stream opening and `onLogStream` still fire BEFORE the
`buildMithrilEnv` await in `runCommand`, as today; statement order inside the Promise matches the
old `runBinary` body, and with `stdinInput === undefined` it matches the old `runCommand` body.

Step 4. Tests: NO changes to `source/main/mithril/mithrilCommandRunner.spec.ts`. The existing
suite already pins the contract — both `'spawns detached on POSIX so the child leads its own
process group'` tests, both Windows non-detached tests, `'prepends the Daedalus origin tag before
command args'`, binary-path resolution, and un-truncated stdout accumulation — and must pass
unmodified. Do not reformat the spec (known prettier 2.1.2 oscillation on
`toHaveBeenCalledWith('str', { obj })` shapes lives in this pair of files).

## 2.2 T27 — collapse the snapshot list/show spawn+parse pipeline in `MithrilPartialSyncService.ts`

File: `source/main/mithril/MithrilPartialSyncService.ts`. Four methods duplicate the
`_runCommand(['cardano-db','snapshot',…,'--json'], { requireKeys: false … })` + `_safeJsonParse`
pipeline: `listSnapshots`, `showSnapshot`, `_listSnapshotsRaw`, `_showSnapshotRaw`. Locked
mechanic (approved plan item 26): the public methods delegate to the raw pipeline —
`listSnapshots` maps `_listSnapshotsRaw` results through the normalizer (each
`ResolvedLatestSnapshot` already carries its normalized `snapshot: MithrilSnapshotItem`);
`showSnapshot` shares `_showSnapshotRaw`.

Step 1. Locate and replace the whole `listSnapshots` method:

```ts
  async listSnapshots(): Promise<Array<MithrilSnapshotItem>> {
    const { stdout } = await this._runCommand(
      ['cardano-db', 'snapshot', 'list', '--json'],
      { requireKeys: false }
    );
    const parsed = this._safeJsonParse(stdout);
    if (!Array.isArray(parsed)) return [];
    return parsed.map((item) => normalizeSnapshotItem(item));
  }
```

with:

```ts
  async listSnapshots(): Promise<Array<MithrilSnapshotItem>> {
    const snapshots = await this._listSnapshotsRaw();
    return snapshots.map(({ snapshot }) => snapshot);
  }
```

Step 2. Locate and replace the whole `showSnapshot` method:

```ts
  async showSnapshot(digest: string): Promise<MithrilSnapshotItem | null> {
    if (!digest || !digest.trim()) return null;
    const { stdout } = await this._runCommand(
      ['cardano-db', 'snapshot', 'show', digest, '--json'],
      { requireKeys: false }
    );
    const parsed = this._safeJsonParse(stdout);
    if (!parsed || typeof parsed !== 'object') return null;
    return normalizeSnapshotItem(parsed);
  }
```

with:

```ts
  async showSnapshot(digest: string): Promise<MithrilSnapshotItem | null> {
    const resolved = await this._showSnapshotRaw(digest);
    return resolved ? resolved.snapshot : null;
  }
```

(`_showSnapshotRaw` already carries the `if (!digest || !digest.trim()) return null;` guard and
the parse-shape guard.)

Step 3. `_listSnapshotsRaw` and `_showSnapshotRaw` stay EXACTLY as they are (they are now the
single pipeline). If `normalizeSnapshotItem` is no longer referenced anywhere else in this file
after Steps 1–2 (CAT-E's T16 edits may have changed usage — check with a file-local search),
remove it from the `mithrilSnapshotMetadata` import list; otherwise leave the import as is.

Step 4. Spec addition — `source/main/mithril/MithrilPartialSyncService.spec.ts`, add directly
after the existing test `'uses a distinct partial sync log file when running Mithril commands'`
(same `runCommandMock` fixture):

```ts
  it('serves snapshot list and show reads from the shared metadata pipeline', async () => {
    const service = new MithrilPartialSyncService();

    runCommandMock.mockResolvedValueOnce({
      stdout: JSON.stringify([
        {
          digest: 'list-digest',
          created_at: '2026-07-01T00:00:00Z',
          size: 10,
          beacon: { immutable_file_number: 1200, epoch: 320 },
        },
      ]),
      stderr: '',
      exitCode: 0,
    });

    await expect(service.listSnapshots()).resolves.toEqual([
      expect.objectContaining({ digest: 'list-digest', size: 10 }),
    ]);

    runCommandMock.mockResolvedValueOnce({
      stdout: JSON.stringify({
        digest: 'show-digest',
        created_at: '2026-07-02T00:00:00Z',
        size: 20,
        beacon: { immutable_file_number: 1300, epoch: 321 },
      }),
      stderr: '',
      exitCode: 0,
    });

    await expect(service.showSnapshot('show-digest')).resolves.toEqual(
      expect.objectContaining({ digest: 'show-digest', size: 20 })
    );

    await expect(service.showSnapshot('   ')).resolves.toBeNull();
  });
```

**Documented behavior delta (inert — verified zero production callers):**
`MithrilPartialSyncService.listSnapshots` now drops list items lacking a certified immutable
number (warn+skip in the raw reduce), and `showSnapshot` throws the pipeline's stage error for a
parsed object without one, where both previously returned metadata-poor items. Verified today:
the only production `listSnapshots()` route (`mithrilBootstrapChannel` → `MithrilController` →
`chainStorageCoordinator`) resolves to `MithrilBootstrapService.listSnapshots`, NOT this service;
`MithrilPartialSyncService.showSnapshot` has no callers at all; the only other caller of the
service's `listSnapshots` is its own spec. See Escalations if this no longer holds when
implementing.

---

# Verification

Environment prep (Node v24 — known tooling constraints, not regressions):

- After the T3 scss edit, regenerate typings for the touched file only:
  `node_modules/.bin/typed-scss-modules source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss`
- If focused jest fails inside the scss transform for component specs, apply the gitignored
  `identity-obj-proxy` jest sidecar `--config` used by prior tasks in this sprint. Never stage it.

Commands (narrow, in this order):

```bash
yarn test:jest --testPathPattern "renderer/app/utils/mithrilErrorMessage"
yarn test:jest --testPathPattern "partialSyncErrorCopy"
yarn test:jest --testPathPattern "MithrilPartialSyncStore"
yarn test:jest --testPathPattern "MithrilPartialSyncSection|SyncingConnectingMithrilPrompt|MithrilPartialSyncConfirmation"
yarn test:jest --testPathPattern "MithrilPartialSyncOverlay"
yarn test:jest --testPathPattern "mithrilCommandRunner"
yarn test:jest --testPathPattern "MithrilPartialSyncService"
# Boundary 11 — bootstrap flow must not regress:
yarn test:jest --testPathPattern "MithrilBootstrap"
yarn test:jest --testPathPattern "App.spec"
yarn compile
```

Lint/format only the touched files, e.g.
`node_modules/.bin/eslint <touched .ts/.tsx files>` and
`node_modules/.bin/prettier --check <touched files>` — never repo-wide reformatting.

# i18n

**CAT-F introduces ZERO new i18n messages.** The only intl dependencies are consumption of the
CAT-E-owned start-failure fallback descriptor (EN: `Unable to start Mithril Sync.`; JA owned and
landed by CAT-E per the JA copy table) and, per the T23 Amendment, resolution of EXISTING
`COPY_BY_CODE` `title` descriptors (EN/JA already landed — e.g.
`loading.mithrilPartialSync.error.failed.title`: EN `Mithril Sync failed`, JA
`Mithril同期に失敗しました`). No edits to `en-US.json` / `ja-JP.json` / `defaultMessages` in this
section; no `yarn i18n:manage` run needed. If the fallback descriptor or the code map is
missing → Escalation 1.

# Files touched

- `source/renderer/app/utils/mithrilErrorMessage.ts` (NEW)
- `source/renderer/app/utils/mithrilErrorMessage.spec.ts` (NEW)
- `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts` (T23
  Amendment — additive `resolvePartialSyncErrorCopyByCode` export ONLY; all copy, descriptors,
  and comments stay CAT-E-owned and byte-identical)
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` (T23, T29, T30)
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` (T23, T29, T30)
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` (T23)
- `source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx` (T23)
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx` (T23)
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx` (T23)
- `source/common/types/mithril-partial-sync.types.ts` (T24)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` (T25)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` (T24 test)
- `source/renderer/app/App.spec.tsx` (T30 mock hygiene)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss` (T3)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss.d.ts` (T3 regen; expected content-identical)
- `source/main/mithril/mithrilCommandRunner.ts` (T26)
- `source/main/mithril/MithrilPartialSyncService.ts` (T27)
- `source/main/mithril/MithrilPartialSyncService.spec.ts` (T27 test)

# Out of scope (owned elsewhere — do not touch)

- The start-failure fallback intl message itself and all `COPY_BY_CODE` descriptors/copy (ids,
  EN, JA, locale files) — CAT-E (T22) / CAT-C / CAT-D. F only ADDS the
  `resolvePartialSyncErrorCopyByCode` lookup export (T23 Amendment, Step 2) — it must not alter
  any descriptor, map entry, or comment in `partialSyncErrorCopy.ts`.
- The finalize-failed copy key — CAT-C. The insufficient-disk-space copy key (T18) — CAT-D.
- All remaining copy work, `PARTIAL_SYNC_DISABLED` / other error-code union additions, and the
  `chainStorageCoordinator.ts` disabled-prose throw conversion — CAT-E (T16); the
  `MithrilPartialSyncErrorCode` union is append-only for D/E.
- N7 (redundant `isMithrilPartialSyncEnabled` re-check in `DaedalusDiagnostics.tsx`) — CAT-B (T11).
- N4 (overlay/progress-view prop threading) and N3 — CAT-G.
- `killProcessTree` / `CardanoNode` kill semantics — T14 refuted, no code change (DD-703-6).
- App.tsx `onRetry` rejection handling — CAT-B (T15 / DD-703-11).

# Acceptance checks

| Thread | Acceptance criterion |
|---|---|
| T23 (AMENDED) | One shared helper file (`utils/mithrilErrorMessage.ts`) is the only extraction logic and all three former sites route through it; a start rejection whose message is a known `MithrilPartialSyncErrorCode` renders that code's mapped `title` copy at BOTH seams and every other rejection renders the shared fallback — raw rejection prose/codes never render (specs assert their absence); the landed CAT-E test `keeps confirmation open and shows the localized start-failure fallback` passes UNMODIFIED; `grep -rn "Unable to start Mithril" source/renderer --include="*.ts*"` hits only the CAT-E message definition (+ locale/spec literals); the store still throws an `Error`, components still set a string; the only `partialSyncErrorCopy.ts` change is the additive lookup export; F defined zero copy strings. |
| T24 | `isMithrilPartialSyncBlockingNodeStart` body is a one-line delegation to `isMithrilPartialSyncWorkingStatus`; the inline 9-status array is gone; overlay-spec predicate tests (old + new) green. |
| T25 | `PROGRESS_STATUSES` constant deleted; `isProgressStatus` computed as `isMithrilPartialSyncWorkingStatus(status) \|\| status === 'completed'`; full overlay spec green (identical routing). |
| T26 | Exactly one `spawn(` call site remains in `mithrilCommandRunner.ts`; POSIX process-group rationale comment preserved verbatim above it; `detached: !environment.isWindows` unchanged; `runCommand` still prefixes `--origin-tag DAEDALUS` and never writes stdin; entire existing runner spec passes UNMODIFIED (incl. both detached-POSIX and both Windows tests). |
| T27 | `listSnapshots`/`showSnapshot` delegate to `_listSnapshotsRaw`/`_showSnapshotRaw`; no duplicated `_runCommand(['cardano-db','snapshot'…` + `_safeJsonParse` block remains in the public methods; new pipeline spec green; existing log-file-name spec green. |
| T29 | No `behindByImmutables` identifier remains in `MithrilPartialSyncStore.ts`; availability handling (`isSignificantlyBehind`, `certifiedEpoch`) untouched; store spec green after the two assertion deletions. |
| T30 | No `elapsedSeconds` observable remains in the store; `startedAt` derivation (incl. `backendElapsed` anchor) untouched and its specs green. |
| T3 | `MithrilBootstrap.scss` contains neither `overlay-backrop` nor the mixin `@import`; regenerated `.scss.d.ts` content-identical; bootstrap specs green and bootstrap-variant completed frame byte-identical (boundary 11). |

# Escalations (stop and report; do not improvise)

1. **T23 / T22 contract (as amended)**: the start-failure fallback descriptor is not exported
   from `partialSyncErrorCopy.ts`, or its EN copy is not `Unable to start Mithril Sync.`, or the
   `COPY_BY_CODE` map (or its `FAILED.title` = `Mithril Sync failed` entry) is missing → STOP.
   F must not define a second fallback string or any new copy. (Preflight verified all of this
   present in the working tree on 2026-07-03 — the fallback check already HELD at the first
   implementation attempt; re-verify anyway.)
2. **T24 drift**: the inline array and `MITHRIL_PARTIAL_SYNC_WORKING_STATUSES` are no longer
   element-identical (an earlier section changed one of them) → STOP; delegation would then change
   node-start gating semantics.
3. **T27 caller appears**: if, at implementation time, any production code calls
   `MithrilPartialSyncService.listSnapshots` or `.showSnapshot` (today: none; the live route uses
   `MithrilBootstrapService`), the documented drop/throw delta is no longer inert → STOP.
4. **T3 typings churn**: regenerated `MithrilBootstrap.scss.d.ts` differs beyond whitespace →
   STOP (class-name surface changed; possible boundary-11 impact).
5. **Anchor drift beyond relocation**: any quoted locate-by snippet above cannot be found even
   approximately (earlier sections rewrote the code, not merely moved it) → implement the closest
   faithful step ONLY if the surrounding logic is unchanged; otherwise STOP and report the
   mismatch. Known-benign drift already accounted for: T26's duplication actually spans the whole
   promise pipeline (spawn block through `close` handler), wider than the brief's quoted
   `:159-176`/`:255-272` spawn hunks — the extraction above covers the full duplicated region per
   the approved plan wording ("options, spawn-info log, exit/close handlers"). The §1.1 anchors
   (both catch blocks, the store's `toStartError`, `partialSyncErrorCopy.ts`, and the landed spec
   tests) were re-verified verbatim against the post-CAT-E working tree on 2026-07-03 as part of
   the T23 Amendment.
6. **T23 landed-test drift**: the landed section spec test
   `keeps confirmation open and shows the localized start-failure fallback` no longer stubs its
   rejection with prose outside the `MithrilPartialSyncErrorCode` union (Step 1c pre-check
   fails), or it cannot pass UNMODIFIED for any other reason → STOP. Its staying green untouched
   is part of the amended contract.
