# 1. T13 — finalize retry, failure error-view with retry, finalize-failed copy EN+JA, spec, storybook story

**Section:** CAT-C — Completed-overlay finalize resilience (DD-703-4) for task-ux-703 (PR #3337 remediation).
**Decision honored (never relitigate):** DD-703-4 — keep ADR D-702a-1 auto-finalize; on rejection retry once, then show the existing error view with a retry action. Boundary 9: auto-finalize stands, ONLY the failure path changes. No Continue button. No silent empty catch.
**Anchors verified 2026-07-03 against the working tree:** constant at `MithrilPartialSyncOverlay.tsx:59`, effect at `:85-94`, swallowed catch at `:91` — all match the brief; no drift. Locate every edit by the quoted content below, never by line number.
**Vocabulary invariant (applies to every string and comment below):** user-facing copy says "Mithril Sync"; never "partial sync", never percentages, never "immutable". No task/finding/thread IDs in source comments or test titles.

All file paths are repo-relative to `/workspaces/mithril-partial-sync-ux`.

---

## Step 1 — Add the three finalize-failed intl messages

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`

**Locate** (quoted; this is the end of the `partialSyncFailedHint` descriptor followed by the start of `partialSyncCancelledTitle`):

```ts
    description: 'Hint shown for Mithril partial sync failed terminal state.',
  },
  partialSyncCancelledTitle: {
```

**Edit:** insert the three new descriptors between `},` and `partialSyncCancelledTitle: {`, so the result reads:

```ts
    description: 'Hint shown for Mithril partial sync failed terminal state.',
  },
  partialSyncFinalizeFailedTitle: {
    id: 'loading.mithrilPartialSync.error.finalizeFailed.title',
    defaultMessage: '!!!Finishing Mithril Sync failed',
    description:
      'Title shown when the automatic finalize hand-off after a completed Mithril partial sync fails twice.',
  },
  partialSyncFinalizeFailedHint: {
    id: 'loading.mithrilPartialSync.error.finalizeFailed.hint',
    defaultMessage:
      '!!!Mithril Sync completed, but Daedalus could not finish the final cleanup step. Try again to continue to Daedalus.',
    description:
      'Hint shown when the automatic finalize hand-off after a completed Mithril partial sync fails twice.',
  },
  partialSyncFinalizeFailedRetry: {
    id: 'loading.mithrilPartialSync.error.finalizeFailed.retry',
    defaultMessage: '!!!Try again',
    description:
      'Retry action label on the finalize-failed error view; re-invokes the finalize hand-off.',
  },
  partialSyncCancelledTitle: {
```

Note: the `!!!` prefix in `defaultMessage` is this repo's convention (polished copy lives in the locale JSON files; a spec asserts locale entries carry no `!!!`). Keep it exactly as shown.

## Step 2 — Add the EN locale entries

**File:** `source/renderer/app/i18n/locales/en-US.json` (alphabetically sorted; `error.finalizeFailed.*` sorts between `error.failed.title` and `error.latestDrift.hint`).

**Locate:**

```json
  "loading.mithrilPartialSync.error.failed.title": "Mithril Sync failed",
```

**Edit:** insert immediately after that line (keep two-space indent and trailing commas):

```json
  "loading.mithrilPartialSync.error.finalizeFailed.hint": "Mithril Sync completed, but Daedalus could not finish the final cleanup step. Try again to continue to Daedalus.",
  "loading.mithrilPartialSync.error.finalizeFailed.retry": "Try again",
  "loading.mithrilPartialSync.error.finalizeFailed.title": "Finishing Mithril Sync failed",
```

## Step 3 — Add the JA locale entries

**File:** `source/renderer/app/i18n/locales/ja-JP.json`

**Locate:**

```json
  "loading.mithrilPartialSync.error.failed.title": "Mithril同期に失敗しました",
```

**Edit:** insert immediately after that line:

```json
  "loading.mithrilPartialSync.error.finalizeFailed.hint": "Mithril同期は完了しましたが、Daedalusは最後の後処理を完了できませんでした。再試行してDaedalusに戻ってください。",
  "loading.mithrilPartialSync.error.finalizeFailed.retry": "再試行",
  "loading.mithrilPartialSync.error.finalizeFailed.title": "Mithril同期の後処理に失敗しました",
```

JA derivation (from `.agent/plans/mithril-partial-sync/mithril-partial-sync-ja-copy-table.md`): cleanup = 後処理 (rows 28–29 "Cleaning up..." / "後処理しています..."); "X failed" = 「…に失敗しました」 (row 33); "could not finish …" = 「…を完了できませんでした」 (row 44); retry = 再試行 (row 47); imperative hint tail 「…してください。」 (row 40); "Daedalusに戻る" (row 32). Use these strings verbatim — do not re-translate.

## Step 4 — Overlay: extend the React import

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`

**Locate:**

```tsx
import React, { useEffect } from 'react';
```

**Edit:** replace with:

```tsx
import React, { useEffect, useRef, useState } from 'react';
```

## Step 5 — Overlay: add the retry-delay constant

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`

**Locate:**

```tsx
const COMPLETED_AUTO_DISMISS_DELAY_MS = 4000;
```

**Edit:** replace with:

```tsx
const COMPLETED_AUTO_DISMISS_DELAY_MS = 4000;
// Delay before the single, silent retry of a rejected automatic finalize; long
// enough for a transient hiccup to clear, short enough that the success frame
// does not linger noticeably beyond its normal hand-off.
const FINALIZE_RETRY_DELAY_MS = 2000;
```

## Step 6 — Overlay: replace the auto-finalize effect with the retry-once + local-failure-state version

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`

**Locate** the entire existing comment + effect block (quoted in full — replace all of it):

```tsx
  // On 'completed', auto-fire the finalize IPC (reset-to-idle +
  // remove staging dir + clear marker + folder deletion) via the stable
  // `onDismissCompleted` MobX action once the success frame has lingered — the
  // explicit "Continue to Daedalus" click is gone. All cleanup semantics are
  // preserved; only the trigger changes from a click to this timeout.
  useEffect(() => {
    if (status !== 'completed') return undefined;
    // Not fire-and-forget — `onDismissCompleted` awaits the async
    // finalize IPC, so wrap it in `Promise.resolve(...).catch(...)` to ensure a
    // finalize rejection can never surface as an unhandled promise rejection.
    const timer = setTimeout(() => {
      Promise.resolve(onDismissCompleted()).catch(() => {});
    }, COMPLETED_AUTO_DISMISS_DELAY_MS);
    return () => clearTimeout(timer);
  }, [status, onDismissCompleted]);
```

**Edit:** replace with (verbatim):

```tsx
  // A finalize failure must not strand the success frame: this local flag
  // switches the overlay to the error view once the automatic finalize attempt
  // and its single silent retry have both failed. It has to be component-local
  // state because 'completed' renders through the progress view.
  const [finalizeFailed, setFinalizeFailed] = useState(false);
  // Finalize outcomes resolve asynchronously and can land after the store has
  // already hidden (unmounted) the overlay; guard state updates so a late
  // outcome never sets state on an unmounted component.
  const isUnmountedRef = useRef(false);
  useEffect(
    () => () => {
      isUnmountedRef.current = true;
    },
    []
  );

  // On 'completed', auto-fire the finalize IPC (reset-to-idle +
  // remove staging dir + clear marker + folder deletion) via the stable
  // `onDismissCompleted` MobX action once the success frame has lingered — the
  // explicit "Continue to Daedalus" click is gone. All cleanup semantics are
  // preserved; only the trigger changes from a click to this timeout.
  useEffect(() => {
    if (status !== 'completed') return undefined;
    let disposed = false;
    let retryTimer: ReturnType<typeof setTimeout> | undefined;
    // Not fire-and-forget — `onDismissCompleted` awaits the async finalize
    // IPC. A rejection is retried once, silently, after a short delay; a second
    // rejection surfaces the finalize-failed error view below instead of being
    // swallowed, so a failure can neither strand the success frame nor become
    // an unhandled promise rejection.
    const timer = setTimeout(() => {
      Promise.resolve(onDismissCompleted()).catch(() => {
        if (disposed || isUnmountedRef.current) return;
        retryTimer = setTimeout(() => {
          Promise.resolve(onDismissCompleted()).catch(() => {
            if (disposed || isUnmountedRef.current) return;
            setFinalizeFailed(true);
          });
        }, FINALIZE_RETRY_DELAY_MS);
      });
    }, COMPLETED_AUTO_DISMISS_DELAY_MS);
    return () => {
      disposed = true;
      clearTimeout(timer);
      if (retryTimer !== undefined) clearTimeout(retryTimer);
    };
  }, [status, onDismissCompleted]);

  // Manual finalize retry from the error view. It re-invokes the same finalize
  // hand-off and is renderer-local (same footing as the defensive Quit below) —
  // it is NOT a backend recovery action and never joins the
  // allowedRecoveryActions-driven membership.
  const handleFinalizeRetry = () => {
    Promise.resolve(onDismissCompleted())
      .then(() => {
        if (isUnmountedRef.current) return;
        // Success hands control back to the store-driven dismissal; clear the
        // failure flag so the hand-off frame shows until the overlay hides.
        setFinalizeFailed(false);
      })
      .catch(() => {
        if (isUnmountedRef.current) return;
        // Remain on the failure view; the retry action stays available.
        setFinalizeFailed(true);
      });
  };
```

Invariants at this step: the 4000 ms auto-finalize trigger and all finalize cleanup semantics are unchanged (boundary 9 / D-702a-1); every `.catch` either schedules the retry or sets visible state — no empty catch remains; no comment cites a task/finding ID.

## Step 7 — Overlay: gate the view switch and the error copy on the finalize failure

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`

**Locate:**

```tsx
  const isProgressStatus = PROGRESS_STATUSES.includes(status);
  const activeHeadingId = isProgressStatus
    ? MITHRIL_PROGRESS_HEADING_ID
    : MITHRIL_ERROR_HEADING_ID;
  const errorCopy = resolvePartialSyncErrorCopy(status, error);
```

**Edit:** replace with:

```tsx
  const isProgressStatus = PROGRESS_STATUSES.includes(status);
  // 'completed' is a progress status, so the finalize-failed frame needs this
  // explicit override to leave the progress view. Gating on 'completed' keeps
  // a stale flag from ever leaking into any other status.
  const isFinalizeFailureShown = finalizeFailed && status === 'completed';
  const showProgressView = isProgressStatus && !isFinalizeFailureShown;
  const activeHeadingId = showProgressView
    ? MITHRIL_PROGRESS_HEADING_ID
    : MITHRIL_ERROR_HEADING_ID;
  const errorCopy = isFinalizeFailureShown
    ? {
        title: MithrilBootstrapMessages.partialSyncFinalizeFailedTitle,
        hint: MithrilBootstrapMessages.partialSyncFinalizeFailedHint,
      }
    : resolvePartialSyncErrorCopy(status, error);
```

Note: the finalize-failed copy pair intentionally lives here, NOT in `partialSyncErrorCopy.ts` — the resolver keys off backend `status`/`error`, while this failure is renderer-local component state. Do not edit `partialSyncErrorCopy.ts` (CAT-E touches it later).

## Step 8 — Overlay: add the single finalize-retry action

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`

**Locate** (end of the `orderedErrorActions` declaration):

```tsx
  const orderedErrorActions = [
    ...errorActions.filter((action) => action.variant !== 'primary'),
    ...errorActions.filter((action) => action.variant === 'primary'),
  ];
```

**Edit:** insert immediately after it:

```tsx
  // The finalize-failed frame offers exactly one action: retry the finalize
  // hand-off. It deliberately bypasses the recovery-action membership above.
  const finalizeFailureActions = [
    {
      label: intl.formatMessage(
        MithrilBootstrapMessages.partialSyncFinalizeFailedRetry
      ),
      onClick: handleFinalizeRetry,
      variant: 'primary' as const,
    },
  ];
```

Invariant: `recoveryActions`/`orderedErrorActions` membership stays driven strictly by the backend `canRetry`/`canRestartNormally`/`canWipeAndFullSync` booleans (boundary 2) — this new array is only ever used when `isFinalizeFailureShown` is true (Step 9).

## Step 9 — Overlay: render the error view on finalize failure

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`

**Edit 9a — locate:**

```tsx
          {isProgressStatus ? (
            <MithrilProgressView
```

replace with:

```tsx
          {showProgressView ? (
            <MithrilProgressView
```

**Edit 9b — locate** the `MithrilErrorView` branch:

```tsx
            <MithrilErrorView
              error={error as any}
              onOpenExternalLink={onOpenExternalLink}
              title={intl.formatMessage(errorCopy.title)}
              hint={intl.formatMessage(errorCopy.hint)}
              hintAsBody={status === 'cancelled'}
              actions={orderedErrorActions}
              rightAlignActions
            />
```

replace with:

```tsx
            <MithrilErrorView
              error={isFinalizeFailureShown ? null : (error as any)}
              onOpenExternalLink={onOpenExternalLink}
              title={intl.formatMessage(errorCopy.title)}
              hint={intl.formatMessage(errorCopy.hint)}
              hintAsBody={status === 'cancelled'}
              actions={
                isFinalizeFailureShown
                  ? finalizeFailureActions
                  : orderedErrorActions
              }
              rightAlignActions
            />
```

`error={null}` on the finalize frame is deliberate: there is no backend `MithrilPartialSyncError` object for this renderer-local failure, and passing none keeps raw backend text out of the frame (boundary 8). Do not modify `MithrilProgressView.tsx` or `MithrilErrorView.tsx` in any way (bootstrap boundary 11; CAT-G later touches these files).

## Step 10 — Spec: two new tests in the overlay spec

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`

Leave every existing test unchanged — in particular `'auto-fires finalize on the completed timeout with no Continue button'` and `'catches a rejecting onDismissCompleted from the auto-dismiss timer (no unhandled rejection)'` still pass against the new effect (first rejection is caught by the retry scheduler, still exactly one call at 4000 ms, still no unhandled rejection; the pending retry timer is cleared by the effect cleanup on unmount).

**Locate** the closing of the last test in the `describe('MithrilPartialSyncOverlay', ...)` block:

```tsx
    expect(
      screen.queryByText(/was stopped before it finished/i)
    ).not.toBeInTheDocument();
  });
});
```

**Edit:** insert the two new tests before the final `});`, so the block ends:

```tsx
    expect(
      screen.queryByText(/was stopped before it finished/i)
    ).not.toBeInTheDocument();
  });

  it('retries a rejected finalize once and stays on the hand-off frame when the retry succeeds', async () => {
    jest.useFakeTimers();
    const onDismissCompleted = jest
      .fn()
      .mockRejectedValueOnce(new Error('finalize failed'))
      .mockResolvedValue(undefined);
    try {
      renderComponent({ status: 'completed', onDismissCompleted });

      await act(async () => {
        jest.advanceTimersByTime(4000);
      });
      expect(onDismissCompleted).toHaveBeenCalledTimes(1);
      // the silent retry is pending: still the success hand-off frame
      expect(screen.getByText(/returning to daedalus/i)).toBeInTheDocument();

      await act(async () => {
        jest.advanceTimersByTime(2000);
      });
      expect(onDismissCompleted).toHaveBeenCalledTimes(2);
      // the successful retry never surfaces the failure frame
      expect(
        screen.queryByRole('heading', {
          name: /finishing mithril sync failed/i,
        })
      ).not.toBeInTheDocument();
      expect(screen.getByText(/returning to daedalus/i)).toBeInTheDocument();
    } finally {
      jest.useRealTimers();
    }
  });

  it('shows the finalize-failed error view after both finalize attempts fail and recovers through its retry action', async () => {
    jest.useFakeTimers();
    const onDismissCompleted = jest
      .fn()
      .mockRejectedValueOnce(new Error('finalize failed'))
      .mockRejectedValueOnce(new Error('finalize failed again'))
      .mockResolvedValue(undefined);
    try {
      renderComponent({ status: 'completed', onDismissCompleted });

      await act(async () => {
        jest.advanceTimersByTime(4000);
      });
      await act(async () => {
        jest.advanceTimersByTime(2000);
      });
      expect(onDismissCompleted).toHaveBeenCalledTimes(2);

      // both attempts failed: the success frame gives way to the error view
      expect(
        screen.getByRole('heading', {
          level: 1,
          name: /finishing mithril sync failed/i,
        })
      ).toBeInTheDocument();
      expect(
        screen.getByText(/could not finish the final cleanup step/i)
      ).toBeInTheDocument();
      expect(
        screen.queryByText(/returning to daedalus/i)
      ).not.toBeInTheDocument();

      // a successful manual retry re-invokes the finalize and hands control
      // back to the store-driven dismissal (the hand-off frame returns while
      // the store hides the overlay via status)
      await act(async () => {
        fireEvent.click(screen.getByRole('button', { name: /try again/i }));
      });
      expect(onDismissCompleted).toHaveBeenCalledTimes(3);
      expect(
        screen.queryByRole('heading', {
          name: /finishing mithril sync failed/i,
        })
      ).not.toBeInTheDocument();
      expect(screen.getByText(/returning to daedalus/i)).toBeInTheDocument();
    } finally {
      jest.useRealTimers();
    }
  });
});
```

No change is needed to the `'ships polished runtime strings without placeholder markers'` locale test — it scans every `loading.mithrilPartialSync.` key by prefix, so it automatically covers the three new keys in both locales (and fails if a `!!!` placeholder leaks into a locale file).

## Step 11 — Storybook: finalize-failed story

**File:** `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`

**Edit 11a — locate:**

```tsx
  elapsedSeconds?: number;
  completed?: boolean;
}
```

replace with:

```tsx
  elapsedSeconds?: number;
  completed?: boolean;
  onDismissCompleted?: () => void | Promise<void>;
}
```

**Edit 11b — locate** (inside `MithrilPartialSyncOverlayStory`'s JSX; the explicit prop must come after the `{...baseProps}` spread, which it does here):

```tsx
      canWipeAndFullSync={props.canWipeAndFullSync || false}
```

replace with:

```tsx
      canWipeAndFullSync={props.canWipeAndFullSync || false}
      onDismissCompleted={
        props.onDismissCompleted || baseProps.onDismissCompleted
      }
```

**Edit 11c — locate** the end of the story chain:

```tsx
  .add('Failed - Finalizing (All Recovery Actions)', () => (
    <MithrilPartialSyncOverlayStory
      status="failed"
      error={finalizingError}
      canRetry
      canRestartNormally
      canWipeAndFullSync
    />
  ));
```

replace with:

```tsx
  .add('Failed - Finalizing (All Recovery Actions)', () => (
    <MithrilPartialSyncOverlayStory
      status="failed"
      error={finalizingError}
      canRetry
      canRestartNormally
      canWipeAndFullSync
    />
  ))
  // Auto-plays the finalize-failure path: the completed frame lingers ~4 s,
  // the automatic finalize rejects, the single silent retry rejects ~2 s
  // later, and the overlay then swaps to the finalize-failed error view with
  // its retry action (which re-rejects here, so the story stays on the error
  // frame).
  .add('Completed - Finalize Failed (auto-plays)', () => (
    <MithrilPartialSyncOverlayStory
      status="completed"
      filesDownloaded={9}
      filesTotal={9}
      elapsedSeconds={845}
      completed
      onDismissCompleted={() => {
        action('onDismissCompleted')();
        return Promise.reject(new Error('finalize failed'));
      }}
    />
  ));
```

The rejecting handler drives the component's real one-shot-retry path; every rejection is caught inside the component, so the story produces no unhandled rejection.

## Step 12 — Regenerate the i18n catalogs

Run:

```bash
yarn i18n:manage
```

(`i18n:extract` + `i18n:check`.) This regenerates `translations/messages.json` and `source/renderer/app/i18n/locales/defaultMessages.json` with the three new descriptors and validates the locale files. Because Steps 2–3 pre-seeded polished entries, the runner must NOT rewrite `en-US.json`/`ja-JP.json` with `!!!` placeholders — if it reports the new keys as missing or added, re-check the key spelling in Steps 1–3. Leave the generated-file diffs in the working tree; do not stage anything.

## Step 13 — Format and verify

1. Format only the touched source files:

```bash
node_modules/.bin/prettier --write \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx \
  storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx
```

2. Run the narrow test scope (covers the overlay spec, the untouched bootstrap specs in the same folder — boundary 11 — plus `partialSyncErrorCopy.spec.ts` and the locale placeholder test):

```bash
yarn test:jest --testPathPattern "source/renderer/app/components/loading/mithril-bootstrap"
```

Key assertions that must pass: the two new tests from Step 10; the pre-existing `'auto-fires finalize on the completed timeout with no Continue button'`, `'catches a rejecting onDismissCompleted from the auto-dismiss timer (no unhandled rejection)'`, and `'ships polished runtime strings without placeholder markers'`; and every test in `MithrilBootstrap.spec.tsx` (bootstrap regression gate).

3. Environment contingency (known tooling, not code regressions): under Node v24, if jest fails resolving `.scss` imports, apply the sprint's gitignored identity-obj-proxy jest sidecar `--config`; if a `tsc --noEmit` check is run and fails on a stale `*.scss.d.ts`, regenerate just that file with `node_modules/.bin/typed-scss-modules <file.scss>`. Apply these before treating a failure as a regression. Never stage the sidecar.

---

## Files touched

- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts` (3 new descriptors)
- `source/renderer/app/i18n/locales/en-US.json` (3 new keys)
- `source/renderer/app/i18n/locales/ja-JP.json` (3 new keys)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` (retry-once effect, local failure state, error-view branch)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` (2 new tests)
- `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx` (prop pass-through + 1 new story)
- Generated by Step 12 (do not hand-edit): `translations/messages.json`, `source/renderer/app/i18n/locales/defaultMessages.json`

Never touch: `.gitignore`, anything under `.agent/skills/` (user's unrelated in-flight changes). Never run `git add`/commit/push or any `gh` command.

## Out of scope (owned by other sections)

- N7 Diagnostics re-check removal — CAT-B (T11); N3 chainStorageCoordinator/MithrilController seams — CAT-B/G.
- T18 insufficient-disk-space copy key and `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` code — CAT-D.
- All remaining copy work, the shared start-failure fallback in `partialSyncErrorCopy.ts` (T22), further error codes, and the coded `PARTIAL_SYNC_DISABLED` throw (T16) — CAT-E.
- `mithrilErrorMessage.ts` helper (T23) and spawn-helper extraction (T26) — CAT-F.
- N4 prop-count reduction in `MithrilPartialSyncOverlay.tsx`/`MithrilProgressView.tsx` — CAT-G (runs after this section; CAT-G must re-locate anchors by content).
- Any change to `MithrilPartialSyncStore.dismissCompletedOverlay`, the finalize IPC, or backend finalize ordering (see Escalations).

## Acceptance checks

- **T13 / DD-703-4 — auto-finalize preserved:** finalize still auto-fires once at the 4000 ms linger with no Continue button — existing spec `'auto-fires finalize on the completed timeout with no Continue button'` stays green (boundary 9, D9 + D-702a-1).
- **T13 — retry once (~2 s):** first rejection schedules exactly one silent retry after `FINALIZE_RETRY_DELAY_MS = 2000`; a successful retry never shows the failure frame — new spec `'retries a rejected finalize once and stays on the hand-off frame when the retry succeeds'`.
- **T13 — failure surfaces with working retry:** two rejections switch the overlay (local state, since `completed` is a progress status) to `MithrilErrorView` with the finalize-failed title/hint and a retry action re-invoking `onDismissCompleted`; a retry that succeeds returns control to the store-driven dismissal — new spec `'shows the finalize-failed error view after both finalize attempts fail and recovers through its retry action'` (dismissal itself is the store's existing `dismissCompletedOverlay` → `shouldShowOverlay` flow, unchanged).
- **T13 — no silent empty catch:** every catch path schedules the retry or sets visible state; no `.catch(() => {})` remains in the file.
- **T13 — copy EN+JA:** `loading.mithrilPartialSync.error.finalizeFailed.{title,hint,retry}` land in both locale files with polished copy; covered by the prefix-scanning locale spec; catalogs regenerated via `yarn i18n:manage`.
- **T13 — storybook:** `'Completed - Finalize Failed (auto-plays)'` story drives the overlay into the error view with the retry action.
- **Boundaries:** recovery-action membership untouched (retry is renderer-local like Quit, never inferred as a backend action); no raw backend text in the new frame (`error={null}`); bootstrap specs and the bootstrap-variant `MithrilProgressView` completed frame untouched/byte-identical (no edits to `MithrilProgressView.tsx`/`MithrilErrorView.tsx`); no percentages, no "partial sync"/"immutable" in user-visible copy; no task IDs in comments or test titles.

## Escalations

1. **End-to-end reachability of the failure path (informational; escalate-on-mismatch).** `MithrilPartialSyncStore.dismissCompletedOverlay` (`source/renderer/app/stores/MithrilPartialSyncStore.ts`, `dismissCompletedOverlay = async () => {` at ~:337) intentionally catches a finalize rejection internally (logs + `finally { await this.syncStatus(); }`, never rethrows), and its spec pins this: `'dismissCompletedOverlay swallows a finalize rejection: … never throws (overlay hides via idle resync)'` (`MithrilPartialSyncStore.spec.ts:301`). So through today's App.tsx wiring the prop never rejects and the new failure frame is contract-hardening for the typed `onDismissCompleted(): void | Promise<void>` seam (it becomes live if the store/backend failure contract ever changes, e.g. the deferred backend reorder its comments mention). DD-703-4's fix and AC are component-level, so this plan faithfully stays overlay-only and does NOT change the store. Escalate only if the orchestrator expected the store to rethrow (that would also require amending the store spec above — a cross-section decision, not CAT-C's to make).
2. No anchor drift found: the brief's `:59`, `:85-94`, `:91` anchors matched the working tree exactly at planning time. If earlier sections (A/B run first) shift them, all Step edits locate by quoted content and remain valid; if a quoted snippet no longer matches at all, stop and escalate rather than improvising.
