# task-ux-703 — CAT-B implementation plan (Start flow and availability)

Section plan for CAT-B of task-ux-703 (PR #3337 remediation). Covers threads T11 (+ nit N7),
T12/T31, T15, T21, T28, and the T2 no-op. Locked decisions DD-703-2, DD-703-3, DD-703-10,
DD-703-11 are translated into mechanics below — do not relitigate them.

All anchors below are given as QUOTED snippets — locate by content, never by line number
(earlier sections' edits may have shifted lines). All paths are repo-relative from
`/workspaces/mithril-partial-sync-ux`.

Global rules for every step:

- User-facing copy says "Mithril Sync" — never "partial sync", never percentages, never the
  word "immutable" in user-visible strings. Internal identifiers keep their `partialSync` names.
- No task/finding/thread IDs (T11, N7, DD-703-x, CAT-B, ...) in source comments or test titles.
  Where a comment is needed, write a plain rationale comment at the surrounding comment density.
- Never commit, push, or run `gh`. Never run `git add -A`/`-u`. Never touch `.gitignore` or
  anything under `.agent/skills/`.
- Recovery actions still come strictly from backend `allowedRecoveryActions` — nothing in this
  section adds renderer-side inference, thresholds, or auto-triggers.

---

# 1. T11 + N7 — availability visibility end-to-end

Goal (DD-703-2 + DD-703-10): the Diagnostics Mithril section renders whenever the feature is
enabled — in all three probe states (behind / near tip / probe failed) — with the CTA enabled in
all three. Today it hides unless significantly behind, and a probe failure is collapsed into
`{ isSignificantlyBehind: false }` before IPC. N7 (absorbed here): the redundant inner
`isMithrilPartialSyncEnabled` re-check inside the `shouldShowRecommendation` expression in
`DaedalusDiagnostics.tsx` disappears as part of the prop replacement; the OUTER
`isMithrilPartialSyncEnabled &&` gate is the kill switch (locked boundary 10) and MUST stay.

## 1.1 Shared type — add the probe-failed flag

**Step 1.** `source/common/types/mithril-partial-sync.types.ts` — in the block starting
`export type MithrilPartialSyncAvailability = {`, locate:

```ts
  isEnabled: boolean;
  isSignificantlyBehind: boolean;
```

Insert immediately after `isSignificantlyBehind: boolean;`:

```ts
  // Set when the behind-ness probe itself failed, so "not significantly
  // behind" can be told apart from "behind-ness unknown". Optional: absent
  // means the probe succeeded and isSignificantlyBehind is trustworthy.
  isProbeFailed?: boolean;
```

Invariant: this is an optional, append-only field on the availability type. Do NOT touch the
`MithrilPartialSyncErrorCode` union (CAT-D/CAT-E own its append-only edits).

## 1.2 Service — set the flag in the probe catch

**Step 2.** `source/main/mithril/MithrilPartialSyncService.ts` — locate the return-type
declaration:

```ts
  async getPartialSyncBehindness(): Promise<{
    isSignificantlyBehind: boolean;
    behindByImmutables?: number;
    certifiedEpoch?: number | null;
  }> {
```

Replace with:

```ts
  async getPartialSyncBehindness(): Promise<{
    isSignificantlyBehind: boolean;
    isProbeFailed?: boolean;
    behindByImmutables?: number;
    certifiedEpoch?: number | null;
  }> {
```

**Step 3.** Same method, in its `catch` block, locate:

```ts
      // Aggregator unreachable, no immutable dir yet, or any failure ⇒ degrade to not-behind.
      logger.warn(
        'MithrilPartialSyncService: behind-ness probe failed; treating as not significantly behind',
        { error }
      );
      return { isSignificantlyBehind: false };
```

Replace with:

```ts
      // Aggregator unreachable, no immutable dir yet, or any failure ⇒ degrade
      // to not-behind, but flag the failure so consumers can render an
      // availability-unknown state instead of a confident near-tip one.
      logger.warn(
        'MithrilPartialSyncService: behind-ness probe failed; treating as not significantly behind',
        { error }
      );
      return { isSignificantlyBehind: false, isProbeFailed: true };
```

(The word "immutable" in code comments is fine — the restriction is user-visible copy only.)

## 1.3 Controller — pass-through (no code change, test only)

**Step 4.** `source/main/mithril/MithrilController.ts` — NO code change. The probe result is
already spread into the payload at:

```ts
    const behindness =
      await this._partialSyncService.getPartialSyncBehindness();
    return { isEnabled, ...behindness };
```

so `isProbeFailed` flows through automatically. The two early returns above it
(`return { isEnabled: false, isSignificantlyBehind: false };` and
`return { isEnabled, isSignificantlyBehind: false };` for the working/cancelled guard)
intentionally do NOT set the flag — those are known states, not probe failures. Leave them
untouched. A pass-through test is added in step 13.

## 1.4 Store — observable + apply

**Step 5.** `source/renderer/app/stores/MithrilPartialSyncStore.ts` — locate:

```ts
  @observable isSignificantlyBehind = false;
```

Insert immediately after that line:

```ts
  // True when the backend behind-ness probe failed, so "not behind" cannot be
  // trusted; the Diagnostics section shows an availability-unknown hint instead.
  @observable isProbeFailed = false;
```

**Step 6.** Same file, inside `_applyAvailability`, locate:

```ts
    this.isSignificantlyBehind = availability.isSignificantlyBehind;
```

Insert immediately after that line:

```ts
    this.isProbeFailed = Boolean(availability.isProbeFailed);
```

## 1.5 Container and Diagnostics wiring (includes N7)

**Step 7.** `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` — locate:

```tsx
          isMithrilPartialSyncSignificantlyBehind={
            mithrilPartialSync.isSignificantlyBehind
          }
```

Insert immediately after the closing `}` of that prop:

```tsx
          isMithrilPartialSyncProbeFailed={mithrilPartialSync.isProbeFailed}
```

**Step 8.** `source/renderer/app/components/status/DaedalusDiagnostics.tsx` — in the Props
type, locate:

```ts
  isMithrilPartialSyncSignificantlyBehind: boolean;
```

Insert immediately after:

```ts
  isMithrilPartialSyncProbeFailed: boolean;
```

**Step 9.** Same file, in the `render()` destructuring, locate:

```ts
      isMithrilPartialSyncSignificantlyBehind,
```

Insert immediately after:

```ts
      isMithrilPartialSyncProbeFailed,
```

**Step 10 (N7).** Same file, locate:

```tsx
              {isMithrilPartialSyncEnabled && (
                <MithrilPartialSyncSection
                  isActionBlocked={isMithrilActionBlocked}
                  isMithrilPartialSyncWorking={isMithrilPartialSyncWorking}
                  shouldShowRecommendation={
                    isMithrilPartialSyncEnabled &&
                    isMithrilPartialSyncSignificantlyBehind
                  }
```

Replace with:

```tsx
              {isMithrilPartialSyncEnabled && (
                <MithrilPartialSyncSection
                  isActionBlocked={isMithrilActionBlocked}
                  isMithrilPartialSyncWorking={isMithrilPartialSyncWorking}
                  isSignificantlyBehind={isMithrilPartialSyncSignificantlyBehind}
                  isProbeFailed={isMithrilPartialSyncProbeFailed}
```

Leave the remaining props (`behindByEpochs`, `showConfirmationOnOpen`, `onRestoreFocus`,
`onStartMithrilPartialSync`) exactly as they are. Invariant: the OUTER
`{isMithrilPartialSyncEnabled && (` gate must remain — it is the kill switch that hides all
Mithril UI (locked boundary 10). The inner re-check is the nit being removed.

## 1.6 Section — always render with three copy states

**Step 11.** `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` — in the
`Props` type, locate:

```ts
  shouldShowRecommendation: boolean;
```

Replace with:

```ts
  isSignificantlyBehind: boolean;
  isProbeFailed: boolean;
```

Then add the variant type import: locate

```ts
import MithrilPartialSyncRecommendation from './MithrilPartialSyncRecommendation';
```

Replace with:

```ts
import MithrilPartialSyncRecommendation from './MithrilPartialSyncRecommendation';
import type { MithrilAvailabilityVariant } from './MithrilPartialSyncRecommendation';
```

**Step 12.** Same file, in `render()`, locate:

```tsx
    const { isActionBlocked, shouldShowRecommendation } = this.props;
```

Replace with:

```tsx
    const { isActionBlocked, isSignificantlyBehind, isProbeFailed } =
      this.props;
```

Then locate and DELETE the early return:

```tsx
    if (!shouldShowRecommendation) {
      return null;
    }
```

Replace it with the variant computation (the section is now always rendered; the parent gates
on the feature being enabled):

```tsx
    // The section stays visible in every probe state; only the tooltip copy
    // adapts. A confident behind result outranks the failure hint, and a
    // failed probe outranks the near-tip reassurance.
    let availabilityVariant: MithrilAvailabilityVariant = 'near-tip';
    if (isSignificantlyBehind) {
      availabilityVariant = 'behind';
    } else if (isProbeFailed) {
      availabilityVariant = 'availability-unknown';
    }
```

Then locate:

```tsx
        <MithrilPartialSyncRecommendation
          isActionBlocked={isActionBlocked}
          onShowConfirmation={this.showConfirmation}
        />
```

Replace with:

```tsx
        <MithrilPartialSyncRecommendation
          isActionBlocked={isActionBlocked}
          variant={availabilityVariant}
          onShowConfirmation={this.showConfirmation}
        />
```

Invariant: the CTA is enabled in all three states — the button's `disabled` is driven only by
`isActionBlocked` (unchanged). Confirmation still precedes start (locked boundary 3) — the
existing `showConfirmation`/`startFromConfirmation` flow is untouched. Do NOT touch the
hardcoded start-failure fallback string near `'Unable to start Mithril partial sync.'` — CAT-E
owns that (T22).

## 1.7 Recommendation — variant-driven tooltip + new intl messages

**Step 13a.** `source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx` —
inside the `defineMessages({ ... })` block, locate:

```ts
  recommendation: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncRecommendation',
```

and after that whole message entry's closing `},` (the line following
`'Tooltip copy shown on hover over the Mithril Sync button in diagnostics',`), insert:

```ts
  recommendationNearTip: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationNearTip',
    defaultMessage:
      '!!!Your node is close to the blockchain tip. You can still use Mithril Sync to restore verified chain data.',
    description:
      'Tooltip copy for the Mithril Sync button in diagnostics when the node is not significantly behind',
  },
  recommendationUnknown: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationUnknown',
    defaultMessage:
      '!!!Daedalus could not check how far behind your node is. You can still use Mithril Sync to restore verified chain data.',
    description:
      'Tooltip copy for the Mithril Sync button in diagnostics when the behind-ness check failed',
  },
```

**Step 13b.** Same file — above the `type Props = {` declaration, add the exported variant
type, and add the prop. Locate:

```ts
type Props = {
  isActionBlocked: boolean;
  onShowConfirmation: () => void;
};
```

Replace with:

```ts
export type MithrilAvailabilityVariant =
  | 'behind'
  | 'near-tip'
  | 'availability-unknown';

type Props = {
  isActionBlocked: boolean;
  variant: MithrilAvailabilityVariant;
  onShowConfirmation: () => void;
};
```

**Step 13c.** Same file, in `render()`, locate:

```tsx
    const { isActionBlocked, onShowConfirmation } = this.props;
    const { intl } = this.context;
```

Replace with:

```tsx
    const { isActionBlocked, variant, onShowConfirmation } = this.props;
    const { intl } = this.context;

    let tooltipMessage = messages.recommendation;
    if (variant === 'near-tip') {
      tooltipMessage = messages.recommendationNearTip;
    } else if (variant === 'availability-unknown') {
      tooltipMessage = messages.recommendationUnknown;
    }
```

Then locate:

```tsx
                {intl.formatMessage(messages.recommendation)}
```

Replace with:

```tsx
                {intl.formatMessage(tooltipMessage)}
```

## 1.8 Locale files (EN + JA)

**Step 14.** `source/renderer/app/i18n/locales/en-US.json` — locate the line:

```json
  "daedalus.diagnostics.dialog.mithrilPartialSyncRecommendation": "If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified chain data to help speed up the sync.",
```

Insert immediately after it (preserving alphabetical key order):

```json
  "daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationNearTip": "Your node is close to the blockchain tip. You can still use Mithril Sync to restore verified chain data.",
  "daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationUnknown": "Daedalus could not check how far behind your node is. You can still use Mithril Sync to restore verified chain data.",
```

**Step 15.** `source/renderer/app/i18n/locales/ja-JP.json` — locate the line:

```json
  "daedalus.diagnostics.dialog.mithrilPartialSyncRecommendation": "Cardanoノードの同期に時間がかかりすぎると感じる場合は、Mithril同期で検証済みのチェーンデータを復元することで、同期を高速化できます。",
```

Insert immediately after it:

```json
  "daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationNearTip": "ノードはブロックチェーンの先端に近い状態です。必要に応じて、Mithril同期で検証済みのチェーンデータを復元できます。",
  "daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationUnknown": "ノードがどの程度遅れているかを確認できませんでした。必要に応じて、Mithril同期で検証済みのチェーンデータを復元できます。",
```

JA derivation: first sentences follow copy-table row 3 (「ノードはブロックチェーンの先端より遅れています。」
pattern for tip-relative position statements); second sentence follows rows 15/18
(「Mithril同期で検証済みのチェーンデータを復元…」). The near-tip first sentence has no exact
table precedent — flagged as a mild uncertainty in the planner log; do not reword further.

**Step 16.** Regenerate the extraction catalog: run `yarn i18n:manage` (equivalent to
`yarn i18n:extract && yarn i18n:check`). This updates `translations/messages.json`; include the
regenerated file in the working tree. If `i18n:check` reports ONLY pre-existing issues untouched
by these two keys, note them and continue; if it flags the two new keys, fix the locale entries
until it passes.

## 1.9 Tests for chunk 1

**Step 17.** `source/main/mithril/MithrilPartialSyncService.spec.ts` — inside
`describe('getPartialSyncBehindness', ...)`:

a) Update the existing test `it('degrades to not behind without throwing when the latest
snapshot lookup rejects', ...)` — change its expectation from:

```ts
      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: false,
      });
```

to:

```ts
      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: false,
        isProbeFailed: true,
      });
```

b) Add a new test right after it (local-read failure lands in the same catch):

```ts
    it('flags the probe as failed when the local immutable read rejects', async () => {
      const service = new MithrilPartialSyncService();
      jest
        .spyOn(service._chainStorageManager, 'getManagedChainPath')
        .mockRejectedValue(new Error('no chain dir yet'));
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25));

      await expect(service.getPartialSyncBehindness()).resolves.toEqual({
        isSignificantlyBehind: false,
        isProbeFailed: true,
      });
    });
```

(Do NOT use `stubLocalImmutableNumber` here — this test needs the rejecting path.)

**Step 18.** `source/main/mithril/MithrilController.spec.ts` — inside
`describe('getPartialSyncAvailability (main-side probe guard)', ...)`, add after the test
`it('still probes behind-ness and merges the figures for idle, failed, and completed', ...)`:

```ts
    it('passes the probe-failed flag through to the availability payload', async () => {
      mockGetPartialSyncBehindness.mockResolvedValue({
        isSignificantlyBehind: false,
        isProbeFailed: true,
      });
      const controller = createController();
      controller.setPartialSyncStatus(createStatusSnapshot('idle'));

      await expect(controller.getPartialSyncAvailability()).resolves.toEqual({
        isEnabled: true,
        isSignificantlyBehind: false,
        isProbeFailed: true,
      });
    });
```

**Step 19.** `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` — add after the test
`it('populates certifiedEpoch from the availability payload and leaves it undefined when
absent', ...)`:

```ts
  it('flags a failed behind-ness probe from the availability payload and clears it when absent', () => {
    const store = setupStore();

    expect(store.isProbeFailed).toBe(false);

    store._applyAvailability({
      isEnabled: true,
      isSignificantlyBehind: false,
      isProbeFailed: true,
    });
    expect(store.isProbeFailed).toBe(true);

    store._applyAvailability({
      isEnabled: true,
      isSignificantlyBehind: false,
    });
    expect(store.isProbeFailed).toBe(false);
  });
```

**Step 20.** `source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx`:

a) In `defaultProps`, locate:

```ts
  shouldShowRecommendation: true,
```

Replace with:

```ts
  isSignificantlyBehind: true,
  isProbeFailed: false,
```

b) REPLACE the test `it('renders nothing and leaves no header row when the recommendation is
gated off', ...)` entirely with two visibility tests:

```ts
  it('keeps the section and an enabled CTA visible with near-tip copy when not significantly behind', () => {
    renderComponent({ isSignificantlyBehind: false });

    expect(screen.getByRole('button', { name: 'Mithril Sync' })).toBeEnabled();
    expect(
      screen.getByText(
        'Your node is close to the blockchain tip. You can still use Mithril Sync to restore verified chain data.'
      )
    ).toBeInTheDocument();
  });

  it('keeps the section and an enabled CTA visible with availability-unknown copy when the probe failed', () => {
    renderComponent({ isSignificantlyBehind: false, isProbeFailed: true });

    expect(screen.getByRole('button', { name: 'Mithril Sync' })).toBeEnabled();
    expect(
      screen.getByText(
        'Daedalus could not check how far behind your node is. You can still use Mithril Sync to restore verified chain data.'
      )
    ).toBeInTheDocument();
  });
```

c) Update the test `it('deep-link open works even when the recommendation is gated off', ...)`:
rename to `'deep-link open works even when the node is not significantly behind'` and change its
override from `shouldShowRecommendation: false` to `isSignificantlyBehind: false`.

d) The existing test `it('exposes the recommendation copy as the Mithril Sync button
tooltip', ...)` stays — `defaultProps` is the behind state, so it now asserts the behind
variant. No change needed.

---

# 2. T12/T31, T15, T21, T28 — start-rejection surfacing, recovery-action hygiene, cache invalidation, probe join (T2 = checklist-only)

## 2.1 T12/T31 — start rejections rethrow after resync; session flag reset

**Step 21.** `source/renderer/app/stores/MithrilPartialSyncStore.ts` — inside
`startPartialSync`, locate the whole post-resync block:

```ts
    if (this.status !== START_PENDING_STATUS) {
      logger.warn(
        'MithrilPartialSyncStore: swallowed partial sync start rejection after backend status resync',
        {
          error: startError,
          status: this.status,
        }
      );
      return;
    }

    throw toStartError(startError);
```

Replace with (pinned predicate: swallow ONLY when the resynced status is `'failed'`; every
other resynced status rethrows; reset the session flag only on resynced `'idle'`, inside
`runInAction` because it runs after awaits under MobX strict mode):

```ts
    if (this.status === 'failed') {
      // The backend already reports the failure and the error view renders
      // from that status; rethrowing would only duplicate the surface.
      logger.warn(
        'MithrilPartialSyncStore: swallowed partial sync start rejection after backend status resync',
        {
          error: startError,
          status: this.status,
        }
      );
      return;
    }

    if (this.status === 'idle') {
      // The attempt never took hold, so re-arm the session-scoped proactive
      // prompt guard. This mutation resumes after the awaits above, outside
      // the enclosing action's synchronous span, so it needs its own action
      // context under strict mode.
      runInAction(
        'MithrilPartialSyncStore: re-arm prompt after rejected start',
        () => {
          this.mithrilAttemptStartedThisSession = false;
        }
      );
    }

    throw toStartError(startError);
```

`START_PENDING_STATUS` remains in use by the optimistic `_updateStatus` call earlier in the
method — do not remove the constant.

**Step 22.** Same file — the flag's declaration comment now lies. Locate:

```ts
  // Separate session-scoped re-pop guard, set true when a Mithril
  // attempt begins (`startPartialSync`). AND-ed into the proactive-prompt gate so
  // the prompt never re-offers after an attempt regardless of the terminal
  // outcome (completed/cancelled/failed/restart-normal). In-memory, session-scoped,
  // never reset (no idle reset). Distinct from `proactivePromptDismissedThisSession`,
  // which stays single-purpose ("user clicked Standard Sync on the prompt").
```

Replace with:

```ts
  // Separate session-scoped re-pop guard, set true when a Mithril
  // attempt begins (`startPartialSync`). AND-ed into the proactive-prompt gate so
  // the prompt never re-offers after an attempt regardless of the terminal
  // outcome (completed/cancelled/failed/restart-normal). In-memory and
  // session-scoped; reset ONLY when a start rejection resyncs to idle (the
  // attempt never took hold, so the prompt may re-offer). Distinct from
  // `proactivePromptDismissedThisSession`, which stays single-purpose ("user
  // clicked Standard Sync on the prompt").
```

## 2.2 T15 — recovery actions catch + resync; overlay retry wrap

**Step 23.** Same file — locate `cancelPartialSync`:

```ts
  @action
  cancelPartialSync = async () => {
    try {
      await mithrilPartialSyncCancelChannel.request();
    } finally {
```

Replace with:

```ts
  @action
  cancelPartialSync = async () => {
    try {
      await mithrilPartialSyncCancelChannel.request();
    } catch (error) {
      // The resync below reflects the true backend outcome (a failed status
      // drives the error view); the rejection adds no renderer state.
      logger.warn('MithrilPartialSyncStore: cancel partial sync rejected', {
        error,
      });
    } finally {
```

(The existing `finally { ... await this.syncStatus(); }` body and its comment stay untouched.)

**Step 24.** Same file — locate:

```ts
  @action
  restartNormally = async () => {
    await mithrilPartialSyncRestartNormalChannel.request();
  };

  @action
  wipeAndFullSync = async () => {
    await mithrilPartialSyncWipeAndFullSyncChannel.request();
  };
```

Replace with:

```ts
  @action
  restartNormally = async () => {
    try {
      await mithrilPartialSyncRestartNormalChannel.request();
    } catch (error) {
      // Same contract as cancel: resync reflects the backend outcome; no
      // renderer-side error surface is added.
      logger.warn('MithrilPartialSyncStore: restart-normal request rejected', {
        error,
      });
    } finally {
      await this.syncStatus();
    }
  };

  @action
  wipeAndFullSync = async () => {
    try {
      await mithrilPartialSyncWipeAndFullSyncChannel.request();
    } catch (error) {
      logger.warn(
        'MithrilPartialSyncStore: wipe-and-full-sync request rejected',
        { error }
      );
    } finally {
      await this.syncStatus();
    }
  };
```

Invariant (DD-703-11): no new UI surface — the backend `failed` status drives
`MithrilErrorView`; these actions only catch, log, and resync.

**Step 25.** `source/renderer/app/App.tsx` — two edits:

a) Add the logger import. Locate:

```ts
import translations from './i18n/translations';
```

Insert immediately after:

```ts
import { logger } from './utils/logging';
```

b) Wrap the overlay retry so the rethrow from step 21 can never become an unhandled rejection
(retry has no confirmation surface; the resynced backend status drives the error view). Locate:

```tsx
                    onRetry={mithrilPartialSync.startPartialSync}
```

Replace with:

```tsx
                    onRetry={() => {
                      // Retry has no confirmation surface to show a rejection;
                      // the resynced backend status drives the error view.
                      mithrilPartialSync.startPartialSync().catch((error) => {
                        logger.warn(
                          'App: Mithril partial sync retry rejected',
                          { error }
                        );
                      });
                    }}
```

Accept prettier's final line-breaking on this block if it differs. Do not change the other
overlay props. The other `startPartialSync` call sites already handle the rethrow locally
(`MithrilPartialSyncSection.startFromConfirmation` and
`SyncingConnectingMithrilPrompt.handleStart` both catch; verified 2026-07-03) — leave them
untouched.

## 2.3 T21 — behind-ness caches invalidated on chain-directory change

Wiring today (verified): `chainStorageCoordinator._notifyDirectoryChanged()` (fired from
`prepareForLocationChange` and `setDirectory`) → `handleDiskSpace.ts` callback
`resetOnDirectoryChange` → `mithrilController.resetStartupGateOnDirectoryChange()`. Hook the
cache drop into that existing controller method — do NOT add new coordinator callbacks.

**Step 26.** `source/main/mithril/MithrilPartialSyncService.ts` — locate:

```ts
  // Drop both behind-ness input caches so the next probe re-resolves fresh. Called at
  // start()/cancel() entry and inside _resetToIdleStatus() (restart-normal/wipe/finalize paths).
  _invalidateBehindnessCaches(): void {
    this._latestCertifiedImmutableCache = null;
    this._localImmutableCache = null;
  }
```

Replace with:

```ts
  // Drop both behind-ness input caches so the next probe re-resolves fresh. Called at
  // start()/cancel() entry, inside _resetToIdleStatus() (restart-normal/wipe/finalize
  // paths), and on chain-directory changes (onChainDirectoryChanged).
  _invalidateBehindnessCaches(): void {
    this._latestCertifiedImmutableCache = null;
    this._localImmutableCache = null;
  }

  // A chain-directory change swaps the chain store under the probe; the cached
  // inputs describe the old location, so drop them before the next read.
  onChainDirectoryChanged(): void {
    this._invalidateBehindnessCaches();
  }
```

**Step 27.** `source/main/mithril/MithrilController.ts` — locate:

```ts
  resetStartupGateOnDirectoryChange(): void {
    this._startupGate.resetOnDirectoryChange();
  }
```

Replace with:

```ts
  resetStartupGateOnDirectoryChange(): void {
    // The behind-ness caches describe the previous chain directory; drop them
    // together with the startup-gate reset so the next probe re-reads.
    this._partialSyncService.onChainDirectoryChanged();
    this._startupGate.resetOnDirectoryChange();
  }
```

## 2.4 T28 — join the two probe reads

**Step 28.** `source/main/mithril/MithrilPartialSyncService.ts` — inside
`getPartialSyncBehindness`, locate:

```ts
      const latest = await this._getCachedLatestCertifiedImmutableNumber();
      // Cached local read dedupes the checkDiskSpace fork + immutable/ readdir.
      const localImmutableNumber = await this._getCachedLocalImmutableNumber();
```

Replace with:

```ts
      // Cached local read dedupes the checkDiskSpace fork + immutable/ readdir;
      // the aggregator query and the local read are independent, so run them
      // concurrently.
      const [latest, localImmutableNumber] = await Promise.all([
        this._getCachedLatestCertifiedImmutableNumber(),
        this._getCachedLocalImmutableNumber(),
      ]);
```

Invariant: the next line — `const certifiedEpoch = this._getCachedCertifiedEpoch();` — must
stay AFTER the joined await (it reads the beacon cache the latest-certified fetch populates).
Do not reorder it.

## 2.5 T2 — no code change (record only)

DD-703-3 is locked: `mithrilPartialSyncThresholdImmutables = 20` stays as the proactive-prompt
gate (service `_getBehindnessThresholdImmutables()` with
`DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES` fallback — untouched). The manual offer is
unconditional via the chunk-1 changes. The reviewer reply lives in
`task-ux-703-pr-comment-checklist.md` (manual, DD-703-8) — no source edit, no test, nothing for
the implementer to do here. Locked boundary 4 reminder: do not add any renderer-side threshold.

## 2.6 Tests for chunk 2

**Step 29.** `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`:

a) UNCHANGED (verify still green): `it('absorbs raw start request rejections once backend
status reports the failure', ...)` — the failed-swallow branch keeps the exact logger message
`'MithrilPartialSyncStore: swallowed partial sync start rejection after backend status
resync'`, so this test passes as-is. Also unchanged: `it('rethrows the original start failure
while status is still pending', ...)` (stopping-node now rethrows via the new predicate).

b) Add after the rethrow test:

```ts
  it('rethrows a start rejection that resyncs to idle and re-arms the proactive prompt guard', async () => {
    const store = setupStore();
    mockStartRequest.mockRejectedValue(new Error('preflight rejected'));
    mockStatusRequest.mockResolvedValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.startPartialSync()).rejects.toThrow(
      'preflight rejected'
    );

    expect(store.status).toBe('idle');
    expect(store.mithrilAttemptStartedThisSession).toBe(false);
  });

  it('rethrows a start rejection that resyncs to a working status and keeps the attempt guard set', async () => {
    const store = setupStore();
    mockStartRequest.mockRejectedValue(new Error('channel dropped'));
    mockStatusRequest.mockResolvedValue({
      status: 'preparing',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.startPartialSync()).rejects.toThrow('channel dropped');

    expect(store.mithrilAttemptStartedThisSession).toBe(true);
  });
```

c) Update `it('always resyncs after a cancel even when the cancel request rejects', ...)`:
replace

```ts
    await expect(store.cancelPartialSync()).rejects.toThrow('post-cutover');
```

with

```ts
    await expect(store.cancelPartialSync()).resolves.toBeUndefined();
```

and append after the existing assertions:

```ts
    expect(logger.warn).toHaveBeenCalledWith(
      'MithrilPartialSyncStore: cancel partial sync rejected',
      expect.objectContaining({ error: expect.any(Error) })
    );
```

(Use `expect.objectContaining` — a literal object second argument oscillates under the repo's
prettier 2.1.2.)

d) Add rejection tests for the other two recovery actions (after the cancel test):

```ts
  it('resyncs and never throws when the restart-normal request rejects', async () => {
    const store = setupStore();
    mockRestartNormalRequest.mockRejectedValue(new Error('backend refused'));
    mockStatusRequest.mockResolvedValue({
      status: 'failed',
      allowedRecoveryActions: ['retry'],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.restartNormally()).resolves.toBeUndefined();

    expect(mockStatusRequest).toHaveBeenCalledTimes(1);
    expect(store.status).toBe('failed');
    expect(logger.warn).toHaveBeenCalledWith(
      'MithrilPartialSyncStore: restart-normal request rejected',
      expect.objectContaining({ error: expect.any(Error) })
    );
  });

  it('resyncs and never throws when the wipe-and-full-sync request rejects', async () => {
    const store = setupStore();
    mockWipeAndFullSyncRequest.mockRejectedValue(new Error('backend refused'));
    mockStatusRequest.mockResolvedValue({
      status: 'failed',
      allowedRecoveryActions: ['retry'],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.wipeAndFullSync()).resolves.toBeUndefined();

    expect(mockStatusRequest).toHaveBeenCalledTimes(1);
    expect(store.status).toBe('failed');
    expect(logger.warn).toHaveBeenCalledWith(
      'MithrilPartialSyncStore: wipe-and-full-sync request rejected',
      expect.objectContaining({ error: expect.any(Error) })
    );
  });
```

e) Update `it('delegates recovery and lifecycle actions through payload-free IPC
requests', ...)`: `restartNormally` and `wipeAndFullSync` now resync too. Replace:

```ts
    // startPartialSync resyncs once (its finally) and cancelPartialSync now
    // always resyncs too (its finally), so syncStatus fires twice.
    expect(mockStatusRequest).toHaveBeenCalledTimes(2);
```

with:

```ts
    // Every lifecycle action resyncs in its finally (start, cancel,
    // restart-normal, wipe-and-full-sync), so syncStatus fires four times.
    expect(mockStatusRequest).toHaveBeenCalledTimes(4);
```

**Step 30.** `source/main/mithril/MithrilPartialSyncService.spec.ts` — inside
`describe('getPartialSyncBehindness', ...)`, add after `it('invalidates the cached local read
on a lifecycle reset so the next probe re-resolves it', ...)`:

```ts
    it('invalidates both cached reads on a chain-directory change so the next probe re-resolves them', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 5);
      const resolveSpy = jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockResolvedValue(createLatestSnapshot(25));
      const getManagedChainPathSpy = jest.spyOn(
        service._chainStorageManager,
        'getManagedChainPath'
      );
      const nowSpy = jest.spyOn(Date, 'now');
      nowSpy.mockReturnValue(1_000);

      await service.getPartialSyncBehindness();
      expect(getManagedChainPathSpy).toHaveBeenCalledTimes(1);
      expect(resolveSpy).toHaveBeenCalledTimes(1);

      service.onChainDirectoryChanged();

      // No Date.now advance: without invalidation the within-TTL caches would
      // serve the stale values.
      await service.getPartialSyncBehindness();
      expect(getManagedChainPathSpy).toHaveBeenCalledTimes(2);
      expect(resolveSpy).toHaveBeenCalledTimes(2);

      nowSpy.mockRestore();
    });

    it('starts the aggregator query and the local read concurrently', async () => {
      const service = new MithrilPartialSyncService();
      stubLocalImmutableNumber(service, 5);
      let resolveLatest;
      jest
        .spyOn(service, 'resolveLatestSnapshotMetadata')
        .mockImplementation(
          () =>
            new Promise((resolve) => {
              resolveLatest = resolve;
            })
        );
      const getManagedChainPathSpy = jest.spyOn(
        service._chainStorageManager,
        'getManagedChainPath'
      );

      const probe = service.getPartialSyncBehindness();
      // Flush a few microtask turns so both joined reads have started while
      // the aggregator query is still pending; a sequential await would not
      // have touched the local read yet.
      await Promise.resolve();
      await Promise.resolve();
      await Promise.resolve();
      expect(getManagedChainPathSpy).toHaveBeenCalledTimes(1);

      resolveLatest(createLatestSnapshot(25));
      await expect(probe).resolves.toEqual({
        isSignificantlyBehind: true,
        behindByImmutables: 20,
      });
    });
```

Fallback for the concurrency test ONLY: if the microtask flush proves unreliable in this spec's
environment (e.g. fake-timer interference), keep the `Promise.all` code change, drop this one
test, and note the drop in the implementation log — the directory-change and TTL tests still
cover the method. Do not weaken any other test to make it pass.

**Step 31.** `source/main/mithril/MithrilController.spec.ts`:

a) Locate:

```ts
const mockForceKillForShutdown = jest.fn();
```

Insert after:

```ts
const mockOnChainDirectoryChanged = jest.fn();
```

b) In the `jest.mock('./MithrilPartialSyncService', ...)` factory, locate:

```ts
    forceKillForShutdown: (...args) => mockForceKillForShutdown(...args),
```

Insert after:

```ts
    onChainDirectoryChanged: (...args) => mockOnChainDirectoryChanged(...args),
```

c) Add a new describe after `describe('getPartialSyncAvailability (main-side probe
guard)', ...)`:

```ts
  describe('resetStartupGateOnDirectoryChange', () => {
    it('drops the behind-ness caches together with the startup-gate reset', () => {
      const controller = createController();

      controller.resetStartupGateOnDirectoryChange();

      expect(mockOnChainDirectoryChanged).toHaveBeenCalledTimes(1);
      expect(
        (controller as any)._startupGate.resetOnDirectoryChange
      ).toHaveBeenCalledTimes(1);
    });
  });
```

(If `(controller as any)._startupGate` is not reachable in this spec's typing setup, keep only
the `mockOnChainDirectoryChanged` assertion — the gate-reset delegation is pre-existing
behavior.)

---

# Verification

Environment prep (known Node-24 gotchas — apply BEFORE treating failures as regressions):

- If `tsc` fails on a missing/stale generated `*.scss.d.ts`, regenerate just the touched file
  with `node_modules/.bin/typed-scss-modules <file.scss>` — do not run the full precompile glob.
- If a focused jest run on a component spec fails on `.scss` imports, use the gitignored
  identity-obj-proxy scss sidecar `--config` used by prior tasks in this sprint (do NOT stage
  it, do NOT edit `.gitignore`).

Commands (narrow, in order):

```bash
yarn test:jest --testPathPattern="MithrilPartialSyncService"
yarn test:jest --testPathPattern="MithrilController"
yarn test:jest --testPathPattern="MithrilPartialSyncStore"
yarn test:jest --testPathPattern="MithrilPartialSyncSection"
# Locked boundary 11 — bootstrap must stay green (no bootstrap files are touched; this is a guard):
yarn test:jest --testPathPattern="MithrilBootstrap"
yarn i18n:manage
yarn compile
```

Trim command output to the failing tail when reporting. Prettier note: new jest assertions with
an object second argument must use `expect.objectContaining` (prettier 2.1.2 oscillation);
accept prettier's formatting on the App.tsx retry block.

---

# Files touched

Source:

- `source/common/types/mithril-partial-sync.types.ts` (optional `isProbeFailed` on availability type)
- `source/main/mithril/MithrilPartialSyncService.ts` (probe return type + catch flag; `onChainDirectoryChanged`; `Promise.all` join)
- `source/main/mithril/MithrilController.ts` (directory-change cache drop; availability pass-through is spread-automatic — no edit in `getPartialSyncAvailability`)
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` (`isProbeFailed` observable + apply; start-rejection predicate + flag reset; recovery-action catch/resync; flag-comment update)
- `source/renderer/app/App.tsx` (logger import; `onRetry` wrap)
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` (new prop)
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` (prop type/destructure/render; N7 inner re-check removed)
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` (props; always-render; variant computation)
- `source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx` (variant prop + two new messages + tooltip selection)
- `source/renderer/app/i18n/locales/en-US.json` (2 new keys)
- `source/renderer/app/i18n/locales/ja-JP.json` (2 new keys)
- `translations/messages.json` (regenerated by `yarn i18n:manage`)

Tests:

- `source/main/mithril/MithrilPartialSyncService.spec.ts`
- `source/main/mithril/MithrilController.spec.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx`

# Out of scope (owned by other sections)

- CAT-A: widening what the backend EMITS in `allowedRecoveryActions`.
- CAT-C (T13): finalize-failure path + its copy key; `MithrilPartialSyncOverlay.tsx` /
  `MithrilProgressView.tsx` edits.
- CAT-D (T18): insufficient-disk-space copy key and `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE`
  error-code union append.
- CAT-E: all remaining copy work, incl. T22 shared start-failure fallback message
  (`partialSyncErrorCopy.ts`) replacing the hardcoded fallback in
  `MithrilPartialSyncSection.tsx` (`'Unable to start Mithril partial sync.'` — left untouched
  here), T16 coded errors in `chainStorageCoordinator.ts`/service prose throws, and further
  error-code union appends.
- CAT-F: T23 `mithrilErrorMessage.ts` helper; T26 spawn-helper extraction in
  `mithrilCommandRunner.ts`.
- CAT-G: N4 (overlay/progress-view props) and N3 (coordinator/controller seams) — G locates
  anchors by content after this section's edits; N7 is DONE here, G must not re-plan it.
- `task-ux-703-pr-comment-checklist.md` reply texts (manual, DD-703-8).

# Acceptance checks

- **T11 (DD-703-2/-10):** Diagnostics Mithril section visible whenever the feature is enabled,
  in all three probe states, each with distinct copy (behind = existing recommendation;
  near tip = `...RecommendationNearTip`; probe failed = `...RecommendationUnknown`), CTA enabled
  in all three; `isProbeFailed` flows service → controller → IPC → store → component
  (steps 1–20; specs in steps 17–20). Kill switch still hides the section entirely.
- **N7:** inner `isMithrilPartialSyncEnabled` re-check inside the old
  `shouldShowRecommendation` expression removed; outer gate intact (step 10).
- **T12/T31:** a start-IPC rejection rethrows after resync unless the resynced status is
  `'failed'`; a preflight rejection surfaces in `startFromConfirmation` (existing section spec
  `'keeps confirmation open and shows concrete start failure'` stays green); on resynced
  `'idle'` the session flag resets inside `runInAction` so the proactive prompt re-arms
  (steps 21–22; tests step 29a/b).
- **T15 (DD-703-11):** each of cancel/restart-normal/wipe-and-full-sync catches its rejection,
  resyncs, logs, and never throws — with a rejecting IPC stub per action (steps 23–24; tests
  step 29c/d/e); overlay retry wrapped so no Mithril button can produce an unhandled rejection
  (step 25).
- **T21:** after a chain-directory change the next behind-ness probe re-reads instead of
  serving the TTL cache (steps 26–27; tests steps 30–31).
- **T28:** aggregator query and local read joined via `Promise.all`;
  `_getCachedCertifiedEpoch()` stays after the joined await (step 28; test step 30).
- **T2 (DD-703-3):** answered with no code change — threshold constant untouched (section 2.5).
- **Locked boundary 11:** no bootstrap file is touched; `MithrilBootstrap` specs run as a guard.

# Escalations

- **T21 wiring drift (trivial, resolved in-plan):** the brief's anchor implied
  `chainStorageCoordinator` calls the controller directly; the live wiring routes through
  `handleDiskSpace.ts` (`chainStorageCoordinator.onDirectoryChanged(resetOnDirectoryChange)` →
  `mithrilController.resetStartupGateOnDirectoryChange()`). The plan hooks the exact existing
  wiring point the brief named (`MithrilController.resetStartupGateOnDirectoryChange`), so the
  fix is faithful; no decision needed. Escalate only if that handleDiskSpace registration is
  gone at implementation time.
- **T28 concurrency test (escalate-on-flake):** the microtask-flush assertion in step 30 depends
  on `_getCachedLocalImmutableNumber` reaching `getManagedChainPath` within three microtask
  turns. If it flakes, apply the stated fallback (drop that one test, keep the code change) and
  record it — do not add timers or retries.
- **Controller spec private access (minor):** step 31c reads
  `(controller as any)._startupGate`; if the spec's lint/type setup rejects it, keep only the
  service-mock assertion as stated inline.
- **JA copy (flagged, not blocking):** the near-tip first sentence
  (「ノードはブロックチェーンの先端に近い状態です。」) has no exact copy-table precedent; it is derived
  from table row 3's tip-position pattern. Use as written; native-speaker review can refine
  post-merge.
