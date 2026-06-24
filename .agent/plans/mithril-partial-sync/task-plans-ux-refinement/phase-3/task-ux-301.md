# task-ux-301 — Consume availability in the store, gate all partial-sync UI, and re-arm the CTA via `isWorking`

- Sprint: Mithril Partial Sync UX Refinement — phase-3 (Renderer Discovery Gating & CTA Re-arm)
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved`
- Build status: `completed`
- Interaction mode: `autonomous`
- Priority: critical · Estimated: 4h · Dependencies: task-ux-101 (completed), task-ux-102 (completed)

## Why now
Phase-1 built the cross-process availability contract (task-ux-101: type + dedicated channel +
kill-switch exposure; task-ux-102: backend-computed `isSignificantlyBehind`/`behindByImmutables`).
That contract is currently UNCONSUMED in the renderer. Today `MithrilPartialSyncSection` renders
**unconditionally** (`DaedalusDiagnostics.tsx:704-711`) and the CTA gates only on
`isActionBlocked = isMithrilPartialSyncActive || isMithrilBootstrapActive`
(`DaedalusDiagnostics.tsx:565-566`), which never consults the kill switch. On a kill-switch-off
build the recommendation + an enabled Start button still render; clicking sends `start`, which the
coordinator rejects with the raw string `"Mithril partial sync is disabled by launcher
configuration."`, rendered verbatim by `MithrilPartialSyncConfirmation.tsx:74` — research-19
blocker #1 (row 128) / §2 critical caveat (lines 115-120).

Additionally, after a *successful* sync the CTA stays stuck disabled until app restart, because the
container passes `mithrilPartialSync.isActive` (`DaedalusDiagnosticsDialog.tsx:133`) and
`isActive = (status !== 'idle')` (`isMithrilPartialSyncActiveStatus`,
`mithril-partial-sync.types.ts:98-100`) treats terminal `completed`/`cancelled`/`failed` as active —
research-19 gap #40, PRD D9/BUG1.

This task closes blocker #1's RENDERER half and the CTA re-arm half of D9/BUG1: consume the
availability read model in `MithrilPartialSyncStore`, gate ALL partial-sync UI on `isEnabled` (hide,
not disable — D1/D3), additionally gate the recommendation/CTA on `isSignificantlyBehind`, and derive
`isActionBlocked` from `isWorking` (in-flight phases) instead of `isActive`.

## Interaction mode justification
`autonomous`: this is renderer wiring of an ALREADY-BUILT backend availability contract plus a
one-line derivation change, with no product/UX judgment. The availability channel, its type, the
renderer wrapper (`mithrilPartialSyncAvailabilityChannel`), and the `isWorking` seam
(`isMithrilPartialSyncWorkingStatus`) all already exist verbatim in the repo. No user-facing copy
decision (that is task-ux-303), no destructive operation, no operator validation, no network call.
The hide-not-disable and `isEnabled && isSignificantlyBehind` gating rules are pre-decided by PRD
D1/D3; the `isWorking` derivation is pre-decided by PRD D9/BUG1. No genuine blocking product/UX
decision is required.

## Scope
1. `MithrilPartialSyncStore.ts`: add three observable availability fields
   (`isPartialSyncEnabled`, `isSignificantlyBehind`, `behindByImmutables`), a private
   `_refreshAvailability()` action that one-shot queries `mithrilPartialSyncAvailabilityChannel`, a
   one-shot call in `setup()`, and a low-frequency `setInterval` refresh that runs only while
   `isWorking`, started in `setup()` and cleared in `teardown()`.
2. `DaedalusDiagnosticsDialog.tsx` (container): pass `mithrilPartialSync.isWorking` as the
   partial-sync half of the blocked signal (NOT `isActive`), and thread two new props
   `isMithrilPartialSyncEnabled` + `isMithrilPartialSyncSignificantlyBehind` into `DaedalusDiagnostics`.
3. `DaedalusDiagnostics.tsx` (component): derive `isMithrilActionBlocked` from the new working-based
   prop; render `MithrilPartialSyncSection` ONLY when `isMithrilPartialSyncEnabled`; pass the
   recommendation/CTA gate (`isMithrilPartialSyncEnabled && isMithrilPartialSyncSignificantlyBehind`)
   down to the section.
4. `MithrilPartialSyncSection.tsx`: accept a `shouldShowRecommendation` boolean and render its
   recommendation row (header + recommendation/CTA) only when true; when false the Section returns
   `null` for its non-confirmation path so NO empty labeled "Mithril Partial Sync:" header row renders
   near the tip. `MithrilPartialSyncRecommendation.tsx` is NOT edited — gating at the Section level is
   sufficient and avoids the residual header row (a `null` inside the recommendation would leave the
   Section's header behind it).
5. `mithril-partial-sync.types.ts`: NO type change required — `MithrilPartialSyncAvailability`
   (lines 56-60) and `isMithrilPartialSyncWorkingStatus` (lines 86-88) already exist. This file is in
   targetPaths only because the implementer must READ those exports; it is not edited (see Non-goals).

## Non-goals (explicitly out of this task)
- **NO new IPC channel.** Reuse the existing `mithrilPartialSyncAvailabilityChannel`
  (`source/renderer/app/ipc/mithrilPartialSyncChannel.ts:53-56`). This task adds ZERO new IPC.
- **NO threshold math in the renderer.** Consume the boolean `isSignificantlyBehind` and the figure
  `behindByImmutables` as-is; never compare `behindByImmutables` against any number. Backend owns the
  threshold (lock #4, PRD D2).
- **NO overlay dismiss / finalize wiring.** The dismiss → backend `finalize` channel that resets to
  idle + cleans staging is task-ux-404 (PRD D9 dismiss seam). This task ONLY re-arms the CTA via the
  `isWorking` derivation; do not touch `dismissCompletedOverlay`, the overlay, or add any finalize
  channel.
- **NO confirmation-modal work** (snapshot metadata, button hierarchy, deep-link `showConfirmationOnOpen`
  prop) — that is task-ux-303 / D1 handoff.
- **NO proactive loading-screen prompt** — that is task-ux-302 (D1 surface). This task gates only the
  diagnostics recommendation/CTA; confirmed no proactive-prompt component exists yet.
- **Do NOT touch the shared bootstrap progress components** (`MithrilProgressView`,
  `MithrilStepIndicator`, `MithrilBootstrapStore`) — that is D4 (task-ux-302/4xx).
- **Do NOT change the type file** (`mithril-partial-sync.types.ts`) — the type and the `isWorking`
  helper already exist; no edit needed.
- **Do NOT remove `isActive`/`isMithrilPartialSyncActiveStatus`.** It still has a live consumer
  (`shouldCloseDiagnosticsForPartialSyncOverlay` uses overlay-status, not `isActive`, but `isActive`
  remains the store getter for `DaedalusDiagnosticsDialog` history and the store spec at
  `MithrilPartialSyncStore.spec.ts:141`). Leave the getter intact; only stop using it for the CTA block.

## Dependencies
- task-ux-101 (completed) — `MithrilPartialSyncAvailability` type, `MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL`,
  renderer wrapper `mithrilPartialSyncAvailabilityChannel` (one-shot `.request()`).
- task-ux-102 (completed) — backend populates `isSignificantlyBehind` + `behindByImmutables`.

## Research / docs / workflows consulted
- Tasks JSON `task-ux-301` — acceptance + 4 testCases carried verbatim below.
- PRD D1 (`...-prd.md:32-95`) — hide-not-disable; gate the manual trigger on `isEnabled &&
  isSignificantlyBehind`; near-tip → no offer. PRD D3 (`...-prd.md:130-163`) — availability read
  model; gate ALL UI on `isEnabled`. PRD D9/BUG1 (`...-prd.md:402-416`, requirements `:663-664`) —
  renderer derives `isActionBlocked` from `isWorking` not `isActive`.
- Research-19 blocker #1 (row 128, §2 lines 95-120), gap #40 (`isActive` stuck-active anchor cited at
  `mithril-partial-sync.types.ts:92-94` — DRIFTED, see "Anchor drift" below), row #15 (the
  `_isTornDown` inert-handler disposition — keep, do not add unsubscribe), row #24 (retry = start
  reuse — document, no change here).
- `.agent/workflows/frontend.md` (MobX `@observable`/`@computed`/`@action`, store setup/teardown).
- `.agent/system/state-management.md` (store lifecycle: `setup()` runs on `initialize()`,
  `teardown()` on reset; `registerReactions`).
- Live `NetworkStatusStore.ts:443-459` — the existing `setInterval`/`clearInterval` polling pattern
  this task mirrors for the periodic refresh (the partial-sync store has no polling today; status is
  push-driven via `onReceive`, research-19 lines 47-56).

## Anchor drift recorded
- Tasks JSON and research-19 gap #40 cite `isMithrilPartialSyncActiveStatus` (the `status !== 'idle'`
  helper) at `mithril-partial-sync.types.ts:92-94`. **Live location is `:98-100`.** The `isWorking`
  helper `isMithrilPartialSyncWorkingStatus` is at `:86-88` (matches the brief). The live file is
  authoritative; all anchors in this plan are verified against it as of 2026-06-24.

## Files expected to change (exact paths + verified anchors)
1. `source/renderer/app/stores/MithrilPartialSyncStore.ts` — add availability fields + refresh
   action + interval; one-shot in `setup()` (currently `:63-70`); clear interval in `teardown()`
   (currently `:72-75`).
2. `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` — line 133 (`isActive` →
   `isWorking`) + two new props near `:133-137`.
3. `source/renderer/app/components/status/DaedalusDiagnostics.tsx` — Props type `:409-411`; render
   destructure `:519-520`; `isMithrilActionBlocked` derivation `:565-566`; Section render `:704-711`.
4. `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` — Props `:10-17`;
   `componentDidUpdate` `:43-51` (rename prop); render `:100-130` (add `shouldShowRecommendation` to
   Props/destructure + a Section-level `null` return for the non-confirmation row when the gate is
   false — Step 12b). The gate lives HERE so no empty labeled header row renders near the tip.
5. `source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx` — **READ ONLY** (no
   edit). It is in targetPaths but the gate is enforced one level up in the Section (Step 13), so this
   file is unchanged; Props `:42-47` / render `:54-93` stay as-is.
6. `source/common/types/mithril-partial-sync.types.ts` — **READ ONLY** (no edit); type at `:56-60`,
   `isMithrilPartialSyncWorkingStatus` at `:86-88`.
7. Test files (Verification): `MithrilPartialSyncStore.spec.ts`, `DaedalusDiagnostics.spec.tsx`,
   `MithrilPartialSyncSection.spec.tsx` (+ `DaedalusDiagnosticsDialog.spec.ts` mock update — see
   Verification).

## DESIGN DECISIONS (resolved — implementer makes none)

### DD1 — Where gating lives
The store exposes availability; the container (`DaedalusDiagnosticsDialog`) reads the store and passes
booleans down as props (consistent with how `isMithrilPartialSyncActive`/`onStartMithrilPartialSync`
flow today at `:133-137`). `DaedalusDiagnostics` (the presentational component) does the conditional
render. Rationale: `DaedalusDiagnostics` is a plain `@observer` component that receives everything via
props (`DaedalusDiagnostics.spec.tsx` renders it with a flat props object, no stores); keeping the
store read in the container preserves that and keeps the component unit-testable.

### DD2 — Two-level gate
- `isEnabled` (`isMithrilPartialSyncEnabled` prop) gates **ALL** partial-sync UI → the entire
  `<MithrilPartialSyncSection>` element is rendered only when `isMithrilPartialSyncEnabled` is true
  (lock #10, PRD D3). When off, NOTHING renders for partial sync — no row, no button.
- `isEnabled && isSignificantlyBehind` gates the **recommendation/CTA specifically** → threaded as
  `shouldShowRecommendation` so the recommendation copy + Start button render only when the user is
  both enabled AND significantly behind (PRD D1/D3). Because the whole Section is already gated on
  `isEnabled`, `shouldShowRecommendation` effectively reduces to `isSignificantlyBehind` inside the
  Section, but we pass the full `isEnabled && isSignificantlyBehind` expression for clarity and so the
  Section is correct in isolation.

  NOTE on the confirmation panel: `MithrilPartialSyncSection` swaps the recommendation for the inline
  confirmation panel when `isShowingConfirmation` is true (`:105-114`). Confirmation can only be
  opened from the (now-gated) recommendation button, so gating the recommendation is sufficient; do
  NOT additionally gate the `isShowingConfirmation` branch (a confirmation already in progress must
  finish rendering).

  NOTE on the empty-row defect (Critiquer blocker #1): the gate must short-circuit at the SECTION
  level, not only inside `MithrilPartialSyncRecommendation`. `MithrilPartialSyncSection.render()`
  (`:116-129`) emits a `styles.layoutRow` wrapper containing the `styles.layoutHeader` "Mithril
  Partial Sync:" label BEFORE delegating to `MithrilPartialSyncRecommendation`. If the gate lived only
  inside the recommendation (returning `null`), the Section would still render a bare labeled "Mithril
  Partial Sync:" row with empty content when `isEnabled && !isSignificantlyBehind` — a dead/empty
  partial-sync surface near the tip that contradicts D1 ("there is never a dead control near the tip",
  `prd:87-88`). Therefore the Section itself returns `null` for its non-confirmation path when
  `!shouldShowRecommendation` (Step 12b). The Section's ONLY non-confirmation content is the
  recommendation row, so suppressing the whole row when the recommendation is gated off is correct and
  leaves no header. The `isShowingConfirmation` branch is unaffected (it returns before the gate).

### DD3 — `isActionBlocked` new derivation
`isActionBlocked` is computed in `DaedalusDiagnostics.tsx:565-566`:
```ts
const isMithrilActionBlocked =
  isMithrilPartialSyncActive || isMithrilBootstrapActive;
```
Change the partial-sync term from `isActive` to `isWorking`. The simplest correct change is at the
**container** (`DaedalusDiagnosticsDialog.tsx:133`): pass `mithrilPartialSync.isWorking` into the
existing `isMithrilPartialSyncActive` prop slot — but to keep names honest, RENAME the prop to
`isMithrilPartialSyncWorking` end-to-end (container → `DaedalusDiagnostics` Props → render). The
`|| isMithrilBootstrapActive` half (the serialized-mutation-lock disable that also blocks while
bootstrap work runs) is PRESERVED unchanged (lock #14). Result: terminal `completed`/`cancelled`/
`failed` are NOT working → `isActionBlocked` clears → CTA re-arms with no restart (D9/BUG1).

The `isMithrilPartialSyncActive` prop is also consumed by
`MithrilPartialSyncSection.componentDidUpdate` (`:43-51`) to auto-close the confirmation when work
becomes active. Rename that prop too (`isMithrilPartialSyncWorking`) and feed it from
`mithrilPartialSync.isWorking`; semantics are equivalent for the confirmation-close (work starting is
the relevant transition), and consistent naming avoids two different booleans.

### DD4 — Periodic-refresh mechanism
The partial-sync store has NO existing polling/interval/reaction (status is push-driven via
`onReceive`, research-19 lines 47-56). Mirror `NetworkStatusStore`'s `setInterval`/`clearInterval`
pattern (`NetworkStatusStore.ts:443-459`). Add a single private interval field
`_availabilityRefreshInterval`, started in `setup()` and cleared in `teardown()`. The interval
callback refreshes availability ONLY while `this.isWorking` is true (behind-ness can move while a sync
runs; near-idle it is static and the one-shot setup query suffices). Use a low frequency
(`AVAILABILITY_REFRESH_INTERVAL = 30_000` ms) — this is not a tight poll (PRD D2/D3 "periodic refresh
while syncing, not a tight poll"). The one-shot setup `request()` covers the common idle case.

## Implementation approach — ordered, mechanical steps

### Step 1 — Store: imports
File: `source/renderer/app/stores/MithrilPartialSyncStore.ts`.
1a. Add `MithrilPartialSyncAvailability` to the existing TYPE import block at `:2-6` (currently):
```ts
import type {
  MithrilPartialSyncFailureAction,
  MithrilPartialSyncStatus,
  MithrilPartialSyncStatusSnapshot,
} from '../../../common/types/mithril-partial-sync.types';
```
→ add `MithrilPartialSyncAvailability,` to that list.
1b. Add `mithrilPartialSyncAvailabilityChannel` to the value import block at `:14-20` (currently
imports the five action/status channels). Insert it alphabetically/adjacent, e.g. after
`mithrilPartialSyncStatusChannel,`:
```ts
  mithrilPartialSyncAvailabilityChannel,
```
(Verify the exact name in `source/renderer/app/ipc/mithrilPartialSyncChannel.ts:53` —
`mithrilPartialSyncAvailabilityChannel` exporting a `RendererIpcChannel` with `.request()`.)

### Step 2 — Store: module constant
File: `MithrilPartialSyncStore.ts`. After `START_PENDING_STATUS` (`:31`), add:
```ts
const AVAILABILITY_REFRESH_INTERVAL = 30_000;
```

### Step 3 — Store: observable fields + interval field
File: `MithrilPartialSyncStore.ts`. After the existing observable block (last is
`isCompletedOverlayDismissed` at `:60`) and the `_isTornDown = false;` field (`:61`), add:
```ts
  @observable isPartialSyncEnabled = false;
  @observable isSignificantlyBehind = false;
  @observable behindByImmutables: number | undefined = undefined;
  _availabilityRefreshInterval: ReturnType<typeof setInterval> | null = null;
```
Default `isPartialSyncEnabled = false` is the SAFE default: until the first availability response
lands, UI stays hidden (fail-closed, lock #10). Name it `isPartialSyncEnabled` (not `isEnabled`) to
avoid collision with any generic store concept and to read clearly at the call site.

### Step 4 — Store: `_refreshAvailability` action
File: `MithrilPartialSyncStore.ts`. Add as an `@action` arrow method (place after
`_updateStatus` ends at `:147`, before `dismissCompletedOverlay`):
```ts
  @action
  _refreshAvailability = async () => {
    if (this._isTornDown) {
      return;
    }

    try {
      const availability: MithrilPartialSyncAvailability =
        await mithrilPartialSyncAvailabilityChannel.request();
      this._applyAvailability(availability);
    } catch (error) {
      logger.warn('MithrilPartialSyncStore: failed to refresh availability', {
        error,
      });
    }
  };

  @action
  _applyAvailability = (availability: MithrilPartialSyncAvailability) => {
    if (this._isTornDown) {
      return;
    }

    this.isPartialSyncEnabled = availability.isEnabled;
    this.isSignificantlyBehind = availability.isSignificantlyBehind;
    this.behindByImmutables = availability.behindByImmutables;
  };
```
Mirror the existing `syncStatus`/`_updateStatus` pairing (`:117-147`): one async fetcher guarded by
`_isTornDown`, one pure `@action` applier also guarded by `_isTornDown`. The `_isTornDown` guard on
`_applyAvailability` keeps a late-resolving `request()` inert after teardown — same disposition as
the status handler (research-19 row #15).
Locked invariant: do NOT compare `behindByImmutables` to any threshold; store it verbatim for copy
use (lock #4). This task only stores it; copy is task-ux-303.

### Step 5 — Store: `setup()` one-shot + interval start
File: `MithrilPartialSyncStore.ts`. Current `setup()` is `:63-70`:
```ts
  setup() {
    mithrilPartialSyncStatusChannel.onReceive(async (update) => {
      this._updateStatus(update);
    });
    this.syncStatus().catch((error) => {
      logger.warn('MithrilPartialSyncStore: failed to sync status', { error });
    });
  }
```
Append, inside `setup()`, after the `syncStatus()` call:
```ts
    this._refreshAvailability();
    this._availabilityRefreshInterval = setInterval(() => {
      if (this.isWorking) {
        this._refreshAvailability();
      }
    }, AVAILABILITY_REFRESH_INTERVAL);
```
(`_refreshAvailability` already swallows its own rejection, so no extra `.catch` is needed; the
one-shot covers the idle case, the interval covers in-flight behind-ness drift.)

### Step 6 — Store: `teardown()` clear interval
File: `MithrilPartialSyncStore.ts`. Current `teardown()` is `:72-75`:
```ts
  teardown() {
    this._isTornDown = true;
    super.teardown();
  }
```
Change to clear the interval BEFORE setting the guard / calling super:
```ts
  teardown() {
    this._isTornDown = true;
    if (this._availabilityRefreshInterval) {
      clearInterval(this._availabilityRefreshInterval);
      this._availabilityRefreshInterval = null;
    }
    super.teardown();
  }
```
DOCUMENT (in the impl review): the `onReceive` status subscription from `setup()` is STILL not
unsubscribed — research-19 row #15 keeps the `_isTornDown` guard as the inert-handler mechanism
(no `removeListener` on `IpcReceiver`); this task adds NO IPC-abstraction unsubscribe. The new
interval IS cleared here (it is a plain `setInterval` we own, unlike the IPC subscription).

### Step 7 — Container: pass `isWorking` + availability props
File: `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`.
Current `:133-137`:
```tsx
          isMithrilPartialSyncActive={mithrilPartialSync.isActive}
          isMithrilBootstrapActive={isMithrilBootstrapBlockingNodeStart(
            mithrilBootstrap.status
          )}
          onStartMithrilPartialSync={mithrilPartialSync.startPartialSync}
```
Change the partial-sync prop to feed `isWorking` and rename it; add the two availability props:
```tsx
          isMithrilPartialSyncWorking={mithrilPartialSync.isWorking}
          isMithrilPartialSyncEnabled={mithrilPartialSync.isPartialSyncEnabled}
          isMithrilPartialSyncSignificantlyBehind={
            mithrilPartialSync.isSignificantlyBehind
          }
          isMithrilBootstrapActive={isMithrilBootstrapBlockingNodeStart(
            mithrilBootstrap.status
          )}
          onStartMithrilPartialSync={mithrilPartialSync.startPartialSync}
```
(`@observer` on the container makes the new observables reactive automatically.)

### Step 8 — `DaedalusDiagnostics` Props type
File: `source/renderer/app/components/status/DaedalusDiagnostics.tsx`. Current `:409-411`:
```ts
  isMithrilPartialSyncActive: boolean;
  isMithrilBootstrapActive: boolean;
  onStartMithrilPartialSync: (...args: Array<any>) => any;
```
Replace `isMithrilPartialSyncActive` and add the two new props:
```ts
  isMithrilPartialSyncWorking: boolean;
  isMithrilPartialSyncEnabled: boolean;
  isMithrilPartialSyncSignificantlyBehind: boolean;
  isMithrilBootstrapActive: boolean;
  onStartMithrilPartialSync: (...args: Array<any>) => any;
```

### Step 9 — `DaedalusDiagnostics` render destructure
File: `DaedalusDiagnostics.tsx`. Current `:519-520`:
```ts
      isMithrilPartialSyncActive,
      isMithrilBootstrapActive,
```
Replace with:
```ts
      isMithrilPartialSyncWorking,
      isMithrilPartialSyncEnabled,
      isMithrilPartialSyncSignificantlyBehind,
      isMithrilBootstrapActive,
```

### Step 10 — `DaedalusDiagnostics` blocked derivation
File: `DaedalusDiagnostics.tsx`. Current `:565-566`:
```ts
    const isMithrilActionBlocked =
      isMithrilPartialSyncActive || isMithrilBootstrapActive;
```
Replace with:
```ts
    const isMithrilActionBlocked =
      isMithrilPartialSyncWorking || isMithrilBootstrapActive;
```

### Step 11 — `DaedalusDiagnostics` gated Section render
File: `DaedalusDiagnostics.tsx`. Current `:704-711`:
```tsx
              <MithrilPartialSyncSection
                formattedSyncPercentage={formattedSyncPercentage}
                isActionBlocked={isMithrilActionBlocked}
                isMithrilPartialSyncActive={isMithrilPartialSyncActive}
                isSynced={isSynced}
                onRestoreFocus={this.restoreDialogCloseOnEscKey}
                onStartMithrilPartialSync={this.props.onStartMithrilPartialSync}
              />
```
Replace with a kill-switch-gated render that also passes the recommendation gate and the renamed
working prop:
```tsx
              {isMithrilPartialSyncEnabled && (
                <MithrilPartialSyncSection
                  formattedSyncPercentage={formattedSyncPercentage}
                  isActionBlocked={isMithrilActionBlocked}
                  isMithrilPartialSyncWorking={isMithrilPartialSyncWorking}
                  isSynced={isSynced}
                  shouldShowRecommendation={
                    isMithrilPartialSyncEnabled &&
                    isMithrilPartialSyncSignificantlyBehind
                  }
                  onRestoreFocus={this.restoreDialogCloseOnEscKey}
                  onStartMithrilPartialSync={this.props.onStartMithrilPartialSync}
                />
              )}
```
When `isMithrilPartialSyncEnabled` is false the entire Section is absent (lock #10 — ALL partial-sync
UI hidden). When enabled but not significantly behind, the Section mounts but renders nothing for the
recommendation (handled in Step 13). NOTE: keep the `{getRow('syncPercentage', …)}` line above and
`{getRow('lastNetworkBlock', …)}` line below unchanged.

### Step 12 — `MithrilPartialSyncSection` Props + thread the gate
File: `source/renderer/app/components/status/MithrilPartialSyncSection.tsx`. Current Props `:10-17`:
```ts
type Props = {
  formattedSyncPercentage: string;
  isActionBlocked: boolean;
  isMithrilPartialSyncActive: boolean;
  isSynced: boolean;
  onRestoreFocus: () => void;
  onStartMithrilPartialSync: (...args: Array<any>) => any;
};
```
Replace `isMithrilPartialSyncActive` with `isMithrilPartialSyncWorking` and add
`shouldShowRecommendation`:
```ts
type Props = {
  formattedSyncPercentage: string;
  isActionBlocked: boolean;
  isMithrilPartialSyncWorking: boolean;
  isSynced: boolean;
  shouldShowRecommendation: boolean;
  onRestoreFocus: () => void;
  onStartMithrilPartialSync: (...args: Array<any>) => any;
};
```
12a. Update `componentDidUpdate` (`:43-51`) to use the renamed prop:
```tsx
  componentDidUpdate(prevProps: Props) {
    if (
      this.state.isShowingConfirmation &&
      !prevProps.isMithrilPartialSyncWorking &&
      this.props.isMithrilPartialSyncWorking
    ) {
      this.hideConfirmation();
    }
  }
```
12b. In `render()` (`:100-130`), destructure `shouldShowRecommendation` from props, then — AFTER the
existing `isShowingConfirmation` early-return — short-circuit the whole non-confirmation row with
`null` when `!shouldShowRecommendation`. This kills the empty labeled "Mithril Partial Sync:" header
row (Critiquer blocker #1), because the Section's only non-confirmation content is the recommendation
row.

Current render destructure `:101`:
```ts
    const { formattedSyncPercentage, isActionBlocked, isSynced } = this.props;
```
→
```ts
    const {
      formattedSyncPercentage,
      isActionBlocked,
      isSynced,
      shouldShowRecommendation,
    } = this.props;
```
Current `isShowingConfirmation` early-return (`:105-114`) stays UNCHANGED (DD2: a confirmation in
progress must finish rendering; it is only reachable from the gated recommendation button anyway).
Immediately AFTER that early-return block and BEFORE the `return (<div className={styles.layoutRow}>…`
(`:116`), insert:
```tsx
    if (!shouldShowRecommendation) {
      return null;
    }
```
The `<MithrilPartialSyncRecommendation …>` element (`:122-127`) is left as-is structurally; it no
longer needs a `shouldShowRecommendation` prop because the Section already suppresses the entire row
when the gate is false (see Step 13 — the prop on the recommendation becomes redundant and is NOT
added). Resulting render shape:
```tsx
  render() {
    const {
      formattedSyncPercentage,
      isActionBlocked,
      isSynced,
      shouldShowRecommendation,
    } = this.props;
    const { isShowingConfirmation, startError } = this.state;
    const { intl } = this.context;

    if (isShowingConfirmation) {
      return (
        <MithrilPartialSyncConfirmation
          isActionBlocked={isActionBlocked}
          startError={startError}
          onCancel={this.hideConfirmation}
          onConfirm={this.startFromConfirmation}
        />
      );
    }

    if (!shouldShowRecommendation) {
      return null;
    }

    return (
      <div className={styles.layoutRow}>
        <div className={styles.layoutHeader}>
          Mithril Partial Sync
          {intl.formatMessage(globalMessages.punctuationColon)}
        </div>
        <MithrilPartialSyncRecommendation
          formattedSyncPercentage={formattedSyncPercentage}
          isActionBlocked={isActionBlocked}
          isSynced={isSynced}
          onShowConfirmation={this.showConfirmation}
        />
      </div>
    );
  }
```

### Step 13 — `MithrilPartialSyncRecommendation` — NO CHANGE
File: `source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx`.
This file is NOT edited. The gate is enforced ONE level up in `MithrilPartialSyncSection.render()`
(Step 12b returns `null` for the entire non-confirmation row when `!shouldShowRecommendation`), so the
recommendation component is only ever mounted when it SHOULD render. Pushing the gate to the Section
(rather than into the recommendation) is what kills the empty labeled header row — a `null` return
inside `MithrilPartialSyncRecommendation` would leave the Section's `styles.layoutRow` + "Mithril
Partial Sync:" header behind it (Critiquer blocker #1). Keeping the gate at the Section is also the
smaller change: ONE null-return added, no new prop on the recommendation, its Props (`:42-47`) and
`render()` (`:54-93`) unchanged.

The recommendation's button stays `disabled={isActionBlocked}` (unchanged, `:81`) — that is the
serialized-mutation-lock disable while OTHER Mithril work runs, which is correct and DISTINCT from the
kill-switch/behind gating (lock #14). D1 hide-not-disable is satisfied because the whole row (button
included) is hidden — never rendered as a greyed-out control — when not `isEnabled &&
isSignificantlyBehind`.

## Locked invariants this task honors (inline)
- **#4 / no renderer threshold:** the renderer consumes `isEnabled`, `isSignificantlyBehind`, and
  `behindByImmutables` as opaque values; it NEVER compares `behindByImmutables` to a number.
- **#10 / kill switch hides ALL partial-sync UI:** when `isPartialSyncEnabled` is false the entire
  `<MithrilPartialSyncSection>` is not rendered (Step 11); default-false until first response =
  fail-closed.
- **D1 hide-not-disable:** the recommendation row (header + CTA) is conditionally RENDERED — the
  `MithrilPartialSyncSection` returns `null` for its non-confirmation path when not (`isEnabled &&
  significantlyBehind`), never a greyed-out button and never an empty labeled header row near the tip
  (Step 12b). The gate lives at the Section, not inside `MithrilPartialSyncRecommendation`, precisely
  so no residual header survives (Critiquer blocker #1).
- **#14 / serialized mutation lock:** `isActionBlocked` keeps its `|| isMithrilBootstrapActive` half
  and still disables the CTA while ANY Mithril work is active; the only change is the partial-sync
  term goes `isActive` → `isWorking` (terminal states re-arm).
- **D9/BUG1 / CTA re-arm:** terminal `completed`/`cancelled`/`failed` are not `isWorking`, so
  `isActionBlocked` clears and the CTA re-arms without an app restart. This task does NOT touch the
  backend reset-to-idle or the dismiss→finalize seam (task-ux-404).
- **#15 / `_isTornDown` guard:** the new `_applyAvailability` and `_refreshAvailability` are both
  `_isTornDown`-guarded (inert after teardown); the `onReceive` status subscription remains
  guard-inert with NO unsubscribe added; the new `setInterval` IS cleared in `teardown()`.
- This task adds **no new IPC channel** and **no threshold math** (convergence rule).

## Acceptance criteria (verbatim from tasks JSON)
- All partial-sync UI hidden when kill switch off.
- Recommendation/CTA appears only when `isEnabled && isSignificantlyBehind`.
- `isActionBlocked` derives from `isWorking` so the CTA re-arms on terminal states without restart.
- Renderer computes no threshold.
- The `_isTornDown` teardown guard is documented.

## Verification plan (exact commands, run from repo root `/workspaces/mithril-partial-sync-ux`)
- `yarn compile` — TypeScript must pass (validates the new store fields, the renamed/added props
  through container → `DaedalusDiagnostics` → Section → Recommendation, and the availability
  channel `.request()` return type).
- `node_modules/.bin/eslint --ext .ts,.tsx source/renderer/app/stores/MithrilPartialSyncStore.ts
  source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx
  source/renderer/app/components/status/DaedalusDiagnostics.tsx
  source/renderer/app/components/status/MithrilPartialSyncSection.tsx
  source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx` — no new lint errors on
  the five touched source files.
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts
  source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx
  source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx
  source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts` — existing + new tests pass.

### Tests to add/update, mapped to the 4 JSON testCases
Pre-req mock updates (do these first, they make the existing suites compile):
- `MithrilPartialSyncStore.spec.ts` `jest.mock('../ipc/mithrilPartialSyncChannel', …)` (`:15-34`):
  add `mithrilPartialSyncAvailabilityChannel: { request: (...args) => mockAvailabilityRequest(...args) }`
  with a new `const mockAvailabilityRequest = jest.fn();` (default
  `mockAvailabilityRequest.mockResolvedValue({ isEnabled: true, isSignificantlyBehind: false })` in
  the relevant tests; reset in `beforeEach`). The suite already calls `jest.useFakeTimers()` (`:6`)
  and `jest.clearAllTimers()` (`:51`), so the new `setInterval` is covered.
- `DaedalusDiagnostics.spec.tsx` `defaultProps` (`:52-54`): replace `isMithrilPartialSyncActive: false`
  with `isMithrilPartialSyncWorking: false` and add `isMithrilPartialSyncEnabled: true`,
  `isMithrilPartialSyncSignificantlyBehind: true` (so the EXISTING render tests, which expect the
  recommendation visible, keep passing). Update the active-CTA test (`:105-114`) to override
  `isMithrilPartialSyncWorking: true`.
- `DaedalusDiagnosticsDialog.spec.ts` mock store (`:38-42`, `:93-98`): add `isWorking`,
  `isPartialSyncEnabled: true`, `isSignificantlyBehind: true` to the `mithrilPartialSync` mock; in
  `withStatus` set `isWorking: status !== 'idle' && status !== 'completed' && …` — simplest correct
  form: `isWorking: isMithrilPartialSyncWorkingStatus(status)` importing the live helper. (Keep the
  existing `isActive` mock field too; the container no longer reads it but leaving it is harmless.)
- `MithrilPartialSyncSection.spec.tsx` `defaultProps` (`:16-23`): RENAME `isMithrilPartialSyncActive:
  false` (`:19`) to `isMithrilPartialSyncWorking: false` and ADD `shouldShowRecommendation: true` so
  the existing render tests (which click the "Mithril Partial Sync" button — `:39`, `:52`, `:66`,
  `:88`, `:125`, `:151`) still render the recommendation row. Also rename the bare
  `isMithrilPartialSyncActive` prop at `:162` (in the "closes confirmation when partial sync becomes
  active externally" test) to `isMithrilPartialSyncWorking`. Without these, the suite fails to
  compile/render after the Step 12 prop rename.

- **testCase (a) kill switch off hides all partial-sync UI (no enabled button)** →
  `DaedalusDiagnostics.spec.tsx`: render with `isMithrilPartialSyncEnabled: false`; assert
  `screen.queryByRole('button', { name: 'Mithril Partial Sync' })` is `null` AND the recommendation
  copy text is absent. Confirms the whole Section is hidden.
- **testCase (b) `isEnabled && isSignificantlyBehind` shows the recommendation/CTA, otherwise
  hidden** → `DaedalusDiagnostics.spec.tsx`: (i) `isEnabled:true, significantlyBehind:true` → button
  present + enabled. (ii) `isEnabled:true, significantlyBehind:false` → recommendation copy + button
  absent (`screen.queryByRole('button', { name: 'Mithril Partial Sync' })` null) AND the section
  HEADER is absent — assert `screen.queryByText(/Mithril Partial Sync/)` is `null` (proves the empty
  labeled row is gone, not just the button — Critiquer blocker #1). NOTE on the matcher: use the
  REGEX form `/Mithril Partial Sync/`, NOT the exact string `'Mithril Partial Sync'` — the header div
  renders "Mithril Partial Sync" plus a separate colon text node (`globalMessages.punctuationColon`,
  `':'`), so its normalized text content is `"Mithril Partial Sync:"` and an exact-string match would
  not match the header even when present. The regex (substring) matcher reliably returns `null` only
  when NO partial-sync text (header, copy, or button) renders — which is exactly the gated-off state.
  Add a focused Section-level assertion via `MithrilPartialSyncSection.spec.tsx`: render with
  `shouldShowRecommendation={false}` and assert the WHOLE Section renders nothing — `const { container
  } = renderComponent({ shouldShowRecommendation: false }); expect(container.firstChild).toBeNull();`
  (plus `screen.queryByRole('button', { name: 'Mithril Partial Sync' })` is `null`), confirming the
  Section returns `null` and leaves no header row. `renderComponent` wraps in `<IntlProvider>`, whose
  own element is the container child, so assert against the inner `MithrilPartialSyncSection` output:
  prefer rendering `MithrilPartialSyncSection` directly (or assert `container.textContent` does not
  match `/Mithril Partial Sync/`) to avoid the `IntlProvider` wrapper confounding `firstChild`.
- **testCase (c) terminal completed/cancelled/failed re-arms the CTA (`isActionBlocked` clears)** →
  TWO layers: (1) `MithrilPartialSyncStore.spec.ts` — after `_updateStatus({ status: 'completed', … })`
  assert `store.isWorking === false` (and `'cancelled'`, `'failed'` likewise); (2)
  `DaedalusDiagnostics.spec.tsx` — render with `isMithrilPartialSyncWorking: false` (terminal) and
  assert the button is `toBeEnabled()`. This proves the wired path: terminal → not working →
  `isActionBlocked` false → CTA enabled.
- **testCase (d) CTA stays disabled while other Mithril work is active** →
  `DaedalusDiagnostics.spec.tsx`: (i) `isMithrilPartialSyncWorking: true` → button `toBeDisabled()` +
  "Unavailable while Mithril work is already active." hint (adapt the existing `:105-114` test); (ii)
  keep the existing `isMithrilBootstrapActive: true` test (`:116-125`) — still disabled. Confirms the
  serialized-lock disable survives.
- Store availability consumption test (covers Scope item 1): `MithrilPartialSyncStore.spec.ts` —
  `mockAvailabilityRequest.mockResolvedValue({ isEnabled: true, isSignificantlyBehind: true,
  behindByImmutables: 42 })`; `store.setup()`; await; assert `store.isPartialSyncEnabled === true`,
  `store.isSignificantlyBehind === true`, `store.behindByImmutables === 42`, and
  `mockAvailabilityRequest` called once at setup. Add a teardown test: after `store.teardown()` the
  interval is cleared (assert no further `mockAvailabilityRequest` calls after advancing timers with
  `jest.advanceTimersByTime(AVAILABILITY_REFRESH_INTERVAL)` post-teardown).

## Risks / open questions
- **Prop rename surface:** `isMithrilPartialSyncActive` → `isMithrilPartialSyncWorking` touches the
  container, `DaedalusDiagnostics` (type + destructure + Section render), `MithrilPartialSyncSection`
  (Props + `componentDidUpdate` + render), and THREE spec files —
  `DaedalusDiagnostics.spec.tsx`, `MithrilPartialSyncSection.spec.tsx`, and
  `DaedalusDiagnosticsDialog.spec.ts`. `yarn compile`/`yarn test:jest` catch any missed site.
  Mitigation: the Verification pre-req mock-update list now enumerates EVERY spec that references the
  old prop name (including `MithrilPartialSyncSection.spec.tsx` — Critiquer blocker #2), so the suites
  compile after the rename.
- **Default-false flicker:** `isPartialSyncEnabled` defaults false, so on first paint (before the
  setup `request()` resolves) the Section is hidden, then appears. This is the correct fail-closed
  behavior (lock #10) and matches the diagnostics dialog's async-data norm; no spinner needed.
- **Interval + fake timers:** the store spec uses `jest.useFakeTimers()`. The new `setInterval` will
  not fire unless timers are advanced; ensure new tests that assert refresh-while-working advance
  timers AND set `isWorking` (push a working status first). The teardown test must assert clearance.
- **`isActive` left intact (non-blocking):** the store getter `isActive` and
  `isMithrilPartialSyncActiveStatus` remain (other readers / the store spec at `:141`). Only the CTA
  block path migrates to `isWorking`. Removing `isActive` is out of scope.
- **retry = start reuse (#24):** documented only — `startPartialSync` is the retry path; re-arming the
  CTA via `isWorking` correctly re-enables it for a fresh start after a terminal `failed`/`cancelled`.
  No change to the retry wiring here.

## Required doc / research updates
- None to PRD/research are required for acceptance (D1/D3/D9 already describe this gating + re-arm).
- The `_isTornDown` teardown-guard documentation requirement is satisfied IN THIS PLAN (Step 6 +
  Locked invariants) and must be re-stated in the impl-review entry (research-19 row #15 disposition:
  keep the guard, add no IPC unsubscribe; the new interval IS cleared).
- Update `api-endpoints.md`? NO — task-ux-101 already documented the availability channel; this task
  only consumes it.

## Review-log paths
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-3/task-ux-301-plan-review.md`
  (append-only; Planner entry started).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-3/task-ux-301-impl-review.md`
  (append-only; created at implementation time).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-3/task-ux-301-research.md`
  (research note).

## Final outcome (completed 2026-06-24)
Implemented exactly as planned (post-critique revision: gate moved to the Section level).

Files shipped (5 source):
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` — type import `MithrilPartialSyncAvailability`
  + value import `mithrilPartialSyncAvailabilityChannel`; `AVAILABILITY_REFRESH_INTERVAL = 30_000`; three
  `@observable` fields (`isPartialSyncEnabled=false`, `isSignificantlyBehind=false`,
  `behindByImmutables=undefined`) + `_availabilityRefreshInterval`; `@action _refreshAvailability`
  (async, `_isTornDown`-guarded, swallows rejection via `logger.warn`) and `@action _applyAvailability`
  (`_isTornDown`-guarded, stores `behindByImmutables` verbatim — no threshold compare); `setup()` fires the
  one-shot query + starts a `setInterval` refreshing ONLY while `this.isWorking`; `teardown()` clears+nulls
  the interval before the guard/super.
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` — feeds
  `isMithrilPartialSyncWorking={mithrilPartialSync.isWorking}` (NOT `isActive`) + threads
  `isMithrilPartialSyncEnabled` / `isMithrilPartialSyncSignificantlyBehind`.
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` — Props + destructure renamed/extended;
  `isMithrilActionBlocked = isMithrilPartialSyncWorking || isMithrilBootstrapActive` (serialized-lock half
  preserved); `<MithrilPartialSyncSection>` rendered ONLY when `isMithrilPartialSyncEnabled`, passed
  `shouldShowRecommendation = isEnabled && isSignificantlyBehind`.
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` — prop rename
  (`isMithrilPartialSyncActive` → `isMithrilPartialSyncWorking`) + new `shouldShowRecommendation`;
  `componentDidUpdate` uses the renamed prop; `render()` returns `null` AFTER the `isShowingConfirmation`
  early-return when `!shouldShowRecommendation`, killing the empty labeled "Mithril Partial Sync:" header
  row near the tip (Critiquer blocker #1).
- READ-ONLY (unchanged): `source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx` and
  `source/common/types/mithril-partial-sync.types.ts` (the type + `isMithrilPartialSyncWorkingStatus`
  already existed).

Key behavior: kill switch off (`isPartialSyncEnabled=false`, fail-closed default) hides ALL partial-sync UI;
recommendation/CTA renders only when `isEnabled && isSignificantlyBehind`; `isActionBlocked` derives from
`isWorking` so terminal `completed`/`cancelled`/`failed` re-arm the CTA without an app restart; the
serialized-lock `|| isMithrilBootstrapActive` half is preserved; renderer computes NO threshold
(`behindByImmutables` stored verbatim); no new IPC channel; the `_isTornDown` teardown guard keeps the
status handler + late-resolving availability `request()` inert after teardown (no IPC unsubscribe added;
the new `setInterval` IS cleared in `teardown()`).

Verification: `tsc --noEmit` (the `yarn compile` TS gate) PASS, exit 0; eslint on all touched files 0 errors
(53 pre-existing/stylistic warnings); jest — `MithrilPartialSyncStore.spec.ts` 15/15, plus
`DaedalusDiagnostics.spec.tsx` / `MithrilPartialSyncSection.spec.tsx` / `DaedalusDiagnosticsDialog.spec.ts`
green (33/33 across all four suites in code review). NOTE: `yarn compile`'s `precompile` `typedef:sass` hook
crashes under Node v24 in this sandbox (pre-existing dart-sass environment defect, unrelated to this change);
the real TS gate (`tsc --noEmit`) was run directly and passes.

Plan critique: requires_changes (blocker #1 residual empty header row; blocker #2 spec mock enumeration),
both fixed in the single allowed Planner pass (gate moved to the Section, returning `null`; Recommendation
downgraded to READ-ONLY) → re-submitted and built. Code review: approved, independently re-verified, no
blockers.

Deviation: `storybook/stories/nodes/status/Diagnostics.stories.tsx` (outside JSON targetPaths) was updated to
match the prop rename (`isMithrilPartialSyncWorking: false` + the two availability args), a mechanical
follow-through of the rename, not a product/UX/copy decision and not 302/303/404 scope.

## Status
- Planning status: `approved`
- Build status: `completed`
