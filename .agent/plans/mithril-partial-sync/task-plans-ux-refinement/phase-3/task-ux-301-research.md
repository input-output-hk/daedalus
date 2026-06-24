# task-ux-301 — Research / durable findings

Scope: renderer wiring of the already-built availability contract (task-ux-101/102), two-level UI
gating, and the `isWorking`-based CTA re-arm (PRD D1/D3 renderer + D9/BUG1 renderer half). No new IPC,
no threshold math.

## Decisions

- **Where gating lives (DD1).** The store exposes availability observables; the container
  (`DaedalusDiagnosticsDialog`) reads them and passes plain booleans down as props. `DaedalusDiagnostics`
  stays a props-only `@observer` (its spec renders it with a flat props object, no stores), keeping it
  unit-testable. Reactivity is automatic because the container is `@observer`.

- **Two-level gate (DD2).** `isEnabled` (prop `isMithrilPartialSyncEnabled`) gates the ENTIRE
  `<MithrilPartialSyncSection>` element (lock #10 — kill switch hides all partial-sync UI; default
  `isPartialSyncEnabled=false` is fail-closed). `isEnabled && isSignificantlyBehind` is threaded as
  `shouldShowRecommendation` and gates the recommendation/CTA specifically (PRD D1/D3).

- **Gate at the SECTION, not the Recommendation (critique fix).** `MithrilPartialSyncSection.render()`
  emits the `styles.layoutRow` wrapper + the `styles.layoutHeader` "Mithril Partial Sync:" label BEFORE
  delegating to `MithrilPartialSyncRecommendation`. A `null` return inside the recommendation would have
  left a bare labeled header row with empty content near the tip — a dead control contradicting D1
  ("never a dead control near the tip", prd:87-88). Fix: the Section itself returns `null` for its only
  non-confirmation content (the recommendation row) when `!shouldShowRecommendation`, AFTER the
  `isShowingConfirmation` early-return (a confirmation in progress must finish rendering and is only
  reachable from the already-gated button). Net effect: `MithrilPartialSyncRecommendation.tsx` is
  UNCHANGED — the smaller change and no residual header.

- **`isActionBlocked` derivation (DD3).** Changed the partial-sync term from `isActive`
  (`status !== 'idle'`) to `isWorking` (in-flight phases only), fed from the container. The
  `|| isMithrilBootstrapActive` serialized-mutation-lock half is PRESERVED unchanged (lock #14). Terminal
  `completed`/`cancelled`/`failed` are NOT in `MITHRIL_PARTIAL_SYNC_WORKING_STATUSES`, so `isWorking` is
  false → `isActionBlocked` clears → CTA re-arms without an app restart (D9/BUG1).

## Gotchas / things worth recording

- **Anchor drift (`isActive` helper).** Tasks JSON + research-19 gap #40 cite
  `isMithrilPartialSyncActiveStatus` (`status !== 'idle'`) at `mithril-partial-sync.types.ts:92-94`.
  **Live location is `:98-100`.** The `isWorking` helper `isMithrilPartialSyncWorkingStatus` is at
  `:86-88` (matches the brief). The live file is authoritative; all plan anchors were verified against
  live code 2026-06-24.

- **`isWorking` seam REUSE.** `isMithrilPartialSyncWorkingStatus()` over
  `MITHRIL_PARTIAL_SYNC_WORKING_STATUSES` already existed (types `:86-88`); the store's `isWorking` getter
  already existed. This task only changed which getter feeds the CTA block (`isActive` → `isWorking`) and
  the prop name end-to-end (`isMithrilPartialSyncActive` → `isMithrilPartialSyncWorking`). No new helper,
  no type-file edit.

- **Prop rename surface.** The `isMithrilPartialSyncActive` → `isMithrilPartialSyncWorking` rename touches
  the container, `DaedalusDiagnostics` (Props + destructure + Section render), `MithrilPartialSyncSection`
  (Props + `componentDidUpdate` + render), THREE specs, AND `storybook/.../Diagnostics.stories.tsx` (cast
  `as any`, so `tsc` did NOT catch it — caught only by reasoning about the story regression). Updated the
  story args as a mechanical follow-through.

- **Header text matcher gotcha.** The header div renders "Mithril Partial Sync" plus a SEPARATE colon text
  node (`globalMessages.punctuationColon`), so its normalized text content is "Mithril Partial Sync:". An
  exact-string `queryByText('Mithril Partial Sync')` would never match the header even when present — use
  the REGEX form `/Mithril Partial Sync/` to prove the header is absent in the gated-off state.

- **Periodic-refresh approach chosen (DD4).** The partial-sync store had NO existing polling (status is
  push-driven via `onReceive`). Mirrored `NetworkStatusStore`'s `setInterval`/`clearInterval` pattern: a
  one-shot `request()` in `setup()` (covers the common idle case) plus a single 30s interval
  (`AVAILABILITY_REFRESH_INTERVAL = 30_000`) that refreshes ONLY while `this.isWorking` (behind-ness can
  drift during a sync; near-idle it is static). Not a tight poll (PRD D2/D3). Interval cleared+nulled in
  `teardown()`.

- **`_isTornDown` teardown guard (#15).** Both new actions (`_refreshAvailability`, `_applyAvailability`)
  are `_isTornDown`-guarded, so a late-resolving `request()` is inert after teardown — same disposition as
  the existing status handler. The `onReceive` status subscription remains guard-inert with NO
  `removeListener`/IPC-abstraction unsubscribe added (consistent with the rest of the store). The ONE thing
  cleaned up in `teardown()` is the new plain `setInterval` we own.

- **Fake timers in the store spec.** `MithrilPartialSyncStore.spec.ts` uses `jest.useFakeTimers()`; the new
  `setInterval` will not fire unless timers are advanced. Refresh-while-working tests must advance timers
  AND push a working status first; the teardown test asserts no further `request()` calls after advancing
  timers post-teardown.

## Residual gaps / handoffs (intentionally out of this task)

- `isActive` getter + `isMithrilPartialSyncActiveStatus` helper left INTACT (other readers + store spec
  `:141`); only the CTA-block path migrated to `isWorking`. Removing `isActive` is out of scope.
- Overlay dismiss → backend `finalize` (reset-to-idle + staging cleanup) is task-ux-404 (the existing
  `mithrilPartialSyncFinalizeChannel` at `mithrilPartialSyncChannel.ts:58-61` is 404 territory; untouched
  here). This task only re-arms the CTA via `isWorking`.
- Proactive loading-screen prompt is task-ux-302; confirmation-modal redesign (snapshot metadata, button
  hierarchy, deep-link) is task-ux-303. Neither touched.
- retry = start reuse (#24): documented only — `startPartialSync` is the retry path; re-arming via
  `isWorking` correctly re-enables a fresh start after terminal `failed`/`cancelled`. No retry-wiring change.

## Environment defect (not a code issue)
`yarn compile`'s `precompile` `typedef:sass` hook (`typed-scss-modules`) crashes loading the bundled
dart-sass 1.44.0 under Node v24 in this sandbox; three `.scss`-importing specs can fail at the transform
layer under the project jest runner for the same reason. The real TS gate (`tsc --noEmit`, which is exactly
`"compile"`) passes, and the test LOGIC is proven green (33/33 across the four suites in code review). This
is a pre-existing environment defect unrelated to task-ux-301.
