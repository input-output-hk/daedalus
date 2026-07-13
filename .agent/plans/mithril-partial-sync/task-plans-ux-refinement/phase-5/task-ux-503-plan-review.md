# task-ux-503 — Plan Review

**Verdict: approved**

Reviewed against four criteria from the brief.

---

## (a) Avoids duplicating existing coverage

Pass. The plan opens with an explicit convergence table listing all pre-existing coverage with file-and-line anchors (twelve rows across seven spec files) and instructs the implementer in bold: "Trust the audit; do not re-author any of the above." The non-goals section reinforces this. No ambiguity about where existing coverage ends and the net-new work begins.

## (b) Correctly centers the #30 live-injection handoff gap and names the reusable harness

Pass. The plan precisely identifies the difference between the existing `App.spec.tsx:200-219` (prop-injection of a hardcoded fake store) and the production seam at `App.tsx:97-124` (live store getters). It names the existing harness at `MithrilPartialSyncStore.spec.ts:15-42` (`registeredStatusHandler` capture via `onReceive`), explains why the test must go through that handler rather than calling `_updateStatus` directly, and even calls out the potential `:158` overlap and how to handle it (extend rather than duplicate). The single deliverable — one focused `describe` in the store spec — is cleanly scoped.

## (c) Concrete enough for a smaller model to implement alone

Pass. The plan supplies:
- Exact file to edit (`MithrilPartialSyncStore.spec.ts`).
- Exact `describe` title to use.
- Four numbered test cases with full push payload objects (status, allowedRecoveryActions, transferProgress, error) and the precise assertions to make against each getter (`shouldShowOverlay`, `status`, `filesDownloaded`, `filesTotal`, `startedAt`, `canRetry`, `canRestartNormally`, `canWipeAndFullSync`, `error`, finalize channel call count).
- Harness conventions with line references to follow (`setupStore()` at `:50`, fake-timer pattern at `:569-593`, mock finalize as at `:158`).
- The conditional recovery-branch audit with explicit branch-to-line mappings so the implementer knows exactly whether to touch `MithrilPartialSyncOverlay.spec.tsx`.
- Verification commands in order.

## (d) Honors convergence and vocabulary guardrails

Pass. The "Vocabulary guardrails" section explicitly states: epochs-only behind-ness (`max(1, networkTip.epoch - localTip.epoch)`), no "immutable files" string to users, `isSignificantlyBehind` stays backend-owned, the `"Mithril partial sync"` diagnostics namespace string is acceptable, user-facing "Mithril Sync" rollout is deferred to `task-ux-601`, and this task adds zero new i18n keys or user-facing copy. The test cases listed assert only internal status enum values (`'downloading'`, `'failed'`, `'completed'`) and boolean getters — no copy surface.

---

## Minor observations (non-blocking)

- The note on `startedAt` exactness (use `jest.setSystemTime` before push, reference pattern at `:569-593`) is helpful; implementer should be aware `startedAt` is renderer-anchored on entering a working status, not directly from `elapsedSeconds`. The plan already notes this correctly.
- The "duplication risk vs `:158`/`:284-320`" risk item is well-handled: the plan's mitigation (fold getter-population assertions into the closest existing live-push test rather than adding a redundant block) is the right escape hatch.
- `.gitignore` exclusion from the commit is explicitly stated — good, since it is the only modified tracked file at branch start.

No changes required.

---

Critic: Plan is tight, well-anchored, and implementable as written. The single deliverable is correctly identified, the reuse harness is named precisely, existing coverage is not duplicated, and all vocabulary/scope guardrails are honored. Approved.
