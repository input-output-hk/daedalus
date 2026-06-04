# Task-301 Diagnostics CTA Notes

- Purpose: record durable renderer and i18n findings from `task-301` so later diagnostics tasks keep the same scope boundary.
- Date: 2026-05-21

## Durable Findings

- `task-301` intentionally does not make the diagnostics CTA actionable. `DaedalusDiagnostics` now renders a visible `Mithril Partial Sync` button, but it stays disabled so confirmation ownership remains with `task-302`.
- The truthful gating inputs for the diagnostics surface are:
  - `stores.mithrilPartialSync.isActive`
  - `isMithrilBootstrapBlockingNodeStart(stores.mithrilBootstrap.status)`
- Recommendation copy is renderer-only context. It can mention current sync state such as `syncPercentage`, but it must not introduce thresholds, slot-lag heuristics, or backend-start logic.
- The diagnostics container did not need a new store seam. Existing store truth plus the shared bootstrap helper were sufficient for the conflicting-work boundary.

## i18n Gotcha

- `yarn i18n:manage` is necessary for new diagnostics messages, but it is not the end of the job for user-facing runtime copy.
- Newly added locale entries can remain seeded with `!!!` placeholder strings after extraction and checking.
- For user-visible diagnostics work, later tasks should verify the runtime locale files directly and polish the new `en-US.json` and `ja-JP.json` values before signoff.
- Focused renderer tests should assert final rendered strings strongly enough to catch `!!!` placeholder leakage, not only broad substrings.

## Verification Evidence

- Focused Jest coverage passed:
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
- i18n workflow passed:
  - `yarn i18n:manage`

## Follow-Ups

- `task-302` should reuse the new diagnostics CTA surface but become the first task that makes it actionable by owning the confirmation modal.
- If a later task adds container-specific behavior around the CTA, consider whether the current component-focused test should gain a small container assertion for the helper-based gating path.
