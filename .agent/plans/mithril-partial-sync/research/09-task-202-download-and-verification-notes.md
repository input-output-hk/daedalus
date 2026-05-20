# Task 202: Download And Verification Notes

## Summary

- `task-202` turned `MithrilPartialSyncService` from a pre-download skeleton into a real staged partial-restore executor that stops after verified staged output and before any live cutover work.

## Durable Findings

- The shipped partial-sync command shape is:
  - `mithril-client --origin-tag DAEDALUS --json cardano-db download latest --download-dir <stateDir>/mithril-partial-sync/download --start <local+1> --end <latestCertified> --include-ancillary --allow-override`
- The partial-sync service must re-resolve `latest` immediately before command launch and fail closed if the latest certified immutable number moved since preflight. Silent reuse of stale latest metadata is not acceptable.
- Mithril partial-sync progress should treat steps 1-3 as `downloading` and steps 4-7 as `verifying`, with truthful stage changes instead of synthesized verification percentages.
- Mixed telemetry must be retained across progress lines. Ancillary-only progress updates must not clear previously known file counts, because future renderer reuse of Mithril progress surfaces depends on stable cumulative status fields.
- Minimal staged-output validation for this task is limited to the staged `db/` root plus required top-level entries already proven by the spike: `clean/`, `immutable/`, `ledger/`, and `protocolMagicId`. Broader allowlist cutover validation remains task-203 work.
- The task-202 terminal success boundary is intentionally a failure-shaped handoff: `PARTIAL_SYNC_CUTOVER_NOT_READY` at stage `converting`. This keeps the flow truthful by avoiding any implication that download verification alone is restart-safe or fully complete.

## Verification Evidence

- Focused Jest coverage passed in `source/main/mithril/MithrilPartialSyncService.spec.ts` for:
  - staged partial-download command shape
  - latest-drift rejection
  - download and verification status mapping
  - mixed file plus ancillary telemetry retention
  - staged-output validation failure
  - pre- and post-verification command failures
  - post-verification cutover handoff boundary
- Repo-wide `yarn compile` was attempted but remains blocked by unrelated existing TypeScript failures outside task-202 scope.

## Follow-On Constraints

- `task-203` must consume the staged `db/` output and replace the current post-verification handoff boundary with conversion and validated staged cutover logic.
- `task-204` must preserve Boundary A recovery semantics for task-202 failures while adding later-boundary cleanup and restart-safety rules.
