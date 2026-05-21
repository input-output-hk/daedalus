# Task 303 Overlay Reuse Notes

- Date: 2026-05-21
- Task: `task-303`

## Durable Findings

- Diagnostics must remain the UI owner through the store's optimistic `stopping-node` seed. Closing the diagnostics dialog from `isActive` alone is unsafe because the start IPC can still reject before any backend-confirmed progress or terminal state exists.
- The safe handoff rule is: close diagnostics only when partial sync enters an overlay-backed backend status. For task-303 that status set is `preparing`, `downloading`, `verifying`, `converting`, `installing`, `finalizing`, `starting-node`, `completed`, `failed`, or `cancelled`.
- Partial sync success does not naturally reset to backend `idle`. Startup emits `starting-node`, then terminal `completed`; the renderer must therefore provide an explicit completed-dismiss action instead of assuming a later success reset.
- Existing Mithril progress presentation is reusable for partial sync only at the stepper and timer level. The bootstrap combined download footer is byte-based, while current partial-sync telemetry is file-count based, so that footer must stay disabled for the partial-sync path until truthful byte-total telemetry exists.
- Existing Mithril error presentation is reusable when the view accepts injected title, hint, and button definitions. Recovery action rendering must stay backend-owned through `allowedRecoveryActions`; renderer status names are not a safe proxy.
- Reusing the existing Mithril bootstrap SCSS shell was sufficient for task-303. No separate overlay styling seam was required.

## Follow-Through For Later Tasks

- `task-304` should keep the polished partial-sync runtime copy aligned with the backend-owned recovery-action surface and should not reintroduce `!!!` placeholders in shipped locales.
- `task-400` can add broader renderer integration coverage, but it should preserve the helper-level ownership assertion added here unless the repo's injected container test seams become less brittle.
