Planner: Iteration 1
Timestamp: 2026-05-21T14:43:54Z
- Read in order as requested: `.agent/readme.md`, `.agent/system/architecture.md`, `.agent/workflows/frontend.md`, `.agent/workflows/test.md`, `.agent/workflows/update-doc.md`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`, `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`, then historical Mithril UX context and the live in-scope files.
- Loaded `understand` first for repo-understanding expectations, then verified all material planning claims against live files before writing the plan. Loaded `i18n-messaging` because this task adds user-facing diagnostics copy.
- Created the canonical task plan at `.agent/plans/mithril-partial-sync/task-plans/task-301.md`.
- Approved implementation direction is intentionally narrow:
  - add diagnostics recommendation copy near existing sync diagnostics
  - add a manual `Mithril Partial Sync` CTA
  - keep the CTA unavailable while Mithril-managed work is active
  - use only existing sync context from `NetworkStatusStore`
  - do not add threshold logic or auto-trigger behavior
  - do not pull confirmation modal work from `task-302`
- Important live-file finding that affected the plan: `MithrilPartialSyncStore.startPartialSync()` already starts backend work immediately, so `task-301` must not wire the new CTA directly to that store action or it would violate the locked `task-302` confirmation boundary. The plan therefore treats this task’s button as a rendering and handoff seam only, with backend start still deferred.
- Planned gating uses existing store truth only:
  - disable when `stores.mithrilPartialSync.isActive`
  - also disable during bootstrap-side active Mithril work, derived from current bootstrap status rather than inventing new store logic
- Expected code changes are limited to:
  - `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
  - `source/renderer/app/components/status/DaedalusDiagnostics.scss`
  - one focused renderer spec for diagnostics CTA rendering/gating
  - i18n catalog artifacts only if implementation chooses to run the message-management workflow in this task
- Acceptance and verification in the plan stay scoped to rendering and gating:
  - recommendation copy renders from existing sync data
  - CTA renders
  - CTA disables for active partial sync
  - CTA disables for active bootstrap-managed work
  - no confirmation behavior and no backend start in this task
- Self-review completed before finalizing the plan:
  - scope creep: blocked
  - stale workflow text: none carried forward
  - missing tests/docs: covered in plan
  - plan consistency: aligned with PRD, task graph, task-300 notes, and live files

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-05-21T14:47:58Z
- Broad pass completed against the requested docs, historical Mithril UX context, `task-300` notes, the task graph, and the live renderer/status files.
- The plan is correctly trying to keep `task-301` narrow, renderer-only, and free of backend-threshold logic. The main problems are around convergence: one task boundary is not implementable truthfully as written, and one workflow decision is still left ambiguous even though this task adds real user-facing copy.
Blocking findings:
- `task-301` currently asks for an available manual CTA while also forbidding both behaviors that would make that CTA truthful. The plan says the button should render and be enabled when no Mithril work is active (`.agent/plans/mithril-partial-sync/task-plans/task-301.md:109-117,149-156`), but it also says this task must not start partial sync and must not add the confirmation flow. Live code does not have an intermediate owner for a “safe handoff only” click: `DaedalusDiagnosticsDialog.tsx:91-128` only passes concrete actions into the component, `DaedalusDiagnostics.tsx:408-413` types callbacks as immediate handlers, and the existing button pattern calls `.trigger()` directly (`DaedalusDiagnostics.tsx:905-910`). `task-300` notes also explicitly preserved modal ownership and deferred diagnostics CTA flow ownership to tasks 301-303 (`.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md:18-23`). As written, implementation converges to either a dead enabled button or smuggling task-302 behavior into task-301. The plan should be narrowed so task-301 is either copy plus disabled placeholder CTA only, or the task boundary should explicitly include the minimal confirmation-opening seam needed to make the button real.
- The i18n workflow is still left as an implementation-time choice even though this task adds committed diagnostics copy. The plan leaves `translations/messages.json` and locale sync undecided (`task-301.md:100,157,165`), but the repo workflow and loaded `i18n-messaging` guidance are explicit: new `defineMessages()` content is expected to run through `yarn i18n:manage` (`package.json:52-54`). Because `check:all` also includes `yarn i18n:manage` (`package.json:17`), deferring catalog updates to `task-304` makes this task’s verification and branch hygiene ambiguous. The plan needs one explicit path before approval: either `task-301` runs `yarn i18n:manage` and carries the generated catalog/locale placeholder churn now, or it explicitly marks i18n artifact sync as a known deferred follow-up that blocks merge until later. Leaving it open is not convergence-oriented enough for an autonomous task.
Non-blocking observations:
- For bootstrap-side gating, the simpler and less stale approach is to use the existing shared helper `isMithrilBootstrapBlockingNodeStart` from `source/common/types/mithril-bootstrap.types.ts:34-47` instead of duplicating a renderer-local status list. The same applies to partial-sync helpers if needed from `source/common/types/mithril-partial-sync.types.ts:60-72`.
- The plan’s likely-test wording is still a bit loose. Since there are no existing diagnostics specs, the most truthful split is probably one focused component spec for rendering/copy plus one very small container-focused assertion only if gating is derived there. Otherwise the container test surface may become heavier than the feature.
- `task-301` in the task graph only lists `DaedalusDiagnostics.tsx` and `DaedalusDiagnosticsDialog.tsx` as target paths (`mithril-partial-sync-tasks.json:443-446`), while the plan expects SCSS and a new spec too. That mismatch is not a blocker, but it should be reconciled when the task is updated after implementation.
Approval bar:
- Clarify the CTA ownership boundary so `task-301` does not require an enabled no-op button and does not silently absorb task-302 behavior.
- Make the i18n artifact workflow explicit for this task instead of deferring the decision to implementation time.
- Keep the rest of the scope narrow: renderer-only copy/gating, no threshold logic, no backend start, no overlay reuse.
Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-21T14:52:00Z
- Re-read the requested docs, loaded `understand` first, verified the critique against live diagnostics/container/store/helper files, and updated the canonical plan at `.agent/plans/mithril-partial-sync/task-plans/task-301.md`.
- Narrowed the CTA ownership boundary so `task-301` converges truthfully without absorbing `task-302`:
  - the plan no longer expects an enabled button before a confirmation owner exists
  - the diagnostics CTA is now explicitly a visible but disabled placeholder in `task-301`
  - no fake click seam or backend-start wiring is introduced
  - `task-302` is now clearly the first task that makes the CTA actionable by owning confirmation
- Made the i18n workflow explicit instead of deferring it:
  - `task-301` now requires `yarn i18n:manage`
  - the expected artifact set now explicitly includes `translations/messages.json` plus locale placeholder updates in `source/renderer/app/i18n/locales/en-US.json` and `source/renderer/app/i18n/locales/ja-JP.json` when generated
  - the fallback is also explicit: if `yarn i18n:manage` is blocked by unrelated repo issues, that must be recorded rather than left ambiguous
- Simplified gating guidance by preferring existing helpers over new machinery:
  - bootstrap-side blocking now points to shared helper `isMithrilBootstrapBlockingNodeStart(...)`
  - partial-sync-side blocking stays on the existing store truth (`mithrilPartialSync.isActive`)
  - no renderer-local status lists or threshold heuristics are added
- Tightened acceptance and verification so they match the reduced truthful scope:
  - recommendation copy renders from existing sync context only
  - CTA exists but is disabled in the baseline task-301 state
  - CTA remains disabled during active Mithril partial sync and bootstrap work
  - no confirmation behavior and no backend start are part of this task
- Why this now converges truthfully: the plan no longer depends on an implementable-but-false enabled CTA, no longer leaves message artifact handling undecided, and keeps the task to the smallest real renderer slice that can land independently without stealing confirmation behavior from `task-302`.

Outcome: Plan revised and finalized for implementation

