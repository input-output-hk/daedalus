# Task 404 Planning Review Log

Planner: Iteration 1
Timestamp: 2026-03-28T14:56:52Z
Outcome: proposed_initial_plan

- The canonical plan scopes `task-404` to a new packaged ingestor at `agentic/src/agentic_kb/ingest/project.py` and explicitly reconciles the stale tasks-json target path away from `agentic/src/ingest/project.py`.
- The proposed implementation uses direct GitHub GraphQL calls from Python, not `gh`, to page through `DripDropz` Project 5 items, normalize the canonical project fields, and upsert searchable rows into `agentic.kb_project_items`.
- The plan keeps cursor persistence, `sync project` CLI orchestration, search behavior, and project mutations out of scope while still requiring reusable bounds/result contracts and focused unit coverage for issue, PR, and draft-issue project items.

Critiquer: Iteration 1
Timestamp: 2026-03-28T14:56:52Z
Outcome: requires_changes

- The biggest blocker is a schema-contract mismatch: `agentic/schema/init.sql` defines `agentic.kb_project_items` without a `preview_text` column, but the plan currently treats persisted `preview_text` as required in scope, acceptance criteria, and verification. Revise the plan to either (a) drop `preview_text` from the required persisted contract for `task-404`, reusing only `body_text` plus derived preview helpers in-memory, or (b) explicitly widen scope to include the needed schema migration and tracker/doc updates. Leaving this implicit will produce an implementation-vs-schema failure.
- The content-linkage contract needs one more guardrail for org-project reality. Because Project 5 is an organization project, linked items may come from repositories other than `DripDropz/daedalus`; in those cases, deriving `content_id` from `task-403` ids would create references to KB entities this platform never ingests. Update the plan to say `content_id` is only populated for `DripDropz/daedalus` issue/PR items that match the `task-403` id scheme, while non-Daedalus linked content keeps `content_id` null and preserves repo/url/node details in `metadata`/`content_node_id`. Add a verification case for that branch.
- The plan doc is also ahead of the process state: planning is still `draft`, but build status is already `in_progress`. Per `.agent/plans/agentic/prompt.md`, the build loop starts only after planning converges. Update the canonical plan so its status fields do not imply implementation has started before critique approval.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-28T14:56:52Z
Outcome: revised_plan_addresses_schema_linkage_and_status_feedback

- Revised the canonical plan to match `agentic/schema/init.sql` by removing persisted `preview_text` from scope, implementation, acceptance, and verification. The plan now treats any preview helper as an in-memory derivative of `body_text`, not a stored column.
- Tightened the linkage contract so `content_id` is populated only for linked `DripDropz/daedalus` issues and pull requests that align with the `task-403` deterministic ids. Linked content from any other repository and draft issues now keep `content_id` null while preserving repo/url/node details in `repo`, `html_url`, `content_node_id`, and `metadata`.
- Corrected the task state to reflect the actual process stage by setting planning to `in_review` and build to `not_started` until critique approval starts the build loop.

Critiquer: Iteration 2
Timestamp: 2026-03-28T14:56:52Z
Outcome: requires_changes

- The revised plan resolves the earlier schema and linkage blockers: it now fits `agentic.kb_project_items` without a schema change, keeps `content_id` scoped to ingestible Daedalus issue/PR entities, and otherwise stays appropriately library-first for `task-404`.
- One process/tracking mismatch still needs cleanup before approval: `.agent/plans/agentic/prompt.md` defines canonical build-status values as `in_progress`, `in_review`, or `completed`, but `task-404.md` now uses `Build Status: not_started`. Update the canonical plan to use an allowed status value or explicitly extend the task-doc status vocabulary elsewhere; as written, the task doc still does not match the required planning contract.
- Once that status-field contract is fixed, I do not see any remaining scope-creep, schema-shape, verification, or docs/tracking blockers in this iteration of the plan.

Decision: requires_changes

Planner: Iteration 3
Timestamp: 2026-03-28T14:56:52Z
Outcome: revised_plan_fixes_build_status_vocabulary_mismatch

- Updated the canonical plan's build-status field from `not_started` to `in_review` so it now matches the allowed vocabulary in `.agent/plans/agentic/prompt.md`.
- Kept the plan honest about implementation not having started by preserving the surrounding planning-stage context and clarifying in the task doc body that no build work has begun yet.
- No other approved plan revisions were changed in this iteration; the update is limited to the status-vocabulary mismatch called out in critique.

Critiquer: Iteration 3
Timestamp: 2026-03-28T14:56:52Z
Outcome: approved

- I re-read the full planning review log and re-stressed the revised canonical plan against the orchestrator contract, the current `agentic.kb_project_items` schema, and the remaining scope boundaries. The prior blockers are now closed: persisted `preview_text` is out of scope, `content_id` is limited to ingestible `DripDropz/daedalus` issue/PR entities, and the canonical status fields no longer use an unsupported build-status value.
- The remaining risks called out in `task-404.md` are implementation risks rather than planning blockers. The plan is explicit enough about loud failure on missing project scope/token access, loud failure on nested field-value pagination overflow, exact-name mapping for canonical Project 5 fields, and the non-goals around sync-state persistence, deletion detection, and CLI orchestration.
- Verification coverage is also sufficient for this stage: the plan requires pagination, typed-field normalization, issue/PR/draft handling, non-Daedalus linkage behavior, rerun-safe upserts, result-contract assertions, and explicit failure-path tests, plus a bounded live smoke pass only when an appropriately scoped token is available.

Decision: approved
