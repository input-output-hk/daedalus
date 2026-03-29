Planner: Iteration 1
Timestamp: 2026-03-29T14:50:03Z
Outcome: initial_plan_documented

- Drafted the initial canonical `task-701` plan around the real packaged sync surface in `agentic/src/agentic_kb/commands/sync.py`, explicitly correcting for tracker drift from the stale `agentic/src/sync/commands.py` target path.
- Scoped the task to the full sync command family and orchestration on top of the ingestion and sync-state foundations already delivered by tasks `301`, `402`, `403`, `404`, `405`, `503`, `602`, and `604`.
- Kept the boundary with neighboring tasks explicit: no stale-index detection (`task-702`), no scheduled automation (`task-703`), no MCP work (`task-801`), and no deeper remote-ingestion redesign unless critique identifies a concrete blocker.
- Defined the post-task-701 meaning of `sync changed` as the general incremental sync command for an already-seeded KB, while preserving the task-604 snapshot-import bootstrap flow as a supported starting case rather than a separate command contract.
- Called out the main critique targets that need review before implementation: per-source initial-sync behavior when baselines are missing, the exact `sync all` failure policy, and the need to keep GitHub `pulls`/`review_comments` plus Project cursor-only limitations explicit in both behavior and operator messaging.

Critiquer: Iteration 1
Timestamp: 2026-03-29T14:52:41Z
Outcome: requires_changes

- The plan is directionally aligned with repo reality, but it still leaves the most important operator contract under-specified: line 95 allows per-source "failure or fallback rules" for missing baselines without deciding them. That is too open for later implementation review because `sync changed` semantics differ sharply by source today. The plan needs to lock explicit source-by-source empty-state behavior for `docs`, `code`, `github`, and `project`, including whether `sync changed` is allowed to widen into initial full sync for any source or must fail when baseline rows are absent. Without that, implementation can drift and still claim conformance.
- The `sync all` contract is also not pinned tightly enough. The plan says `sync all` should orchestrate source-specific commands in order, but it does not decide whether `sync all` includes `sync changed` as part of that orchestration or whether it is strictly `docs` + `code` + `github` + `project`. Given the task-604/task-701 distinction, that needs to be explicit. Repo reality and the platform workflow suggest `sync changed` is a separate incremental operator command, not a step inside `sync all`, and the plan should say so directly. It also needs to lock the failure policy called out in Risks: continue-and-report versus stop-on-first-error.
- The GitHub incremental section still overstates certainty relative to the current ingestion contract. The current ingestor supports all four streams in one `ingest_github(...)` flow and applies client-side filtering for unsupported streams after fetch; task-604 intentionally bypassed that for safety. For task-701, the plan should explicitly decide whether `sync github` will use the general `ingest_github` path with per-stream/page bounds and client-side filtering for `pulls` and `review_comments`, or preserve the task-604 selective-stream pattern. Without that decision, acceptance criteria like "reuses stored per-stream sync state" are not specific enough to test because multiple materially different implementations would satisfy the current wording.
- The Project section correctly preserves the cursor-only limitation, but it does not state what `sync project` should do when a cursor already points at the end and an existing item changed server-side. Since current ingestion is continuation-only, the command contract should say whether task-701 merely advances from the stored cursor and may miss edits to old items, or whether it intentionally widens to a bounded replay strategy. Right now the plan says "whatever safe refresh behavior current ingestion can support," which is too vague for acceptance review.
- Tracking and docs scope need one correction. The plan makes `.agent/workflows/agentic-kb.md` part of task-701, but the tasks tracker currently assigns that full workflow update to `task-803`. Task-701 can and should update only the sync-command wording needed to stop contradicting shipped CLI behavior, but the plan should call that out as a narrow partial update so implementation does not accidentally absorb the broader workflow finalization already reserved for `task-803`.
- Verification is broad but still missing a few implementation-grade assertions tied to the contested contracts above. It should explicitly require tests for `sync all` excluding `sync changed`, `sync all` failure behavior, `sync github` behavior for unsupported server-side streams (`pulls`, `review_comments`) under task-701 semantics rather than task-604 defer semantics, and `sync project` repeated-run behavior when no new cursor pages exist. Those are the cases most likely to drift from the plan during implementation.
Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-29T14:53:57Z
Outcome: plan_revised_per_critique

- Locked `sync changed` missing-baseline behavior source by source: docs and code require successful `repo_commit_hash` baselines, GitHub requires all four stream rows with watermarks, and Project requires a non-empty stored cursor; the command now explicitly fails rather than widening into first-sync fallback.
- Locked `sync all` as explicit full-source orchestration of `sync docs` -> `sync code` -> `sync github` -> `sync project`, explicitly excluding `sync changed` and stopping on first source failure.
- Locked `sync github` to the existing general four-stream `ingest_github(...)` path for task-701, with a shared lower-bound watermark, upstream `since` only on supported streams, and client-side filtering for `pulls` and `review_comments`; tightened acceptance and verification language around that contract.
- Locked `sync project` to initial full ingest only for explicit `sync project`, then cursor-only continuation on later runs, including explicit no-op repeated-run behavior and explicit acknowledgement that already-seen item edits can still be missed.
- Narrowed `.agent/workflows/agentic-kb.md` scope to only the sync-command wording needed to match shipped task-701 behavior, preserving the broader workflow finalization boundary for `task-803`.
- Strengthened verification requirements around `sync all` exclusion/failure behavior, `sync changed` preflight failures, GitHub stream-specific bounds behavior, and Project repeated-run cursor semantics.

Critiquer: Iteration 2
Timestamp: 2026-03-29T14:56:19Z
Outcome: approved

- The iteration-2 plan resolves the prior missing-baseline ambiguity by locking `sync changed` to fail before any source work when docs, code, GitHub, or Project baselines are absent or incomplete, with source-specific baseline requirements and explicit guidance to use the source sync commands or `sync all` first.
- The `sync all` contract is now implementation-grade: it explicitly runs only `sync docs`, `sync code`, `sync github`, then `sync project`, excludes `sync changed`, and stops on the first failure while preserving reporting of earlier successful sources.
- The `sync github` contract is now aligned with current ingestion reality in `agentic/src/agentic_kb/commands/sync.py` and the task-403 research: one general four-stream ingest path, shared lower-bound watermark derivation, upstream `since` only where supported, and explicit client-side filtering limits for `pulls` and `review_comments`.
- The `sync project` contract is now specific enough to review and test: first explicit run can seed from `after_cursor=None`, repeated runs continue only from the stored cursor, no-op reruns at the end cursor are allowed, and the current inability to replay already-seen items remains explicit instead of being overclaimed.
- The workflow-doc scope is now properly narrowed to task-701-owned sync wording only, which keeps the broader `.agent/workflows/agentic-kb.md` finalization boundary with `task-803` intact.
- The verification section now covers the previously under-specified edge cases that matter for implementation review, including `sync all` exclusion/failure behavior, `sync changed` preflight failures, GitHub stream asymmetry, and Project repeated-run cursor semantics.
Decision: approved
