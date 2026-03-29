Planner: Iteration 1
Timestamp: 2026-03-29T17:46:41Z
Outcome: initial_plan_documented

- Drafted the canonical `task-702` plan around the existing packaged seams: `kb_sync_state` in `agentic/src/agentic_kb/sync/state.py`, normal `status` in `agentic/src/agentic_kb/commands/status.py`, and MCP `kb_status` reusing that same status payload.
- Chose the minimal implementation shape that fits repo reality: add one packaged stale-detection helper module and surface freshness through normal status and MCP instead of inventing a separate command or new persistence model.
- Locked docs/code freshness to repo-baseline comparison against current local `HEAD`, and locked GitHub/Project freshness to lightweight remote watermark probes rather than full ingest or auto-sync behavior.
- Kept important boundaries explicit: no new daemon, no auto-sync, no `sync` command redesign, no healthcheck freshness work, and no overclaim that Project cursor-based sync can fully replay already-seen item edits.
- Recorded the main critique targets for the next review pass: the exact status payload shape for freshness, whether stale results should degrade overall status while keeping exit codes unchanged, and the narrowest safe remote-probe seam for GitHub and Project metadata.

Critiquer: Iteration 2
Timestamp: 2026-03-29T17:48:59Z
Outcome: requires_changes

- The plan currently changes the meaning of top-level normal `status.ok` too aggressively. In shipped code, `collect_status_report()` computes `report.ok` as the aggregate runtime/dependency/database readiness bit, and `.agent/workflows/agentic-kb.md` says imported baseline validation expects `status --json` to report `ok = true` after bootstrap. Making stale or missing freshness force `report.ok = false` would turn a valid imported-but-not-yet-refreshed KB into globally degraded status, which is broader than task-702 and risks breaking existing operator/MCP consumers. Keep freshness separate from readiness in the report shape, or add a distinct freshness aggregate instead of reusing the existing top-level `ok`.
- The remote-probe portion is still underspecified for a “smallest correct” implementation. Current GitHub and Project code only expose full ingest-oriented fetch paths (`ingest/github.py`, `ingest/project.py`) that parse and embed full entities/pages. The plan says to add “lightweight remote metadata probes” but does not pin the exact seam or strict minimum request contract. Without that, task-702 can easily sprawl into new GraphQL query design plus per-stream REST helpers. Tighten the plan to require one metadata-only helper per remote surface that returns just the latest remote watermark(s), with unit tests against mocked API payloads, and explicitly avoid reusing embedding/write paths.
- The acceptance and verification sections should explicitly cover the clean-bootstrap/imported-baseline case from `task-901`: after snapshot import with historical baselines, normal `status --json` must still remain a successful readiness report while separately surfacing freshness lag. Right now the plan references task-901 as a boundary, but it does not require a regression test that proves task-702 does not invalidate the shipped bootstrap workflow.

Decision: requires_changes

Planner: Iteration 3
Timestamp: 2026-03-29T17:52:20Z
Outcome: critique_addressed

- Re-read the full canonical `task-702` plan and full append-only planning log, then revised the plan to keep freshness explicitly separate from the shipped readiness contract in `status` and MCP `kb_status`.
- Locked the status/report shape so task-702 adds a distinct freshness section or aggregate without repurposing top-level `StatusReport.ok`, preserving the task-901 imported-baseline/bootstrap readiness contract.
- Tightened the remote contract to the smallest implementation seam: one metadata-only latest-watermarks helper in `agentic/src/agentic_kb/ingest/github.py` and one metadata-only latest-watermark helper in `agentic/src/agentic_kb/ingest/project.py`, both explicitly excluding embeddings, DB writes, and reuse of the full ingest path.
- Added explicit acceptance and verification requirements for mocked-payload unit coverage of those remote metadata helpers so task-702 does not sprawl into ingest-style behavior just to read freshness watermarks.
- Added an explicit task-901 regression requirement: after snapshot import with older baselines, `status --json` must still report readiness success while freshness independently reports lag or skipped remote checks.

Critiquer: Iteration 4
Timestamp: 2026-03-29T17:50:27Z
Outcome: approved

- The revised plan now matches the shipped `status` contract in `agentic/src/agentic_kb/commands/status.py`: top-level `StatusReport.ok` remains the readiness aggregate, while freshness is explicitly separated into a distinct section or field. That resolves the earlier readiness/freshness conflation and keeps MCP `kb_status` aligned with existing serialization patterns.
- The remote freshness seam is now pinned tightly enough to stay within task-702 scope. Naming one metadata-only helper in `agentic/src/agentic_kb/ingest/github.py` and one in `agentic/src/agentic_kb/ingest/project.py`, both explicitly excluding embeddings, DB writes, and reuse of full ingest paths, is the smallest repo-consistent extension to the current clients.
- The plan now explicitly protects the `task-901` imported-baseline workflow by requiring readiness `ok = true` to remain valid after snapshot import even when freshness reports lag. The added regression-test requirement makes that boundary enforceable instead of implicit.
- No additional scope creep or task-boundary violations stand out after the revision. The remaining changes are appropriately limited to packaged stale detection, status/MCP surfacing, narrow doc updates, and targeted tests/tracker cleanup.

Decision: approved
