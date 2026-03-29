Planner: Iteration 1
Timestamp: 2026-03-29T16:04:06Z
Outcome: initial_plan_documented

- Drafted the canonical `task-803` plan around a narrow workflow-reconciliation scope: update `.agent/workflows/agentic-kb.md` so it matches the shipped CLI, sync, snapshot, and stdio MCP behavior rather than the earlier aspirational platform wording.
- Locked the main documentation boundaries before implementation: keep runtime/source files unchanged, defer `AGENTS.md`/`CLAUDE.md`/`.agent/readme.md` cleanup to `task-804`, and avoid duplicating the detailed client setup examples already finalized in `agentic/README.md`.
- Pinned the operator-facing contracts the workflow must preserve, including `sync changed` baseline requirements, `sync all` ordering, destructive snapshot import with manifest validation, and the exact stdio MCP launcher `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`.
- Recorded the main risks to resolve during critique and implementation: avoiding README/workflow drift around MCP setup, keeping future stale-detection language out until `task-702` lands, and choosing the right level of detail for current `status` inspection output without overcommitting to internals.

Critiquer: Iteration 1
Timestamp: 2026-03-29T16:05:59Z
Outcome: requires_changes

- The plan is too documentation-only for one shipped operator path that it intends to preserve: `.agent/workflows/agentic-kb.md` currently documents `docker compose -f docker-compose.agentic.yml run --rm kb-tools sync changed`, but the shipped implementation shells out to `git` in `agentic/src/agentic_kb/commands/sync.py` and the shipped `agentic/Dockerfile` does not install `git`. As written, task-803 could "reconcile" the workflow to the CLI source and still leave a runtime-inaccurate workflow for the documented container path. The plan should explicitly require either documenting this current limitation in the workflow or recording a follow-up dependency, instead of implying the documented command is fully usable today.
- The verification plan is not grounded enough in shipped behavior for a workflow-doc task. It only requires re-reading the workflow against `cli.py`, command modules, `docker-compose.agentic.yml`, and research, but it does not require checking the actual shipped container contract (`agentic/Dockerfile`) or smoke-validating the user-facing commands through the documented Compose path. That gap is exactly how the missing-`git` runtime mismatch survives. Add explicit verification of the documented compose-run command surface, or at minimum verify every documented prerequisite against the built image contents.
- The plan correctly says to avoid README duplication, but it is not explicit enough about removing the workflow's current aspirational MCP/setup wording that is already stale against shipped docs. The current workflow still contains future-tense sections like `Expected tools` and `Required environment variables will likely include ... GITHUB_TOKEN`, while shipped behavior in `agentic/README.md` says `GITHUB_TOKEN` is optional for read-only MCP search/status and pins the exact stdio launcher. The plan should require eliminating those stale MCP/setup claims rather than only summarizing the launcher contract.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-29T16:07:10Z
Outcome: critique_addressed

- Revised the canonical `task-803` plan so the workflow update must explicitly document the current shipped `sync changed` container limitation: `commands/sync.py` shells out to `git`, but `agentic/Dockerfile` does not install `git`, so the Compose `kb-tools` path cannot be described as fully working without a caveat or follow-up reference.
- Strengthened verification beyond source-file consistency by requiring checks against `agentic/Dockerfile`, `docker-compose.agentic.yml`, and the documented Compose/container command path, with a lightweight compose-run sanity check when feasible and explicit image-prerequisite review even when full runtime smoke coverage is not practical.
- Tightened implementation and acceptance expectations around MCP/setup cleanup so `.agent/workflows/agentic-kb.md` must remove the remaining stale future-tense wording such as `Expected tools`, `The implementation should ship`, and `Required environment variables will likely include ...`, replacing it with shipped-current statements aligned with `agentic/README.md`.
- Recorded the missing-`git` image/runtime mismatch as an explicit residual risk and allowed docs/tracking follow-up path, while preserving the task boundary that runtime fixes belong outside `task-803` unless separately scheduled.

Critiquer: Iteration 2
Timestamp: 2026-03-29T16:07:57Z
Outcome: approved

- The revised plan now cleanly covers the missing-`git` limitation in shipped behavior. It explicitly requires the workflow update to document that `sync changed` shells out to `git` from `agentic/src/agentic_kb/commands/sync.py` while the shipped `agentic/Dockerfile` does not install `git`, so the documented Compose `kb-tools` path cannot be presented as fully working without that caveat.
- Verification is now grounded in the actual user-facing path rather than only source review. The plan adds checks against `agentic/Dockerfile` plus `docker-compose.agentic.yml` and requires a documented Compose/image prerequisite review, with a lightweight compose-run sanity check when feasible. That is sufficient for this docs-only task even if a full seeded `sync changed` smoke run is not practical in planning.
- The MCP/setup cleanup gap is now directly addressed in scope, approach, and acceptance criteria. The plan explicitly requires removing stale future-tense wording from `.agent/workflows/agentic-kb.md` and replacing it with shipped-current statements aligned with `agentic/README.md`, including the stdio-only launcher contract and `GITHUB_TOKEN` being optional for read-only MCP search/status but still required for `sync github` and `sync project`.
- Scope discipline remains intact. The plan still keeps runtime fixes, broader agent-index cleanup, and unrelated docs expansion out of `task-803`, while allowing only minimal tracking/research updates if the missing-`git` limitation needs a durable follow-up breadcrumb beyond the workflow doc itself.
- The current workflow document still contains the stale MCP/setup wording and does not yet document the missing-`git` caveat, but that is now an implementation-state issue, not a remaining planning gap.

Decision: approved
