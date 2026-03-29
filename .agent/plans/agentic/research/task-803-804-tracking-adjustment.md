# Task 803 And 804 Tracking Adjustment

- Date: 2026-03-27
- Evidence: `.agent/plans/agentic/knowledge-base-platform-tasks.json`, `.agent/workflows/agentic-kb.md`

## Durable Findings

- `task-803` and `task-804` should remain pending even though initial documents already exist, because their remaining work is to reconcile those docs with the final implemented KB workflow and MCP setup after dependencies land.
- The better task framing is maintenance-oriented, not creation-oriented:
  - `task-803` is an update task for `.agent/workflows/agentic-kb.md`
  - `task-804` is a check-and-update task for `AGENTS.md`, `CLAUDE.md`, and `.agent/readme.md`
- Keeping these tasks pending preserves dependency integrity while still acknowledging that early draft documents were created ahead of implementation.
- After `task-803`, the durable workflow contract is current-state rather than aspirational: `.agent/workflows/agentic-kb.md` should describe only shipped KB behavior and link to `agentic/README.md` for copy-paste MCP client configs instead of carrying plan-era future-tense setup language.
- `task-803` also surfaced a durable operator caveat that remains relevant for later docs/index cleanup in `task-804`: `sync changed` shells out to `git` from `agentic/src/agentic_kb/commands/sync.py`, but the shipped `kb-tools` image built from `agentic/Dockerfile` does not install `git`, so the documented Compose/container path must be described as a current limitation until a runtime task fixes the image.
