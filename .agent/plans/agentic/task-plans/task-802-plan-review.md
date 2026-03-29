Planner: Iteration 1
Timestamp: 2026-03-29T15:49:03Z
Outcome: initial_plan_documented

- Drafted the canonical `task-802` plan around the shipped `agentic-kb mcp-search` stdio contract and the current Compose `mcp-search` parity-harness role so the setup docs do not regress into older daemon-style MCP wording.
- Kept scope intentionally narrow to agent-facing setup docs, with `agentic/README.md` as the primary target and explicit deferral of workflow reconciliation to `task-803` and index cleanup to `task-804`.
- Pinned the main documentation decisions that implementation must preserve: prefer a Compose-backed client launch path, explain that agent clients spawn stdio sessions instead of talking to a network endpoint, and distinguish required MCP runtime env vars from `GITHUB_TOKEN`, which is optional for read-only MCP but needed for sync/project workflows.
- Called out the main open questions for review before implementation: the exact OpenCode and Claude Code config schema shapes, whether to mention direct local `agentic-kb mcp-search` as an advanced optional path, and how to present env guidance accurately without a checked-in `agentic/.env.example`.

Critiquer: Iteration 1
Timestamp: 2026-03-29T15:52:20Z
Outcome: requires_changes

- The plan correctly keeps `task-802` scoped to `agentic/README.md` setup docs and defers the broader workflow/index reconciliation to `task-803` and `task-804`, which matches the task tracker and the tracking-adjustment research.
- The biggest gap is that the implementation approach still leaves the primary MCP launcher as a "likely command shape" instead of pinning the exact non-TTY stdio spawn form the docs must use and verify. For this task, the plan should explicitly require the final examples to use the concrete client-spawned Compose command shape that matches the shipped stdio process, rather than leaving that decision open during implementation.
- The verification plan should add the direct source-of-truth check against `agentic/src/agentic_kb/cli.py` and/or the task-801 implementation evidence so the docs are verified against the actual packaged `agentic-kb mcp-search` CLI contract, not only against README/workflow/Compose wording.
- The plan currently frames the absence of `agentic/.env.example` as an open question, but the repo reality is already clear: that file does not exist. The plan should treat inline env documentation as the expected path for `task-802`, not as an unresolved decision that could drift toward extra scope.
- The acceptance/verification text should also make explicit that `agentic/README.md` must stop implying `docker compose ... up -d mcp-search` is a user setup flow for MCP clients, because the current README example is exactly the kind of parity-harness confusion this task is supposed to fix.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-29T16:05:00Z
Outcome: critique_addressed

- Revised the canonical `task-802` plan to pin the exact client-spawned non-TTY MCP launcher the setup docs must use: `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`, while still allowing client-specific config schemas to express the same argv as split command/args fields.
- Tightened acceptance and verification so implementation must check the README wording directly against the packaged CLI contract in `agentic/src/agentic_kb/cli.py`, the shipped Compose service shape in `docker-compose.agentic.yml`, and the task-801 research evidence that `agentic-kb mcp-search` is the only approved stdio runtime.
- Removed the prior ambiguity around env docs by treating inline `agentic/README.md` environment documentation as the expected task-802 path, since `agentic/.env.example` does not exist and adding one would broaden scope unnecessarily.
- Made the README correction explicit in the plan: the final setup docs must stop implying `docker compose -f docker-compose.agentic.yml up -d mcp-search` is the user MCP setup flow, preserving that command only as parity/smoke-harness context if it is mentioned at all.

Critiquer: Iteration 2
Timestamp: 2026-03-29T15:54:15Z
Outcome: approved

- The revised plan now meets the launcher accuracy bar: `agentic/src/agentic_kb/cli.py` confirms `agentic-kb mcp-search` as the shipped stdio entrypoint, and `docker-compose.agentic.yml` confirms `mcp-search` is the matching Compose service that can be client-spawned with the pinned no-TTY form `docker compose -f docker-compose.agentic.yml run --rm -T mcp-search`.
- Verification is now anchored to actual runtime evidence rather than doc-to-doc consistency alone, because the plan explicitly requires checking README wording against both the packaged CLI contract and the Compose service/env contract, which matches the task-801 research findings.
- The env guidance expectation is now correctly resolved instead of deferred: inline `agentic/README.md` documentation is the right scope for this task given the current repo state, and the plan properly distinguishes direct local runtime requirements from the broader shared-env parity contract.
- Scope boundaries are now explicit and correct: README setup docs own the copy-pasteable OpenCode, Claude Code, and `.mcp.json` examples for the shipped runtime, while parity/smoke-harness explanation stays separate and broader workflow/index reconciliation remains deferred to `task-803` and `task-804`.

Decision: approved
