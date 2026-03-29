Implementation: Iteration 1
Timestamp: 2026-03-29T18:13:34Z
Outcome: completed

- Updated `AGENTS.md` so the `/agentic-kb` workflow row now describes the shipped KB surface in current terms and explicitly routes KB workflow guidance to `.agent/workflows/agentic-kb.md` plus MCP client setup examples to `agentic/README.md`.
- Updated `CLAUDE.md` with the same KB routing at quick-reference level while keeping the file Claude-specific and concise.
- Updated `.agent/readme.md` so the workflow index and adjacent note explicitly split KB operator guidance to `./workflows/agentic-kb.md` and MCP client setup examples to `../agentic/README.md`.
- Files touched: `AGENTS.md`, `CLAUDE.md`, `.agent/readme.md`, `.agent/plans/agentic/task-plans/task-804.md`, `.agent/plans/agentic/task-plans/task-804-impl-review.md`.
- Verification run: re-read the edited sections in all three index docs against `.agent/workflows/agentic-kb.md` and `agentic/README.md` for wording consistency; verified the KB/MCP link targets exist from repo reality with `test -f` checks for `.agent/workflows/agentic-kb.md` and `agentic/README.md`; confirmed the `.agent/readme.md` references use its own relative path context.
- Deviations from approved plan: none.

Code Review: Iteration 1
Timestamp: 2026-03-29T18:14:45Z
Outcome: approved

- Reviewed the implementation against the approved canonical plan and found it stays within the intended scope: only the three index docs were updated to route agents to `.agent/workflows/agentic-kb.md` for workflow guidance and `agentic/README.md` for MCP client setup examples.
- Verified the updated wording remains index-level, preserves the document-role split (`CLAUDE.md` as Claude-specific quick reference; `AGENTS.md` and `.agent/readme.md` as broader indexes), and stays aligned with the shipped-current KB/MCP contract documented in the workflow and README.
- Verified the referenced KB/MCP targets exist and that `.agent/readme.md` uses the correct relative path context (`./workflows/agentic-kb.md`, `../agentic/README.md`). No broken-link or role-drift issues found.

Decision: approved
