Planner: Iteration 1
Timestamp: 2026-03-29T18:09:11Z
Outcome: initial_plan_documented

- Drafted the canonical `task-804` plan around a narrow index-reconciliation scope: verify `AGENTS.md`, `CLAUDE.md`, and `.agent/readme.md` against the now-final `.agent/workflows/agentic-kb.md` workflow and `agentic/README.md` MCP setup docs.
- Kept the ownership boundaries explicit so implementation stays documentation-only: the workflow doc remains the operator source of truth, `agentic/README.md` remains the client-setup source of truth, and the three index docs should only route agents to those places rather than duplicating their content.
- Captured the main repo-state gap to fix: the current index docs already list `/agentic-kb`, but they still describe it generically and do not explicitly tell agents where the finalized MCP setup examples live.
- Pinned the main acceptance bar for critique: updated index docs must use shipped-current KB/MCP wording, preserve the stdio-only read-only MCP contract indirectly by linking to the right source docs, and avoid overstating the clean-bootstrap or shared-snapshot story beyond what the finalized workflow already documents.

Critiquer: Iteration 1
Timestamp: 2026-03-29T18:10:55Z
Outcome: requires_changes

- The plan is missing one verification check that matters for this exact index-routing task: it never requires validating the actual relative links from each edited doc to the intended source-of-truth docs. `AGENTS.md` and `CLAUDE.md` live at repo root, but `.agent/readme.md` lives one directory deeper, so a naive "point to `agentic/README.md`" edit can easily ship a broken path there. Add an explicit acceptance/verification requirement that every new KB/MCP cross-reference resolves correctly from its file location.
- The implementation approach says to keep the docs short and navigational, but it does not explicitly protect the current audience split strongly enough during verification. `CLAUDE.md` is a Claude-specific quick reference, while `AGENTS.md` and `.agent/readme.md` are broader indexes. The plan should require preserving each document's existing navigation role while adding the KB/MCP routes, so implementation does not flatten them into near-duplicates just to satisfy consistency.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-29T18:18:00Z
Outcome: plan_revised

- Revised the canonical `task-804` plan narrowly to address both critique points without broadening scope.
- Added explicit scope, implementation, acceptance, and verification requirements to preserve document roles: `CLAUDE.md` stays a Claude-specific quick reference, while `AGENTS.md` and `.agent/readme.md` stay broader agent indexes.
- Added an explicit per-file relative-link verification requirement so implementation must confirm KB/MCP links resolve correctly from each document's own location, including `.agent/readme.md`'s different path context.
- Kept the task bounded to index reconciliation plus planning artifacts only; no implementation or research expansion was added.

Critiquer: Iteration 2
Timestamp: 2026-03-29T18:12:35Z
Outcome: approved

- The revised plan now closes the two concrete readiness gaps from Iteration 1: it explicitly requires preserving the distinct roles of `CLAUDE.md` versus the broader agent indexes, and it requires per-file relative-link verification so `.agent/readme.md` cannot accidentally ship broken KB/MCP cross-references.
- The scope remains appropriately narrow and implementation-ready. It stays limited to index reconciliation against the finalized source-of-truth docs, routes KB workflow guidance to `.agent/workflows/agentic-kb.md`, routes client setup examples to `agentic/README.md`, and explicitly guards against drifting into workflow/runtime/doc expansion.
- The verification plan is now sufficient for this task: it checks doc-role preservation, shipped-current wording, per-file link resolution, and consistency with the task-802, task-803/task-804, and task-901 durable findings.

Decision: approved
