Planner: Iteration 1
Timestamp: 2026-03-28T16:53:49Z
Outcome: proposed_initial_plan

- Drafted the canonical `task-204` plan around a packaged search registry at `agentic/src/agentic_kb/search/config.py` that becomes the single source of truth for all seven current searchable entity types.
- Kept the scope library-first for `task-502`: the registry should describe entity/table/text/filter contracts and explicit prefix-match path filters without implementing BM25/vector/RRF query execution or widening schema/index scope.
- Called out the stale tracker path mismatch for `task-204`: the tasks JSON still points at `agentic/src/search/config.py`, and implementation should reconcile it to the packaged `agentic/src/agentic_kb/search/config.py` path before code lands.

Critiquer: Iteration 1
Timestamp: 2026-03-28T16:56:24Z
Outcome: requires_changes

- The project-item filter contract is incomplete against current schema and ingestion reality: `kb_project_items` has a canonical typed `size` column from `task-404`, but the v1 filter set and acceptance criteria omit it. Either add `size` to the approved project filters or state explicitly why this established field is excluded so `task-502` and later CLI/MCP surfaces do not drift.
- The plan needs a sharper contract for registry-level filters versus column-backed filters. `entity_type` is synthetic and `source_domain` is only a real column on `kb_documents`, yet the verification plan currently says filter references should point only to real typed schema columns. Revise the plan so `task-502` can distinguish pre-table/entity-selection filters from per-entity SQL predicates without inventing semantics during implementation.
- Lock the public entity-name contract in the canonical plan instead of leaving it as an open question. The current draft already leans toward plural table-aligned names; make that explicit now so downstream `task-502`/`task-503`/`task-801` code and docs do not need a later rename pass.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-28T16:57:55Z
Outcome: revised_plan_addresses_filter_contract_gaps

- Added a dedicated `Stable Registry Vocabularies` section that locks the public entity-name contract to the plural table-aligned identifiers `documents`, `code_chunks`, `github_issues`, `github_issue_comments`, `github_prs`, `github_pr_comments`, and `project_items`.
- Clarified filter categories so `entity_type` is an explicit registry-level global filter, `source_domain` is registry metadata rather than a pretend universal DB column, and only entity-level filters map to real typed columns or approved prefix-match path fields.
- Expanded the approved Project 5 filter list and acceptance criteria to include the existing typed `size` column from `kb_project_items`, keeping the registry aligned with current schema and ingestion reality.

Critiquer: Iteration 2
Timestamp: 2026-03-28T16:58:49Z
Outcome: approved_plan_ready_for_implementation

- The revised plan cleanly resolves the prior blockers: `size` is now part of the approved Project 5 filter contract, the registry/global-versus-column-backed filter split is explicit, and the public entity-name vocabulary is now locked for downstream tasks.
- The implementation approach, acceptance criteria, and verification plan now line up with current schema/index reality in `agentic/schema/init.sql` and `agentic/schema/create_indexes.sql`, including the explicit handling of prefix-match path filters and non-column registry metadata.
- No further planning gaps block implementation; the remaining work is execution discipline only, especially creating the new packaged `agentic_kb.search` module and reconciling the stale `task-204.targetPath` before code lands.

Decision: approved
