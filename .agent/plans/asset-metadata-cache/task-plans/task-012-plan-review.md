Planner: Iteration 1
Timestamp: 2026-09-15T02:10:00Z

Plan Summary:
- Created `.agent/plans/asset-metadata-cache/task-plans/task-012.md` with the twenty-one sections the plan-workspace readme requires.
- Classified the task `agent_execution`. A temporary database and test-sized bounds cover every criterion.
- Two methods on the database module, two constants and two call sites in the image store, and cases in the existing spec.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the two bounds at `:480-486` and the row-read argument at `:337-339`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-012` entry.
- `source/main/governance/anchorCache.ts:10-31` and `:80-127`, the named precedent.

Repo-Verified Findings Used To Shape The Plan:
- `anchorCache.ts:29-31` derives its sweep floor from the two caps rather than typing it, and `:90` returns early below it.
- 64 MiB over the 256 KiB per-entry cap gives a floor of 256 entries, below which neither bound can bite.
- SQLite reads a whole row to reach any column, which is the PRD's own reason for separating the two tables, so `sum(byte_length)` reads every blob while `count(*)` does not.
- `asset_image_fetched_at` exists and orders the eviction; it does not cover `byte_length`.
- An `UPDATE` of one column rewrites the whole row, blob included.
- The foreign key cascades from metadata to image and not the reverse.

Planned Approach:
- Derived sweep floor, count before sum, oldest-first walk, one delete statement, a rate-limited touch on read, and enforcement after a write.

Scope Guard / Self-Review:
- No bound on metadata, no new index, no timer, and no knowledge of what the user holds.

Outcome: Canonical task plan drafted and ready for critique

Critique of Iteration 1
Timestamp: 2026-09-15T02:18:00Z

Two gaps, in descending severity.

1. The sweep floor rests on an invariant nothing enforces. The derivation says that below 256 rows the byte bound cannot be crossed, and that is true only if no row exceeds the 256 KiB per-entry cap. Today that holds because the image store checks the cap before calling `writeImage`. But the cap lives in one module and the write lives in another, and the floor's correctness now depends on a caller's discipline rather than on the data. One future writer that skips the check turns the early return into a silent failure to enforce a bound, which is the worst shape a cache bound can take: it looks enforced. `writeImage` should refuse a row over the cap itself, which makes the invariant a property of the table rather than a convention.

2. Every bound case is driven at test-sized values, so nothing asserts that the shipped path uses the shipped numbers. The criterion says "neither bound can be exceeded after a write", and a suite that only ever calls `enforceImageBounds(3, 500)` proves the algorithm while leaving the configuration untested. Writing 64 MiB of blobs to test it properly is not the answer. Making the store's bounds injectable, defaulting to the constants, and asserting the default is: it is the same shape as the injected clock already in that module, and it turns the question from "does the algorithm work" into "does the shipped path carry the shipped numbers", which are different questions and both worth answering.

Nothing else in the plan is wrong. Rate-limiting the touch because SQLite rewrites the whole row is the right reasoning from the right fact, and it is the same fact the PRD used to separate the two tables in the first place, which is a good sign that the fact is load-bearing rather than decorative.

Outcome: requires_changes

Planner: Iteration 2
Timestamp: 2026-09-15T02:25:00Z

Plan Updates In Response To Critique:
- `writeImage` refuses a row over the per-entry cap, so the sweep floor's invariant is a property of the table rather than of a caller. Two cases.
- The image store's bounds are injectable and default to the exported constants, with a case asserting the defaults and one asserting the post-write enforcement uses them.

Resulting Approved Plan Shape:
- Two methods on the database module, one of which now enforces the per-entry cap; two injectable bounds in the store defaulting to the shipped constants; a derived sweep floor; a rate-limited touch; and enforcement after a write.

Scope Guard / Self-Review:
- The revision moves one check into the table's own writer and makes two numbers injectable. It changes nothing about the eviction order or the bounds themselves.

Outcome: Canonical task plan revised after critique and approved for build execution
