Planner: Iteration 1
Timestamp: 2026-09-15T00:15:00Z

Plan Summary:
- Created `.agent/plans/asset-metadata-cache/task-plans/task-010.md` with the twenty-one sections the plan-workspace readme requires.
- Classified the task `agent_execution`. A stubbed transport and a temporary database cover every criterion.
- Two new files. Nothing existing changes.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the decimals resolution order at `:1134-1141`, the corpus table at `:1145-1155`, the freshness argument at `:1019-1047` and the advisory at `:1213-1232`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-010` entry plus `task-014`, `task-016` and `task-019`.
- The three completed phase-2 plans, for the surfaces this module sequences.

Repo-Verified Findings Used To Shape The Plan:
- The exact exported surface of `assetMetadataDb.ts`, `assetRegistryClient.ts` and `assetVerification.ts` at `41f98a5d6`.
- `retry_after` is an absolute instant, and both writes take an explicit timestamp so the client and the database can share one clock.
- Four separate places in the PRD describe the `verified` column's behaviour and every one of them is about `decimals`.
- `asset_metadata` refuses a decimals value above 20, so an out-of-range registry value would cost the whole row if written.
- Of 7,976 mapping files, 7,464 sit at sequence number 0 on every property, and 375 commits over a year produced 38 file modifications.

Planned Approach:
- Two primitives, a convenience over them, a three-clause due rule, no database call inside the awaited section, a row built from per-property verdicts, and a callback that emits what resolved.

Scope Guard / Self-Review:
- No IPC, no timer, no image fetch, no user-facing error and no enumeration.

Outcome: Canonical task plan drafted and ready for critique

Critique of Iteration 1
Timestamp: 2026-09-15T00:25:00Z

Four gaps, in descending severity.

1. The refresh path emits rows that did not change. The plan calls the callback with "the rows that were written", and the refresh path writes a row on every successful re-read, including the case where nothing changed and only `updated_at` moved. That is the common case by a wide margin: 7,464 of 7,976 subjects sit at sequence number 0 on every property and have never been edited. So a token list rendered after a week would push a full set of unchanged rows across the IPC boundary and through a MobX store for nothing. Emission should carry the rows whose content changed, and a timestamp-only touch should not be one.

2. `request` starts work nobody can observe. It is the entry point the IPC handler will use and the one most of the behavioural criteria are about, and the plan gives a test no way to know when the background resolve finished. Worse, a rejected background promise with no handler is an unhandled rejection, which Node will eventually make fatal. The resolver needs to keep the in-flight promise and expose it, so a test can await it and so the rejection has an owner.

3. The sequence number comparison has two cases that are not comparisons. A stored `sequence_number` of null is not "lower than" anything: in JavaScript it coerces to zero, so a response at sequence 0 would compare equal and the row would be treated as unchanged when nothing is actually known about it. And a stored row whose `source` is `chain` carries no sequence number at all by construction, while the PRD's precedence rule says the registry wins where both could answer. Both need stating as rules rather than left to coercion.

4. The timer criterion is checked by reading the module's own source in its own spec. That asserts the absence of a string, not the absence of behaviour, and it would pass while a timer lived in a module this one imports. There is a real version of this check: run a resolve to completion under fake timers, advance the clock by a day, and assert the transport was not called again. That states the property the criterion is reaching for.

Nothing else in the plan is wrong. Arguing the meaning of the `verified` column from four places in the PRD, and recording the reading rather than quietly picking one, is the right handling of the only genuinely interpretive decision in the phase.

Outcome: requires_changes

Planner: Iteration 2
Timestamp: 2026-09-15T00:33:00Z

Plan Updates In Response To Critique:
- Emission now carries only rows whose content changed. A timestamp-only touch is written and not emitted, with a case asserting both halves.
- `request` keeps its in-flight promise and the resolver exposes `pending()`, so a test can await it and a rejection has an owner. Background failures are caught and logged rather than left unhandled.
- Stated two rules for the sequence comparison: a stored null means nothing is known, so any response rewrites; and a stored row whose source is not `registry` is always rewritten by a registry response, per the PRD's precedence rule.
- Replaced the source-reading timer check with a behavioural one: resolve to completion under fake timers, advance a day, assert no further transport call.

Resulting Approved Plan Shape:
- Two new files. Read from disk, decide what is due in two statements, fetch outside any database call, verify per property, write, and emit what actually changed.

Scope Guard / Self-Review:
- The revision narrows what is emitted, gives the background work an owner, and replaces one weak assertion with a behavioural one. It changes nothing about what is fetched or stored.

Outcome: Canonical task plan revised after critique and approved for build execution
