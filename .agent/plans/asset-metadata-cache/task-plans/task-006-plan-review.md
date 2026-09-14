Planner: Iteration 1
Timestamp: 2026-09-14T20:05:00Z

Plan Summary:
- Created `.agent/plans/asset-metadata-cache/task-plans/task-006.md` with the twenty-one sections the plan-workspace readme requires.
- Classified the task `agent_execution`. Every acceptance criterion is assertable against a temporary database file and the four Nix checks.
- Four new files and one line of existing configuration: the module, its real-filesystem spec, an ambient type declaration, a Jest shim, and one `moduleNameMapper` entry.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the schema at `:335-493` and the placement at `:494-550`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-006` entry plus `task-010`, `task-011` and `task-012`.
- `.agent/plans/asset-metadata-cache/task-plans/task-005.md` and its review logs, for the standard and for the `blake2b` realm finding.
- `CLAUDE.md` for the spec conventions and the `@ts-ignore` rule.

Repo-Verified Findings Used To Shape The Plan:
- Under the Node the checks use, `pkgs.nodejs_24` at `nix/internal/common.nix:220`, `node:sqlite` reports SQLite 3.50.4 and enforces `STRICT`, `CHECK` and a `BLOB` round trip as a `Uint8Array`.
- Jest 27.5.1 cannot resolve `node:sqlite`: `jest-runtime/build/index.js:1758-1773` strips the `node:` prefix and requires `sqlite`, which does not exist. Three routes were measured and only `vm.runInThisContext('process.mainModule.require.bind(process.mainModule)')` reaches the real module.
- `source/main/config.ts:125` exports `stateDirectoryPath`; `source/main/governance/anchorCache.ts:36-37` is the directory shape to mirror; `:42-49` and `:60-76` are the degradation shape.
- `source/renderer/app/config/assetsConfig.ts:1` sets `MAX_DECIMAL_PRECISION = 20`, which is where the `decimals` bound comes from.
- `source/main/assets` does not exist and `grep -rn "assetMetadataDb\|asset_metadata" source tests` returns nothing.
- Six specs in this tree declare `@jest-environment node`; the default is jsdom at `jest.config.js:147`.

Planned Approach:
- Transcribe the PRD schema unaltered, open behind one function that cannot throw, treat any unexpected `user_version` and any corruption as a reason to delete and recreate, and expose four methods with no enumerating form.

Scope Guard / Self-Review:
- No image reads or writes, no network, no resolution policy, no migration framework and no singleton.

Outcome: Canonical task plan drafted and ready for critique

Critique of Iteration 1
Timestamp: 2026-09-14T20:12:00Z

Four gaps, in descending severity.

1. The batched upsert is designed around an assumption nobody measured. The plan puts each row in its own savepoint so that one bad row does not take its batch down. Measured in this tree under SQLite 3.50.4: a `CHECK` violation inside an explicit transaction aborts the statement, not the transaction, and a `COMMIT` afterwards keeps the rows that succeeded. Three rows were offered, the middle one violating `decimals <= 20`, and two committed. The savepoints buy nothing and cost a nested construct in the one method every later task writes through. A plain `try`/`catch` per row inside one transaction gives the same result.

2. Nothing in the plan says who supplies `updated_at` and `attempted_at`. That is the difference between a cache-freshness fact the module owns and a value a caller can get wrong, and `task-010` reads `updated_at` to decide whether a row is inside its refresh window. If a caller supplies it, a caller can write a future timestamp and freeze a subject out of refresh permanently. The module should stamp both from the clock, with an explicit override parameter that exists so `task-010`'s spec can place a row in the past, and the input row type should not carry either column.

3. The host parameter limit is not addressed, and there are two SQLite builds in play. `WHERE subject IN (?, ?, ...)` uses one bound parameter per subject. Measured here, a 40,000-placeholder statement prepares without complaint, but that is this build; `SQLITE_MAX_VARIABLE_NUMBER` is a compile-time constant and the Electron build is a different one. The failure mode matters more than its likelihood: the read is wrapped so that a throw degrades to a miss, so a wallet large enough to cross the limit would lose the whole cache silently and look exactly like a cold one. Chunking the subject list at a fixed size removes the case entirely for the cost of a loop.

4. Acceptance criterion 9 is not a criterion. "Asserted by reading the module rather than by a test" is an inspection, and an inspection that a later task is free to invalidate. The property it is reaching for, that nothing enumerates the cache, is already carried by the API surface: there is no method that returns rows without a subject list. Say that in Non-Goals, which already does, and drop the criterion rather than keeping a check that checks nothing.

Nothing else in the plan is wrong. Measuring the three Jest routes before choosing one, and recording which line of `jest-runtime` causes the failure, is the right order, and it is what makes the `moduleNameMapper` entry a conclusion rather than a preference.

Outcome: requires_changes

Planner: Iteration 2
Timestamp: 2026-09-14T20:20:00Z

Plan Updates In Response To Critique:
- Replaced the per-row savepoint scheme with a `try`/`catch` per row inside one transaction, and recorded the measurement that settles it: a `CHECK` violation aborts the statement and leaves the transaction usable.
- Gave `updated_at` and `attempted_at` to the module. The input row types no longer carry them, both are stamped from the clock, and one optional parameter exists so a spec can place a row in the past.
- Added subject-list chunking to both reads, with the measurement and the reason the unchunked version would fail silently rather than loudly.
- Dropped acceptance criterion 9 and left the property where it belongs, in Non-Goals as an API-surface statement.

Resulting Approved Plan Shape:
- Four new files and one configuration line. The schema is the PRD's, unaltered. Open cannot throw, an unexpected version or a corrupt file is deleted and recreated once, and four methods carry every read and write with no enumerating form among them.

Scope Guard / Self-Review:
- The revision removes a construct, moves two columns out of the caller's hands and adds a loop. It changes nothing about what is stored or what the schema enforces.

Outcome: Canonical task plan revised after critique and approved for build execution
