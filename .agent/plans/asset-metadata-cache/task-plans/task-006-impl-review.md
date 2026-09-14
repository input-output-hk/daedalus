Implementation: Iteration 1
Timestamp: 2026-09-14T20:45:00Z

Changes made:
- `source/main/assets/assetMetadataDb.ts`: new. The three-table schema transcribed from the PRD, an open path that cannot throw, a version check, one recreation attempt on failure, and four accessors.
- `source/main/assets/nodeSqlite.d.ts`: new. An ambient declaration covering `DatabaseSync` and `StatementSync` and nothing else, because `@types/node` in this tree is 14.18.1 and predates the module.
- `source/main/assets/assetMetadataDb.realfs.spec.ts`: new. Thirty-two cases in six groups: the on-disk path, every `asset_metadata` constraint, the `asset_image` foreign key, `asset_resolution` states, opening and recovery, and the accessors.
- `tests/jest/shims/nodeSqlite.js`: new. Test-only.
- `jest.config.js`: one `moduleNameMapper` entry. Test-only.

Files touched:
- `source/main/assets/assetMetadataDb.ts`
- `source/main/assets/assetMetadataDb.realfs.spec.ts`
- `source/main/assets/nodeSqlite.d.ts`
- `tests/jest/shims/nodeSqlite.js`
- `jest.config.js`
- `.agent/plans/asset-metadata-cache/task-plans/task-006.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-006-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-006-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

A correction to the approved plan, recorded rather than edited into it:

The Verification Plan lists "a database whose `user_version` is set to a lower non-zero value is recreated empty" as its own case. With `ASSET_METADATA_DB_VERSION` at 1 there is no lower non-zero value: 0 means a fresh file and is handled by applying the schema. The case is unreachable today and was not written. What the spec asserts instead is that the rule is a disequality rather than a one-sided comparison, by driving two values above the current one, `ASSET_METADATA_DB_VERSION + 1` and 97, and asserting both are recreated and restamped. The below-range case becomes reachable the first time the version is raised, and the rule the code implements already covers it.

Two details decided during implementation:

A read that throws part-way returns the rows it has rather than an empty array. A chunked read that failed on its third chunk has two chunks of correct rows in hand, and discarding them would turn a partial failure into a total one for no gain; the subjects in the failed chunk are then misses, which is what a miss already means everywhere else in this design.

The write path returns 0, not a partial count, when the transaction itself fails. A per-row rejection is counted out and the batch continues, which is the case the plan measured. A failure of `BEGIN`, `prepare` or `COMMIT` is a different thing: nothing is known to have committed, so the honest count is zero.

Verification run:

- `jest source/main/assets --coverage=false` under the Node the checks use, 24.15.0 from `nix/internal/common.nix:220` — 32 passed.
- Every `CHECK` in the schema is driven by a row that violates it and by a row that satisfies it: the subject composition rule with an empty asset name as its own case, `verified` at 0, 1 and 2, `decimals` at -1, 0, 20, 21 and null, `source` outside its two values, a registry row carrying a slot, a chain row carrying a sequence number, and `asset_resolution.state` outside its four.
- `STRICT` is asserted directly, by storing text in `decimals`.
- The foreign key is asserted by inserting an `asset_image` row for a subject with no metadata row, which fails only if `PRAGMA foreign_keys = ON` actually took effect.
- Recovery is driven four ways: a version ahead, a version of 97, a file overwritten with text, and a file overwritten with text alongside a stale `-wal` sibling. Each produces an empty working cache and a restamped version.
- The directory is deleted under a live handle and the next open gives a working empty cache, which is the case a user following a reset instruction produces.
- A path that cannot be created, a file where the directory belongs, yields an instance whose reads are empty, whose writes report zero and whose close does not throw.
- A batch with one `decimals = 21` row in the middle writes the other two and returns 2, with the rows either side present and the offending one absent.
- A 600-subject read crosses the 500-subject chunk size and returns all 600, which is what proves the chunking joins its results.

Checks, all four through Nix with every new file staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built `3av6vds2fyp3a8w2yd2jfnkvl94hjy37-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built `d6pgsjqkmg6l1y2jmkjri9lfg3dkiwq2-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 77 suites passed, 1040 tests with 1037 passed and 3 skipped, exit 0. The previous state of this branch was 76 suites and 1008 tests, so one suite and thirty-two tests were added and nothing else moved.

`nix fmt` was run and changed two files, both new, before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`. `git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- The spec is `assetMetadataDb.realfs.spec.ts` rather than the task graph's `assetMetadataDb.spec.ts`. Recorded in the plan under Live Repo Findings and Required Docs before the work, not discovered during it.
- The lower-version case was not written, for the reason under the correction above.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-14T20:52:00Z

Acceptance criteria, each against the evidence:

1. *`yarn test:jest` passes against a temporary database file.* Met, through `nix build '.#checks.x86_64-linux.jest'`. Each case gets its own `mkdtemp` directory and removes it afterwards, so no case can see another's rows.

2. *Deleting the directory while the process is running and reopening produces a working empty cache.* Met, and driven rather than reasoned about: the row is written, the directory is removed under the open handle, and the reopened cache both reads empty and accepts a fresh write.

3. *A decimals value of 21 is rejected by the engine, asserted directly rather than by inspection.* Met. The typed accessors cannot express the violating row, so the spec goes at the engine with a raw statement, which is the only form of this assertion that proves anything. 20 is asserted accepted in the same group, so the boundary is stated from both sides.

4. *`asset_resolution` records state for every attempt outcome, including the `unregistered` case.* Met. All four states round-trip through the typed accessor and a fifth is rejected by the engine.

5. *Every `CHECK` in the PRD schema is asserted by a row that violates it.* Met, six of six, plus the `asset_resolution` state check and the `STRICT` typing.

6. *`compile`, `lint` and `i18n` green from `nix build`.* Met, all three.

7. *No new `@ts-ignore` and no new `@ts-expect-error`.* Met.

8. *`package.json` and `yarn.lock` unchanged.* Met.

The correction about the lower-version case is the right shape: it says what the plan asked for, why it cannot exist yet, and what was asserted instead. A spec that quietly skipped it would have left a reader believing a case was covered.

One thing worth naming for the tasks that follow. The Jest mapping is the only way this repository can test anything built on `node:sqlite` while Jest stays at 27.5.1, and it is already in place, so `task-010`, `task-011` and `task-012` inherit it and need no further configuration. The shim fails loudly rather than silently if `process.mainModule` ever goes away, which is the failure mode to want.

Summary: The schema is the PRD's, unaltered, and every constraint in it is now asserted against the engine rather than against the DDL. The open path cannot fail startup: a version mismatch, a corrupt file, a deleted directory and an uncreatable path each degrade to an empty cache, and three of those four are driven in the spec. Nothing imports the module yet; `task-010` is where it is sequenced.

Decision: approved
