Implementation: Iteration 1
Timestamp: 2026-09-14T19:03:00Z

Changes made:
- `source/renderer/app/utils/assets.ts`: `getUniqueId` removed. It was the last declaration in the file and had no reader.
- `source/renderer/app/api/assets/requests/getUnknownAsset.ts`: file removed. The sibling `getAssets.ts` still has readers, so the directory survives.
- `source/renderer/app/api/assets/types.ts`: `GetUnknownAssetRequest` removed. Its only reader was the module removed with it.
- `source/main/config.ts`: `FALLBACK_TOKEN_METADATA_SERVER_URL` and the comment above it removed. `MOCK_TOKEN_METADATA_SERVER_URL` and `MOCK_TOKEN_METADATA_SERVER_PORT` two declarations below are untouched.

Files touched:
- `source/renderer/app/utils/assets.ts`
- `source/renderer/app/api/assets/requests/getUnknownAsset.ts` (deleted)
- `source/renderer/app/api/assets/types.ts`
- `source/main/config.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-004.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-004-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-004-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Verification run:

The four names, re-grepped over the whole repository at the same width used for planning, excluding `node_modules` and `coverage`. Remaining hits by file:

```
 5 .agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md
 6 .agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json
 5 .agent/plans/asset-metadata-cache/task-plans/task-004-plan-review.md
25 .agent/plans/asset-metadata-cache/task-plans/task-004.md
```

Four plan documents and no source file, which is the expected result: the PRD and the task graph describe this deletion and the two plan documents are this task's own record.

Checks, all four through Nix on a dirty tree, each built locally rather than substituted:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built `lzlxnb6wji8gchvbymawmkn8sw8404ag-daedalus-compile.drv`, exit 0. This is the check that proves no importer of the removed module survives.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built `hriljhv22hbay8skrbcbbl3mnkrrh6v2-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built `5yarq7qszb6wzymww8x3np32kclkw45v-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 75 suites passed, 989 tests with 986 passed and 3 skipped, exit 0. Identical to the previous task's result, which is what a deletion of unreferenced code should produce: no suite gained, none lost, none changed.

`stylelint` and `storybook` were not run, per the plan: no SCSS and no story is touched and neither check reads any of the four files.

`git diff --stat` shows four source files, one of them a deletion, plus this task's plan documents and the task graph. Nothing adjacent was tidied.

Deviations from the approved plan:
- None. All four declarations named in the task graph were removed and nothing else.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-14T19:05:00Z

Acceptance criteria, each against the evidence:

1. *`yarn lint` and `yarn compile` pass.* Met, both through `nix build`, both built locally.

2. *A repository-wide grep for each of the three names returns nothing outside history.* Met. The only remaining hits are the four plan documents that describe the deletion, enumerated above by file and count so the claim can be re-checked rather than taken on trust.

3. *`jest` and `i18n` green from `nix build`.* Met, and the jest count is unchanged at 75 suites and 989 tests, which is the signal that nothing under test reached the removed code.

4. *The deletion is confined to the four declarations.* Met. The diff is three removed blocks and one removed file. `MOCK_TOKEN_METADATA_SERVER_URL` and `MOCK_TOKEN_METADATA_SERVER_PORT`, two declarations below the removed constant in the same file, are untouched, as are the thirteen surviving exported types in `api/assets/types.ts`.

5. *No new `@ts-ignore` and no new `@ts-expect-error`.* Met. The diff removes code and adds none.

One thing worth stating plainly rather than leaving implicit: the value of this commit is not the four lines it removes, it is that `getUniqueId` spelled a subject key as asset name followed by policy id while every live site in the codebase, including the derivation `task-002` added two commits ago, spells it policy id followed by asset name. A dead helper that contradicts the convention it looks like it implements is worse than no helper, and the cache's primary key is written against that convention next.

Summary: A deletion that compiles, whose search is recorded at the width it was run, and whose test count did not move.

Decision: approved
