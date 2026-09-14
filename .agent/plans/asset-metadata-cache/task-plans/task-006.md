# Task task-006: Database module: schema, open, version check, point read and write

## Task ID and Title

- ID: `task-006`
- Title: `Database module: schema, open, version check, point read and write`

## Why Chosen Now

`task-006` has no dependencies in the task graph and is the first of the four phase-2 tasks that
build the main-process cache. `task-010`, `task-011` and `task-012` all name it as a dependency, so
nothing else in the phase can be written against a real store until it exists.

It is also the task that decides the shape every later one is written against: what a row is, how a
handle is obtained, and what happens when the file on disk is unusable. Deciding that once, here,
is cheaper than three modules each inventing their own answer.

## Interaction Mode

- Mode: `agent_execution`

Everything is assertable in a Jest spec against a temporary database file and the four Nix checks.
Nothing needs a running node, a network fetch or an operator.

## Scope

- A new `source/main/assets/assetMetadataDb.ts` holding the three-table schema from the PRD, the
  open path with its version check and corruption recovery, and point reads and batched upserts for
  `asset_metadata` and `asset_resolution`.
- A colocated `source/main/assets/assetMetadataDb.realfs.spec.ts` driving that module against real
  files in a temporary directory.
- A Jest module mapping so that `node:sqlite` resolves under Jest 27, with the shim it maps to.

Revertible on its own. Nothing imports the module yet.

## Non-Goals

- No image reads or writes. `asset_image` is created here because the schema is one unit and a
  foreign key from `asset_image` to `asset_metadata` cannot be declared against a table that does
  not exist. Reading and writing it is `task-011`, and evicting from it is `task-012`.
- No network, no fetch, no verification and no resolution policy. This module stores what it is
  given and enforces the constraints; deciding what to store is `task-010`'s.
- No full-table scan and no enumeration API. Nothing in the design lists the cache, and an
  enumerating method would be the first thing a later task reached for instead of batching subjects.
  The guard is the API surface rather than a test: no method returns rows without being handed the
  subjects to return.
- No migration framework. There is one schema version. A file written by another version is deleted
  and recreated, because it holds nothing that cannot be fetched again.
- No singleton and no module-level open. The handle is created by a call, so a spec can point it at
  a temporary path without mocking the module that computes the default one.

## Dependencies

- None in the task graph. `task-006` has `"dependencies": []`.
- Practical dependency: Nix, for the four checks.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`: the schema and its rationale at
  `:335-493`, the placement and on-disk location at `:494-550`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`: the `task-006` entry, plus
  `task-010`, `task-011` and `task-012`, which are its consumers.
- `.agent/plans/asset-metadata-cache/task-plans/task-005.md` and its two review logs, for the
  standard this workspace holds and for the `blake2b` realm finding carried forward from it.

## Docs, Workflows, and Skills Consulted

- Docs:
  - `.agent/plans/asset-metadata-cache/task-plans/readme.md` for the cycle and the section list.
  - `CLAUDE.md` for the spec conventions and the `@ts-ignore` rule.
- Workflows:
  - `.agent/workflows/test.md` for the Jest invocation, read against the `CLAUDE.md` trust map.
- Skills: none apply. No message, no store registration and no IPC channel in this task.

## Live Repo Findings Verified For Planning

Verified at `ca0c7be00` on branch `docs/asset-metadata-cache-plan`, 2026-09-14.

**`node:sqlite` is present and enforces what the schema relies on.** Measured with the Node that
the Nix checks use, `pkgs.nodejs_24` at `nix/internal/common.nix:220`, resolved in the store as
24.15.0:

- `select sqlite_version()` returns `3.50.4`. The PRD measured 3.51.3 inside Electron 41.3.0
  (`package.json:223`). Both are above the 3.37 that `STRICT` requires, and the two engines are
  different builds of the same library reached from the same API, so the schema has to hold under
  both. Every constraint below was re-measured under 3.50.4 rather than carried over.
- `CHECK (b IS NULL OR (b >= 0 AND b <= 20))` rejects 21 with
  `CHECK constraint failed: b IS NULL OR (b>=0 AND b<=20)` and accepts 20.
- A `STRICT` table rejects a text value for an `INTEGER` column with
  `cannot store TEXT value in INTEGER column t.b`.
- A `BLOB` written as a `Uint8Array` round-trips as a `Uint8Array` byte for byte.
- `DatabaseSync` exposes `open, close, prepare, exec, function, location, aggregate, createSession,
  applyChangeset, enableLoadExtension, enableDefensive, loadExtension, setAuthorizer`, and
  `StatementSync` exposes `iterate, all, get, run, columns, setAllowBareNamedParameters,
  setAllowUnknownNamedParameters, setReadBigInts, setReturnArrays`. `run` returns
  `{ changes, lastInsertRowid }`.

**Jest 27 cannot resolve `node:sqlite`, and the reason is specific enough to state.**
`package.json` pins `jest` at 27.5.1. `node_modules/jest-runtime/build/index.js:1758-1773` strips a
`node:` prefix and calls `require` on what is left, so `node:sqlite` becomes `require('sqlite')`,
which is not a module. `node:sqlite` has no unprefixed alias, so nothing resolves it. This is not a
Node version question: the same failure occurs under Node 24.15.0, and
`jest-resolve/build/isBuiltinModule.js` would have accepted the name, because Node 24 lists it in
`module.builtinModules` as `node:sqlite` with the prefix. The resolver agrees and the runtime does
not.

Three routes were measured in this tree before one was chosen:

| Route | Result |
|---|---|
| `import { DatabaseSync } from 'node:sqlite'` in a spec | `Cannot find module 'sqlite'` from `_requireCoreModule` |
| `createRequire(__filename)('node:sqlite')` | same failure: Jest substitutes its own `module` |
| `vm.runInThisContext('process.mainModule.require.bind(process.mainModule)')` | returns the real Node `require`, and `node:sqlite` loads |

The third is what the shim uses, behind a `moduleNameMapper` entry, so the module under test keeps
the plain `import ... from 'node:sqlite'` that it will run with in Electron.

**Where the file goes, and the two paths it is built from.** `source/main/config.ts:125` is
`export const stateDirectoryPath = stateDir;`, and `source/main/governance/anchorCache.ts:36-37`
builds its own directory as `path.join(stateDirectoryPath, ANCHOR_CACHE_DIRECTORY_NAME)`. This
module mirrors that with `asset-metadata-cache`, and puts the database inside it rather than beside
it, because WAL mode creates `-wal` and `-shm` siblings and deleting one directory is then the whole
reset procedure.

**The degradation shape to copy.** `anchorCache.ts:42-49` returns `null` from a read whose
`readFileSync` threw, and `:60-76` swallows a failed write after logging it. Nothing there can fail
startup. The same property is what this module needs, with one addition the file cache does not
have: a single file holds every row, so one unreadable file is the whole cache rather than one
entry.

**The decimals bound is not arbitrary.** `source/renderer/app/config/assetsConfig.ts:1` is
`export const MAX_DECIMAL_PRECISION = 20;`. A verified `decimals` above that would be applied to
amounts automatically while remaining unselectable in the settings dialog, so the engine refuses to
store one.

**The directory does not exist yet.** `ls source/main/assets` reports no such file or directory, and
`grep -rn "assetMetadataDb\|asset_metadata" source tests` returns nothing. There is no colliding
name and no prior art in the repository to match beyond the governance modules above.

**The main-process spec convention.** Six specs in this tree carry
`@jest-environment node` in a leading docblock: `source/main/ipc/open-external-url.spec.ts:2`,
`source/main/ipc/governanceAnchorChannel.spec.ts:2`,
`source/renderer/app/components/governance/drep-directory/helpers.spec.ts:5`, and the three under
`tests/jest/governance/`. The default is `jest-environment-jsdom` (`jest.config.js:147`), which is
the environment that produced the `blake2b` realm failure recorded under `task-005`. A main-process
spec that touches Node built-ins declares the node environment.

**Real-filesystem specs are named apart.** `ensureDirectoryExists.spec.ts` and the five specs
introduced with it established `<Unit>.realfs.spec.ts` for a spec that uses the real filesystem
rather than a mocked one, so that a mocked spec can sit beside it later without the two fighting
over the module registry. This spec writes real files, so it takes that name. That is a deliberate
departure from the `assetMetadataDb.spec.ts` in the task graph's `targetPaths`, recorded here and in
Required Docs rather than made silently.

## Files Expected To Change

- `source/main/assets/assetMetadataDb.ts` — new.
- `source/main/assets/assetMetadataDb.realfs.spec.ts` — new. The task graph names
  `assetMetadataDb.spec.ts`; see the naming finding above.
- `tests/jest/shims/nodeSqlite.js` — new. Test-only.
- `jest.config.js` — one `moduleNameMapper` entry. Test-only.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-006` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-006*.md` — this plan and its two review logs.

No runtime file outside `source/main/assets/` changes, and `package.json` and `yarn.lock` are
untouched.

## Implementation Approach

1. **The schema, transcribed from the PRD rather than paraphrased.** All three `CREATE TABLE`
   statements and both indexes exactly as `asset-metadata-cache-prd.md:344-386` gives them,
   including `STRICT` on each table and every `CHECK`. `asset_image` is created here even though
   nothing writes it until `task-011`, because its foreign key names `asset_metadata`.

2. **Open, in one function that cannot throw.**

   ```ts
   export const openAssetMetadataDatabase = (
     filePath: string = assetMetadataDatabasePath()
   ): AssetMetadataDatabase
   ```

   It creates the containing directory, opens the file, sets `PRAGMA journal_mode = WAL` and
   `PRAGMA foreign_keys = ON`, reads `PRAGMA user_version`, and applies the schema. A returned
   instance is always usable; if the engine could not be reached at all, the instance holds no
   handle and every method answers as an empty cache would.

3. **The version rule, stated as one comparison.** `ASSET_METADATA_DB_VERSION` is `1`. A file
   reporting `0` is new: the schema is applied and the version is stamped. A file reporting the
   current version has its schema reapplied, which is free because every statement is
   `IF NOT EXISTS`. Any other value, ahead or behind, means the file was written by different code,
   so it is closed, deleted with its `-wal` and `-shm` siblings, and recreated empty. The cache
   holds nothing that cannot be fetched again, which is what makes deleting the correct answer
   rather than a lossy one.

4. **Corruption takes the same path as a version mismatch.** A throw from the open, from a pragma or
   from the schema application is caught, the file is deleted with its siblings, and one recreation
   is attempted. A throw from that second attempt leaves the instance without a handle. There is no
   third attempt: two failures in a row is a filesystem the process cannot use, and retrying it on
   every call would turn a degraded cache into a stalled one.

5. **Reads and writes, four methods and no more.**

   ```ts
   readMetadata(subjects: Array<string>): Array<AssetMetadataRow>
   writeMetadata(rows: Array<AssetMetadataWrite>, now?: number): number
   readResolutions(subjects: Array<string>): Array<AssetResolutionRow>
   writeResolutions(rows: Array<AssetResolutionWrite>, now?: number): number
   ```

   Each read is `WHERE subject IN (?, ?, ...)` with one placeholder per subject, which is a point
   read for one subject and a batched point read for many. Neither read has a form that omits the
   `WHERE` clause. Each write is an upsert inside one transaction and returns how many rows it
   wrote, so a caller can tell a rejected row from a stored one.

   The subject list is chunked before it reaches a statement. One bound parameter per subject means
   a long list runs into `SQLITE_MAX_VARIABLE_NUMBER`, which is a compile-time constant of whichever
   SQLite build is loaded: this tree's is generous enough to prepare a 40,000-placeholder statement,
   and the Electron build is a different one. The consequence decides it rather than the
   probability. Reads are wrapped so a throw degrades to a miss, so a wallet large enough to cross
   the limit would lose the entire cache and look exactly like a cold one. A fixed chunk size
   removes the case for the cost of a loop.

6. **A rejected row does not take its batch down, and nothing more is needed to get that.** Measured
   under SQLite 3.50.4: a `CHECK` violation inside an explicit transaction aborts the statement, not
   the transaction. Three rows were offered with the middle one violating `decimals <= 20`, the
   insert raised for that row alone, and the `COMMIT` kept the other two. So each row is attempted
   in its own `try`/`catch` inside one transaction, with no savepoint. The count returned is what
   the caller compares against what it handed in.

7. **`updated_at` and `attempted_at` belong to the module, not to the caller.** Both are
   cache-freshness facts recorded by whoever wrote the row, and `task-010` reads `updated_at` to
   decide whether a subject is inside its refresh window. A caller that could supply it could write
   a future timestamp and freeze a subject out of refresh permanently. So the write row types omit
   both columns, and the module stamps them from the clock. Each write takes an optional explicit
   timestamp, which exists so that a spec can place a row in the past; that is the only reason it
   exists and it is not on the read path.

8. **Types are declared, not inferred from the engine.** `node:sqlite` returns rows as plain
   objects with a null prototype. The module declares `AssetMetadataRow` and `AssetResolutionRow`
   and maps each returned row onto them, so a column rename is a compile error at the boundary
   rather than an `undefined` three modules later.

9. **`node:sqlite` has no types in this tree.** `@types/node` is 14.18.1, which predates the module.
   A minimal ambient declaration for the surface actually used goes in
   `source/main/assets/nodeSqlite.d.ts`, covering `DatabaseSync`, `StatementSync` and nothing else.
   It is type-only, adds no runtime dependency, and is narrower than the real module on purpose: a
   declaration that claims more than the code uses is a claim nothing checks.

10. **The Jest mapping, and why it is in the shared config.** `jest.config.js` gains
   `'^node:sqlite$': '<rootDir>/tests/jest/shims/nodeSqlite.js'`, and the shim reaches the real
   module through `vm.runInThisContext`. The alternative, injecting the constructor into the module
   under test, was rejected: it would leave the production import path untested, and the point of
   this spec is that the engine enforces the constraints rather than that the module believes it
   does.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **`yarn test:jest` passes against a temporary database file.** Run as
   `nix build '.#checks.x86_64-linux.jest' --no-link`.
2. **Deleting the directory while the process is running and reopening produces a working empty
   cache.** Driven directly: open, write, `rm -rf` the directory, reopen, assert the read is empty
   and a subsequent write succeeds.
3. **A decimals value of 21 is rejected by the engine, asserted directly rather than by
   inspection.** The spec writes 21 through the real driver and asserts the row is absent
   afterwards, and asserts that 20 is accepted, so the boundary is stated from both sides.
4. **`asset_resolution` records state for every attempt outcome, including the `unregistered`
   case.** All four states round-trip, and a fifth value is rejected by the engine.

Five this task adds to its own closure:

5. Every `CHECK` in the PRD schema is asserted by a row that violates it, not only by the DDL being
   present.
6. `compile`, `lint` and `i18n` are green from `nix build`, not from host tooling.
7. No new `@ts-ignore` and no new `@ts-expect-error`.
8. `package.json` and `yarn.lock` are unchanged.

## Verification Plan

Execution verification, each case driven rather than implied.

**Schema constraints, one failing row each.**

- `subject` that is not `policy_id || asset_name` is rejected; the matching row is accepted.
- `verified` of `2` is rejected; `0` and `1` are accepted.
- `decimals` of `21` is rejected, `-1` is rejected, `0` and `20` are accepted, `NULL` is accepted.
- `source` of `'koios'` is rejected; `'registry'` and `'chain'` are accepted.
- `source = 'registry'` with a non-null `slot` is rejected; with a `sequence_number` it is accepted.
- `source = 'chain'` with a non-null `sequence_number` is rejected; with a `slot` it is accepted.
- `asset_resolution.state` of `'stalled'` is rejected; `pending`, `resolved`, `unregistered` and
  `failed` all round-trip.
- A `STRICT` violation, a text value in `decimals`, is rejected.
- An `asset_image` row for a subject with no `asset_metadata` row is rejected while
  `PRAGMA foreign_keys` is on, which is what proves the pragma took effect rather than being set
  and ignored.

**Open, version and recovery.**

- A fresh path creates the directory and the file, and `PRAGMA user_version` reads
  `ASSET_METADATA_DB_VERSION`.
- Reopening an existing database preserves its rows.
- A database whose `user_version` is set to `ASSET_METADATA_DB_VERSION + 1` out of band is
  recreated empty, and the row written before is gone.
- A database whose `user_version` is set to a lower non-zero value is recreated empty for the same
  reason, which is the case the task graph does not name and which a one-sided comparison would get
  wrong.
- A truncated file, produced by writing a valid database and then overwriting its first bytes with
  text, is recreated empty rather than throwing.
- Deleting the whole directory under an open handle and reopening gives a working empty cache.
- A path that cannot be created, a file where the directory should be, yields an instance whose
  reads are empty and whose writes report zero, and does not throw.

**Reads and writes.**

- A batched upsert of two rows followed by a point read of one returns that row and not the other.
- An upsert over an existing subject replaces it rather than duplicating it, and the row count in
  the table stays at one.
- A batch containing one constraint-violating row writes the rest and returns a count one lower
  than it was handed, with the violating row absent and the rows either side of it present.
- A read for subjects with no rows returns an empty array rather than throwing.
- A read with an empty subject list returns an empty array without preparing a statement.
- A read for more subjects than one chunk holds returns every row that exists, which is what proves
  the chunking joins its results rather than returning only the last chunk.
- `updated_at` is stamped by the module: a row written without a timestamp comes back with one
  close to the clock, and a row written with an explicit one comes back with exactly that.
- `blob` round-trip is not exercised here; `asset_image` writes belong to `task-011`.

**Commands.**

- `nix build '.#checks.x86_64-linux.jest' --no-link`, with every new file staged.
- `nix build '.#checks.x86_64-linux.compile' --no-link`
- `nix build '.#checks.x86_64-linux.lint' --no-link`
- `nix build '.#checks.x86_64-linux.i18n' --no-link`
- `git diff -- package.json yarn.lock` must be empty.

`perSystem/checks.nix:15-32` builds from `srcWithoutNix`, derived from `inputs.self`, so every new
file must be `git add`ed before a check can see it.

## Risks and Open Questions

1. **The Jest mapping is a workaround for a test runner, and it is in the shared config.** It
   applies to every suite, not only to this one. The blast radius is one module specifier that
   nothing else in the repository imports today, and the failure mode if the shim ever breaks is
   loud: every spec that touches the database fails to load. The alternative, upgrading Jest, is a
   much larger change and is not this task's.
2. **The shim depends on `process.mainModule`.** Under the Jest CLI that is jest's own entry module
   and its `require` is the real Node one. If Jest were ever launched through an ESM entry point,
   `process.mainModule` would be `undefined` and the shim would throw at load. It throws with its
   own message rather than a property access error, so the diagnosis is in the failure.
3. **Two SQLite builds, not one.** The checks run 3.50.4 and Electron ships 3.51.3. Everything this
   module relies on, `STRICT`, `CHECK`, foreign keys and WAL, predates both by years, and each was
   re-measured under 3.50.4 rather than assumed from the PRD's Electron measurement. A constraint
   that behaved differently between them would be a SQLite regression rather than a design risk.
4. **The ambient declaration can collide with a future `@types/node`.** If that package is ever
   raised past the version that declares `node:sqlite`, two declarations of the same module would
   merge and any disagreement would be a compile error. The declaration is deliberately minimal to
   keep the surface that could disagree small, and the error would be at compile time rather than
   at runtime.
5. **WAL in a directory the user can delete.** Deleting the directory under a live handle is in the
   verification plan because it is what a user following a support instruction would do. SQLite
   tolerates it on POSIX, and the next open recreates. On Windows the delete would fail rather than
   succeed silently, which is the safer of the two outcomes.
6. **`metadata` is stored and returned as text.** The module does not parse it, validate it as
   JSON or look inside it. Name resolution reads it in a later phase; storing it opaquely is what
   keeps this module free of the registry's response shape.
7. No open questions for the project owner.

## Required Docs, Research, and Tracking Updates

- Update `task-006`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-006-plan-review.md` and `task-006-impl-review.md` as the cycle requires.
- No PRD change. The schema is transcribed from it unaltered.
- Two task-graph inconsistencies recorded rather than edited from inside this task. The spec is
  named `assetMetadataDb.realfs.spec.ts` where `targetPaths` says `assetMetadataDb.spec.ts`, for the
  convention reason under Live Repo Findings. And `task-012` lists
  `source/main/assets/assetDatabase.ts` in its `targetPaths`, a file this task does not create and
  which is the same module under a second name; `task-012` is where that is settled.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-006-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-006-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Plan drafted, critiqued once and revised. Approved for build execution.

## Final Outcome

- `task-006` complete. Reviewed and approved in `task-006-impl-review.md`.
- The three-table schema is transcribed from the PRD unaltered, and every `CHECK` in it is asserted
  by a row the engine refuses rather than by the DDL being present.
- Jest 27.5.1 cannot resolve `node:sqlite`, because `jest-runtime` strips the `node:` prefix and
  requires a module that does not exist. One `moduleNameMapper` entry and a shim that reaches the
  real Node `require` settle it for every later task in this phase, and the module under test keeps
  the import it will run with under Electron.
- One case in the Verification Plan could not be written: with the schema version at 1 there is no
  lower non-zero value to test. The disequality rule is asserted from above instead, with two
  values, and the correction is recorded in the implementation review.
- Checks, all from `nix build` and all built locally: `compile` exit 0, `lint` exit 0, `i18n` exit 0,
  `jest` 77 suites and 1040 tests with 1037 passed and 3 skipped. `package.json` and `yarn.lock` are
  unchanged.

## Self-Review

- The Jest resolution finding is stated with the line that causes it and the three routes that were
  measured, so the workaround is a conclusion rather than a preference.
- Every `CHECK` in the schema has a failing row in the verification plan. A schema asserted only by
  its DDL is a schema nobody has tested.
- The one place this plan departs from the task graph, the spec filename, is recorded in two
  sections rather than applied quietly.
