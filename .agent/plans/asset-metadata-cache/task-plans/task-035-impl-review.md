Implementation: Iteration 1
Timestamp: 2026-09-16T18:15:00Z

Changes made:
- `source/main/assets/assetMetadataResolver.ts`: the chain pass, the row builder
  and the two retry constants.
- `source/main/assets/assetMetadataResolver.realfs.spec.ts`: eight cases running
  the whole channel.
- `source/main/assets/immutableBlockReader.ts`: `resolveChainPath`, which
  reproduces the resolution at `source/main/index.ts:360-369`.
- `source/main/ipc/index.ts`: the chain path, resolved once and passed in.
- `source/main/ipc/assetMetadataChannel.ts`: the immutable directory option and
  the pointer source assignment.
- `source/common/ipc/api.ts` and `source/renderer/app/ipc/assetMetadataChannel.ts`:
  `sourceUrl` on the metadata request.
- `source/renderer/app/api/assets/types.ts`, `domains/Asset.ts`,
  `utils/assets.ts` and `stores/AssetsStore.ts`: `source` onto the row a surface
  renders.
- `source/renderer/app/utils/assetName.ts` and its spec: the chain rung.
- `source/renderer/app/components/assets/Asset.tsx` and its spec: the source
  passed to the resolver.
- `source/renderer/app/stores/AssetsStore.spec.ts`: four cases.

Files touched:
- the fifteen source files above
- the three review-log files for this task
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Correction to the plan's finding 7 and its approach:

**The handler cannot resolve the chain path itself.** Reading the custom chain
path means importing `electronStoreConversation`, which imports `electron-store`,
which requires the Electron **binary** at import time. That binary is not present
in the check sandbox, so the first implementation turned all 24 cases in
`assetMetadataChannel.realfs.spec.ts` red with "Electron failed to install
correctly". The resolution moved to `source/main/ipc/index.ts`, which is the
composition root, is only ever loaded inside Electron, and already imports that
module. The channel takes the directory as an option, and the option's comment
says why.

That failure only appeared in the Nix check, because the working tree has a
patched Electron binary and the sandbox does not. It is worth recording as a
property of this repository: a main-process module that a spec constructs must
not import `electron-store`, directly or transitively.

Two details decided during implementation:

**The CIP-25 name is read from both spellings.** The recorded fixture's name is
`["Northwind Demo"]`, because CIP-25 splits a string over 64 bytes into an array
and minters also write single values that way. A bare string and an array of
strings are both read; anything else is no name rather than a rendered object.

**The pending retry is one hour.** The window is about twelve hours on mainnet,
so an hour is comfortably inside it, and a retry costs two requests for the whole
batch. The case asserts both the constant and that it is under twelve hours.

Verification run:

- `yarn jest source/main/assets/assetMetadataResolver.realfs.spec.ts` — 45
  passed, 8 added. These run the channel end to end: a stubbed pointer transport,
  a temporary immutable database written from the recorded preprod block, and the
  real confirmation between them.
- The row assertion names all six columns rather than the two the schema does not
  enforce, so a change to any of them fails here rather than in a QA pass.
- The precedence case asserts the transport was never called, not only that the
  registry row survived.
- `yarn jest source/renderer/app/utils/assetName --coverage=false` — the chain
  rung driven both ways, with the same name string under both sources.
- `yarn jest source/renderer/app/components/assets/Asset.spec.tsx` — a CIP-25
  name renders where the asset name alone shows nothing, it is not marked
  minter-chosen, and the same asset shows its fingerprint before the row exists.
- `yarn jest source/renderer/app/stores/AssetsStore` — 44 passed, 4 added,
  including the two that matter for locked decision 10: a chain row formats
  nothing, and the user's own setting still applies over one.

Checks, all six through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 96 suites passed,
  1556 tests with 1553 passed and 3 skipped, exit 0. The branch stood at 96
  suites and 1537 tests, so this adds nineteen tests and no suite, and every
  existing case still passes, which is criterion 7.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.

Two earlier check runs failed and both are recorded above: the Electron import in
the jest sandbox, and a duplicate import of `electronStoreConversation` in
`source/main/ipc/index.ts` that lint caught.

`nix fmt` was run and changed files before each of the check runs.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- The chain path is resolved in `source/main/ipc/index.ts` rather than in the
  channel module, for the reason above.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-16T18:25:00Z

Acceptance criteria, each against the evidence:

1. *The row shape.* Met, all six columns asserted.

2. *A registry row is not overwritten, and the index is not asked.* Met, both
   halves.

3. *A pointer in the volatile window writes no row and records a pending retry
   inside the window.* Met, asserted as a bound rather than as an equality.

4. *A CIP-25 name renders; the fingerprint renders before the row exists.* Met.

5. *A chain row formats nothing, and a user setting still applies.* Met, driven
   through the merge helper rather than on the store.

6. *A chain name is not minter-chosen, and provenance distinguishes the two
   sources.* Met.

7. *The `task-019` decimals specs pass unchanged.* Met, and it is the whole suite
   that says so.

8-9. *All six checks, suppressions, dependencies.* Met.

Three judgements worth naming.

**Precedence by exclusion rather than by resolution.** A subject the registry
answered never reaches the index, so there is no chain row for it to lose to. The
alternative, writing both and preferring one, would have made the privacy
property depend on a comparison rather than on a request never being made.

**A chain name is treated as published rather than as decoded, and that is a
claim.** It rests on the CIP-25 record being in the transaction that minted the
asset, which had to satisfy the minting policy. That is true of CIP-25 and it is
**not** true of CIP-68, whose datum lives at a spendable output and can have been
changed by anyone who could spend it since. Both are stored in the same column
and rendered the same way, so a CIP-68 name is being presented with CIP-25's
provenance. The PRD accepts that because the value is a name, and the risk class
is the same as an unverified registry name; the row does not record which kind it
is, and a reader of the database cannot tell. That is the sharpest thing in this
task and it is worth carrying into any later work that wants to trust the column.

**The Electron import was a real defect caught by the sandbox and not by the
working tree.** It would have shipped as a main process that throws on startup
had the checks not run there. The rule it implies is written into the option's
comment rather than left in this log.

Decision: approved
