Implementation: Iteration 1
Timestamp: 2026-09-15T03:40:00Z

Changes made:
- `source/common/types/asset-metadata.types.ts`: new. `AssetResolutionState`,
  `AssetMetadataSource`, `AssetIpcCorrelated`, `AssetMetadataEntry` and
  `AssetUnresolvedSubject`.
- `source/common/ipc/api.ts`: the three channel constants with their request and
  response types, appended after the governance block, plus one `import type`.
- `source/main/assets/assetMetadataDb.ts`: the two unions are imported from the
  common module and re-exported under the names its callers already use.

Files touched:
- `source/common/types/asset-metadata.types.ts`
- `source/common/ipc/api.ts`
- `source/main/assets/assetMetadataDb.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-013.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-013-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-013-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Verification run:

- The correlation criterion, driven rather than asserted by reading. A file
  declaring `const uncorrelated: AssetImageMainResponse = { status: 'absent' }`
  was added, staged, and compiled through
  `nix build '.#checks.x86_64-linux.compile' --no-link`, which failed with:

  ```
  source/common/types/correlation-negative-check.ts:4:14 - error TS2322: Type
  '{ status: "absent"; }' is not assignable to type 'AssetImageMainResponse'.
  Property 'requestId' is missing in type '{ status: "absent"; }' but required
  in type '{ requestId: string; }'.
  ```

  The file was then removed. The error names the union member rather than the
  alias, which is what shows the requirement reaches into the union rather than
  sitting beside it.
- The re-export keeps every existing importer working without an edit:
  `assetMetadataResolver.ts` and `assetImageStore.ts` still import
  `AssetResolutionState` and `AssetMetadataSource` from the database module, and
  `compile` passes with no change to either file.
- `grep -n "ASSET" source/common/ipc/api.ts` shows the three new constants and
  the pre-existing `LOAD_ASSET_CHANNEL` at `:152`, so no name collides. The
  collision would otherwise throw at construction
  (`source/common/ipc/lib/IpcChannel.ts:87-89`).

Checks, all four through Nix with the new file staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `hyd3rw6j2jy3dvnq4fwsqpfw5zbj8cqn-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `k9ig3arm75661mw2h6z0l9r8jqxjizzq-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 81 suites passed, 1199
  tests with 1196 passed and 3 skipped, exit 0. Identical to the state of this
  branch before the change, which is the result to want from a declaration-only
  commit.

`nix fmt` reported no change.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None. The database re-export was recorded in the plan before the work.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T03:45:00Z

Acceptance criteria, each against the evidence:

1. *Every request carries a `requestId` and every response echoes it, enforced by
   the types.* Met, and shown by a compile that fails rather than by reading the
   declaration. The wrapper reaches inside the image response union, which is the
   place a later addition would otherwise slip through.

2. *`AssetResolutionState` is declared and the response carries `unresolved`
   alongside `entries`.* Met, with one declaration rather than two. The union the
   renderer reads is the union the `CHECK` constraint enforces.

3. *`AssetMetadataEntry` carries `source`.* Met.

4. *No channel name collides.* Met, checked against the file rather than assumed.

5-8. *Compile, lint, no new suppressions, dependencies unchanged.* All met.

The push channel carries no `requestId` and that is correct rather than an
oversight: it answers no request, so an id on it would be an id for nothing. The
criterion is about requests and their responses, and the two request channels
both satisfy it.

Summary: Three channels and one entry type, with correlation expressed as a type
so the next person to add a response shape cannot leave it out, and with the
resolution state declared once for both processes. Nothing imports the constants
yet; `task-014` and `task-015` are where they are instantiated.

Decision: approved
