Implementation: Iteration 1
Timestamp: 2026-09-15T04:35:00Z

Changes made:
- `source/main/ipc/assetMetadataChannel.ts`: new. The three channels, the
  row-to-entry mapping with its JSON parse, subject cleaning, the two handlers,
  the push, and an idempotent registration taking the window.
- `source/main/assets/assetMetadataDb.ts`: `readImageSubjects`, answered from the
  primary-key index.
- `source/main/ipc/index.ts`: one line beside `handleGovernanceAnchorRequests()`.
- `source/main/ipc/assetMetadataChannel.realfs.spec.ts`: new. Sixteen cases
  across four groups.
- `source/main/assets/assetMetadataDb.realfs.spec.ts`: two cases for the new
  accessor.

Files touched:
- `source/main/ipc/assetMetadataChannel.ts`
- `source/main/ipc/assetMetadataChannel.realfs.spec.ts`
- `source/main/ipc/index.ts`
- `source/main/assets/assetMetadataDb.ts`
- `source/main/assets/assetMetadataDb.realfs.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-014.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-014-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-014-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Two details decided during implementation:

The handlers are a class rather than a set of closures. Three pieces of state
have to be shared between the two handlers and the push, and the spec needs to
reach the resolver to drive a resolve; a class makes both plain. The class takes
its collaborators as options, so the spec builds one against a temporary
database and a stubbed transport without registering anything.

`handleAssetMetadataRequests` returns the handler set on the first call and null
afterwards. A boolean would have been enough for the guard, but returning the
instance is what lets the idempotence case assert the second call did nothing
rather than infer it.

Verification run:

- `jest source/main/ipc/assetMetadataChannel source/main/assets/assetMetadataDb
  --coverage=false` — 50 passed, of which 18 are new.
- The property the whole design turns on is driven rather than argued. The
  "answers from the cache without waiting for the transport" case hands the
  resolver a transport whose `post` returns a promise that never settles. The
  handler still resolves, with the cached row for the subject that has one and
  the other subject under `unresolved`. A handler that awaited the fetch would
  not answer at all, so the case fails by hanging.
- Cold cache: no entries, both subjects `pending`, no throw.
- The echo: two requests with different ids each get their own back.
- The three absences are three states: a subject with a `failed` resolution row,
  one with `unregistered`, and one with no row at all.
- A subject with a metadata row never appears under `unresolved`, asserted
  alongside a `resolved` resolution row so the two tables cannot be confused.
- The mapping is asserted with `toEqual` against a complete entry, so a dropped
  or added field fails the case: `verified`, `source`, `decimals` and the parsed
  `metadata` object all survive. A metadata column holding text that is not JSON
  maps to null.
- `hasImage` is true for the subject with an image row and false for the one
  without, in the same response.
- A subject named three times yields one entry.
- A closed database answers with the request's own id and empty lists rather
  than rejecting.
- The push: a resolve driven through a stubbed registry produces one `send` on
  the update channel carrying the changed row with `hasImage` filled, and no
  `send` at all when the window reports itself destroyed.
- The image handler answers `present` with bytes that compare equal to the
  stored PNG and its media type, `absent` for a subject the registry answers
  without a logo, and `absent` rather than a rejection when the transport fails.
- Registration twice registers one handler on each request channel.

Checks, all four through Nix with every new file staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `fdi8dyc3qxv7h52ksfcblvzxlnhdmxmy-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `z2xx0lr65bygifnm59rlqba49wqpficp-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 82 suites passed, 1217
  tests with 1214 passed and 3 skipped, exit 0. The previous state of this branch
  was 81 suites and 1199 tests, so one suite and eighteen tests were added and
  nothing else moved.

`nix fmt` was run and changed two files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None in scope. `readImageSubjects` and the spec that covers it were recorded in
  the plan before the work.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T04:42:00Z

Acceptance criteria, each against the evidence:

1. *`compile` and `lint` pass.* Met.

2. *A request with an empty cache returns an empty entry list rather than an
   error.* Met, and it says more than the criterion asked: every requested
   subject comes back named and `pending`, so a renderer can tell the cache
   heard the question.

3. *The response echoes the `requestId`.* Met, with two requests rather than
   one, because a handler that returned a constant would pass a single-request
   case.

4. *A cached read makes no call the caller waits on.* Met by the
   never-settling transport. This is the criterion most likely to be lost to a
   later tidy-up, and it now fails loudly.

5. *`hasImage` is true only for a subject with an image row.* Met, and the
   accessor behind it reads no blob: `subject` is the primary key of
   `asset_image` and the select names only that column.

6. *A resolved row reaches the renderer without a second request.* Met, driven
   through the resolver's own callback rather than by calling `push` directly,
   so the wiring is what is under test.

7. *The image handler's three answers.* Met.

8. *Registering twice registers one set of handlers.* Met.

9-10. *Jest, suppressions, dependencies.* All met.

The window is held rather than looked up, which is worth naming. A module-level
`BrowserWindow.getAllWindows()[0]` would have been shorter and would have made
this file untestable without Electron, and it would have pushed to whichever
window happened to be first.

Summary: The cache has a caller. A read answers from disk and schedules what is
missing, absence comes back with its reason attached, rows arrive on their own
as they resolve, and the one call that can wait is the one that carries a
picture. The handler cannot reject, which is what the renderer's correlation
scheme depends on: every request gets exactly one attributable response.

Decision: approved
