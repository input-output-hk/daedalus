Implementation: Iteration 1
Timestamp: 2026-09-16T11:20:00Z

Changes made:
- `source/renderer/app/api/assets/requests/checkAssetMetadataSourceHealth.ts`:
  new. Its own `https` GET against `{url}/tip`, capped, timed, and parsed rather
  than trusted.
- `source/renderer/app/api/assets/requests/checkAssetMetadataSourceHealth.spec.ts`:
  new. Fourteen cases against a fake `global.https`.
- `source/renderer/app/api/assets/types.ts`: the tip and the three-valued check.
- `source/renderer/app/api/api.ts`: `checkAssetMetadataSourceIsValid`.
- `source/renderer/app/api/errors.ts` and `source/renderer/app/domains/ApiError.ts`:
  two codes and two messages.
- `source/renderer/app/config/assetsConfig.ts`: the probe timeout and the lag
  bound, each with the argument it was chosen from.
- `source/renderer/app/utils/assets.ts` and its spec: the preset mapping and the
  freshness rule.
- `source/renderer/app/api/utils/localStorage.ts`,
  `source/common/config/electron-store.config.ts` and
  `source/common/types/electron-store.types.ts`: one key and the trio.
- `source/renderer/app/actions/assets-actions.ts`: two actions.
- `source/renderer/app/stores/AssetsStore.ts` and its spec: the observables, the
  startup read, the selection and the error reset.
- `jest.config.js`: `koiosUrl` on the test globals.
- the four translation artifacts, regenerated, with the `!!!` marker stripped
  from the two new keys in `en-US.json` by hand and left in `ja-JP.json`.

Files touched:
- the sixteen source files above
- the three review-log files for this task
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Three details decided during implementation:

**The freshness rule moved out of `api.ts`.** The plan put the lag comparison
inside `checkAssetMetadataSourceIsValid`, and the boundary is exactly what
criterion 4 asks to be driven. There is no `api.spec.ts` in this repository and
`api.ts` imports most of the renderer, so a spec for it would be a new suite
carrying the whole graph to assert one subtraction. `assetMetadataSourceTipIsFresh`
is in `utils/assets.ts` beside the other asset helpers, `api.ts` calls it, and
the four boundary cases are driven directly.

**`koiosUrl` went into `jest.config.js` rather than into `jest.isolateModules`.**
The plan's approach was to set the global inside each spec that needs it and
re-require the config module. `jest.config.js` already supplies `environment` to
every spec for the same reason, the preset URL is per network and the mainnet one
is the right stand-in, and one line there removes the need for module gymnastics
in two files. This is a deviation from the approved plan and from `task-030`'s
finding 10.

**The error observable is typed `LocalizableError | ApiError`.** `ApiError` is
not a subclass of `LocalizableError`; it carries the same three fields
`intl.formatMessage` reads and nothing more. `StakingStore` types its equivalent
field as `LocalizableError` and stores an `ApiError` in it, which type-checks
only because the value arrives from an untyped `catch`. Widening the type is what
that field actually holds, and it needed no suppression and no cast.

Verification run:

- `yarn jest source/renderer/app/api/assets/requests --coverage=false` — 14
  passed.
- `yarn jest source/renderer/app/utils/assets.spec.ts --coverage=false` — 32
  passed, 5 mapping cases and 5 freshness cases added.
- `yarn jest source/renderer/app/stores/AssetsStore.spec.ts --coverage=false` —
  40 passed, 9 added.
- Criterion 2, that `direct` issues no request, is asserted in `api.ts` by the
  short circuit and in the store by the call count on the fake: selecting a URL
  that is already selected leaves `checkAssetMetadataSourceIsValid` uncalled.
- Criterion 3 is driven six ways rather than one, because "does not answer as an
  instance" has six shapes: an object instead of an array, an element with no
  `abs_slot`, an `abs_slot` that is a string, an HTML page, a `503`, and a socket
  error. A probe that accepted a bare `200` passes none of the first four.
- Criterion 4 is driven at the bound and one slot beyond it, plus far ahead and
  with the local tip unknown.
- Criteria 5 and 6 are driven as two stores: one writes, a second reads what the
  first wrote and comes up on it; and a store with nothing stored comes up on the
  preset.
- The refusal case asserts three things together: the selection did not move, the
  error carries the right id, and nothing was written to storage. A refusal that
  stored anyway would pass the first two.

Checks, all six through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 91 suites passed,
  1435 tests with 1432 passed and 3 skipped, exit 0. The branch stood at 90
  suites and 1402 tests, so this adds one suite and thirty-three tests.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.

An earlier lint run failed on three findings, all introduced here and all fixed
before the run above: `utils/assets` imported twice in `AssetsStore.ts`, a nested
ternary in the spec's store builder, and an unused `catch` binding.

`nix fmt` was run and changed three files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`. The spec's fake `https` uses
`as unknown as typeof global.https`, which is the pattern
`AssetsStore.spec.ts:55` and `ProfileStore.spec.ts:28` already use for the same
problem.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- The freshness rule lives in `utils/assets.ts` rather than in `api.ts`.
- `koiosUrl` is supplied by `jest.config.js` rather than per spec.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-16T11:30:00Z

Acceptance criteria, each against the evidence:

1. *The preset mapping.* Met, five cases, including the one the plan did not ask
   for: the default URL with a trailing slash maps to `custom`, because the
   reduction compares strings. That is the honest answer and the case records it
   rather than leaving it to be discovered.

2. *`direct` accepted with no request.* Met, asserted on a call count.

3. *A URL that does not answer, and a `200` that is not a tip.* Met, six shapes.

4. *The staleness bound.* Met at the bound, one slot past it, far ahead, and with
   an unknown local tip.

5-6. *Survives a restart; a fresh profile comes up on the preset.* Met, driven as
   two stores rather than argued from a storage call.

7-8. *All six checks, suppressions, dependencies.* Met.

Three judgements worth naming.

**The probe does not reuse the repository's HTTP client, and that is the most
consequential decision here.** `api/utils/request.ts` would have sent a
user-named URL over plain HTTP on selfnode and presented cardano-wallet's client
certificate to a third party. Neither is visible from the call site, and both
would have shipped had the six lines been copied as the task graph describes.

**The staleness bound is argued, not measured.** Twelve hours is the order of the
volatile window the chain confirmation cannot see into, so an index further
behind cannot answer for anything the local check could confirm. Nothing here
says real instances drift that far or further, and the plan says so.

**A stored URL is never re-probed.** An instance that was healthy at selection
and is dead now comes up selected and answers nothing, and no surface
distinguishes that from an asset the index has never heard of. Re-probing on
every start would put a third-party request on every launch for a channel whose
failure mode is already an absent row. Recorded as a risk rather than fixed.

What this task does not establish: that anything reads the selection. Nothing
does yet. `task-033` is where the pointer client takes this URL, and until then
the setting is stored, validated and inert.

Decision: approved
