Implementation: Iteration 1
Timestamp: 2026-09-16T09:40:00Z

Changes made:
- `source/renderer/app/types/assetTypes.ts`: new. One union, with the reason it
  has no `none` member where `SmashServerType` does.
- `source/renderer/app/config/assetsConfig.ts`: the preset list, the type map
  and the validator, beside the two decimal constants already there.
- `source/renderer/app/config/assetsConfig.spec.ts`: new. Thirteen cases.
- `declaration.d.ts`: `koiosUrl` declared on `global` as `string | undefined`.
- `nix/internal/launcher-config.nix`: `koiosServers` beside `smashServers`, and
  the assignment beside `smashUrl`.
- `source/main/config.ts`: typed on `LauncherConfig` and re-exported.
- `source/main/preload.ts`: put on `global`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`.

Files touched:
- the seven source files above
- the three review-log files for this task
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Two details decided during implementation:

`ASSET_METADATA_SERVERS_LIST` is `Partial<Record<AssetMetadataSourceType, ...>>`
rather than the PRD's `Record<AssetMetadataSourceType, ...>`. The PRD's form does
not type-check: the list holds two of the three members, which is why the SMASH
original carries a suppression. `Partial` states the same fact without one and
keeps the keys typed, which a `Record<string, ...>` would not.

The comment above `koiosServers` in the nix file says why `mainnet_flight` is
there and selfnode is not. It is the one place a reader will ask, and the answer
is not derivable from the surrounding lines.

Verification run:

- `yarn jest source/renderer/app/config/assetsConfig.spec.ts --coverage=false`
  — 13 passed.
- The validator cases are one input class each. Accepting is driven five ways
  (the default with its path, a port, a port and a path, a trailing slash, and
  `direct`) and rejecting four (`http://`, a query string, a host carrying `%`,
  and the empty string). A pattern that accepted everything fails four cases and
  one that accepted nothing fails five, so neither degenerate form passes.
- The list's shape is asserted as a key set rather than by looking up `koios`,
  because the property that matters for `task-031` is that `custom` is **not**
  there: a reduce over this list falls back to `CUSTOM`, and a `custom` entry
  would match on its own placeholder URL.
- `koiosUrl` is `undefined` under jsdom, so no case asserts its value. What the
  spec can assert is the shape around it, and the value is checked by the
  launcher configuration rather than by a renderer spec.

Checks, all six through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `vl5340vvrddkqkjvmp8ngw58mgcc1cjg-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `s4hr01dk4gnzb382m6xn4zif3b7hlpsc-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — built
  `a9rxd5qa8jghkh6v4z2lf93hnl99csfd-daedalus-stylelint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `ypl42i0wrq4g0ch0j21liacbar1z051f-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 90 suites passed,
  1402 tests with 1399 passed and 3 skipped, exit 0. The branch stood at 89
  suites and 1389 tests, so this adds one suite and thirteen tests and moves
  nothing else.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.

`nix fmt` was run and reported 0 files changed, which is criterion 4.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-16T09:45:00Z

Acceptance criteria, each against the evidence:

1. *The validator accepts and rejects what it should.* Met, nine cases.

2. *The list holds two presets and the map holds three.* Met, asserted as key
   sets.

3. *`koiosUrl` present for four networks and absent for selfnode.* Met as
   restated, not as originally worded. The launcher assigns it under
   `__hasAttr network koiosServers`, exactly as it assigns `smashUrl`, and the
   attrset names `mainnet`, `mainnet_flight`, `preprod` and `preview`. The
   evidence is the expression and `nix fmt` parsing it, not a per-network
   evaluation; the plan says so rather than implying a stronger check.

4. *`nix fmt` leaves the nix file unchanged.* Met, 0 files changed.

5-6. *All six checks, suppressions, dependencies.* Met.

Two judgements worth naming.

**`mainnet_flight` diverges from the precedent by one line, deliberately.** The
SMASH block omits it, so a Flight user has no SMASH default today. Reproducing
that here would leave the chain metadata channel dead on Flight for no reason
other than symmetry with a gap. The divergence is one attribute and a comment
saying why, and the SMASH gap is recorded as a finding rather than fixed inside
an asset task.

**Declaring the global rather than suppressing the read is the second
divergence.** It is the more consequential one: `stakingConfig.ts:8` carries an
`@ts-ignore` whose entire cause is a missing line in `declaration.d.ts`, and a
structural copy that copied it would have added the branch's first new
suppression in six phases for no benefit.

What this task does not establish: that any of these URLs answers. A preset
whose hostname a third party changes is a channel that silently returns nothing,
and nothing here would notice. The probe in `task-031` turns that into a visible
refusal for a URL the user types, and for the default it would take a release.

Decision: approved

Correction to Iteration 1
Timestamp: 2026-09-16T09:50:00Z

The iteration above records "Deviations from the approved plan: None" and that is
wrong by one. The plan's finding 10 says the spec sets `global.koiosUrl` and loads
the module through `jest.isolateModules`, and the spec as written does neither: no
case needs the value, so the module is imported normally and `koios.url` is
`undefined` throughout. The finding stands as a fact about the environment and is
what `task-031`'s spec will need, because `getAssetMetadataSourceIdFromUrl` maps
the default URL back to its preset and cannot do that against `undefined`.
