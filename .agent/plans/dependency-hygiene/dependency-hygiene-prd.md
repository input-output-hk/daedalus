# Dependency Hygiene PRD

## Overview

One branch of dependency work with two halves that belong together.

The first half makes a single prettier version govern the repository. The
version named in `package.json` and the version `nix fmt` runs are different
majors, and they disagree about 210 files on a clean checkout of `master`. The
second half takes the runtime dependency bumps that are safe to take now,
clears two advisories that a `resolutions` block is currently holding shut, and
adds the known-answer tests the crypto path has never had.

No formatting option changes. No framework migrations. Nothing that needs
physical hardware or a human eye to verify.

## Problem Statement

### The formatter runs two different prettiers

| Entry point | Version | Source |
|---|---|---|
| `yarn prettier:check` (via `yarn check:all`) | 2.1.2 (`package.json:163`) | the repository pin |
| `nix fmt`, `checks.treefmt` | 3.6.2 | `nixpkgs/nixos-25.11` (`flake.nix:5`) |

Measured at `50e6b84b3`, `yarn prettier:check` reports 210 files with style
issues and `nix fmt -- --ci` reports 0 changes on the same tree. The direction
is the opposite of the obvious reading: the merge gate is satisfied and the
documented local pre-flight is what fails. `checks.treefmt` is not defined in
`perSystem/checks.nix`; it arrives from `inputs.treefmt-nix.flakeModule`, and
every `x86_64-linux` check is a constituent of the `required` aggregate in
`flake.nix`.

The failure is not inert, because the obvious response to it is destructive. A
contributor who runs `yarn check:all`, sees 210 files reported, and runs
`yarn prettier:format` rewrites those files into prettier 2 shape.
`checks.treefmt` then reverts 209 of them and fails the build. The remaining
one, `source/renderer/app/api/news/requests/getNews.ts`, is stable under
prettier 3 in both shapes, so the prettier 2 rewrite of it survives the
authoritative formatter and lands in the tree.

Three conditions let the same divergence recur, and one loose end sits beside
them:

1. **`.prettierrc` is not the config `nix fmt` uses.** `perSystem/formatter.nix`
   sets `programs.prettier.settings`, and treefmt-nix generates its own config
   from those settings and passes it as `--config`, which stops prettier
   discovering `.prettierrc`. The values match today by hand, not by
   construction.
2. **Nothing asserts the two versions agree.** The treefmt prettier tracks
   nixpkgs. A nixpkgs bump moves it and the gap reopens with no signal.
3. **The prettier `includes` list is inert and misleading.** It is written to
   `settings.formatter.prettier.includes`, which is not the option controlling
   the module's include list; that is `programs.prettier.includes`. Both become
   definitions of the same `listOf str`, so they concatenate rather than the
   repository's list winning. The effective set covers `*.md`, `*.yaml`,
   `*.html` and more, tree-wide. None of it is formatted, only because prettier
   applies `.prettierignore` even to paths passed explicitly.
4. Prettier 3 removed `--loglevel` in favour of `--log-level`. Three scripts and
   one skill document still pass the old spelling, which prettier 3 ignores with
   a warning.

Full evidence, with commands and outputs, is in
[`research/01-measured-divergence.md`](./research/01-measured-divergence.md).

### A `resolutions` block is holding two advisories shut

`package.json` pins `pbkdf2` and `lodash` in both `dependencies` and
`resolutions`. A `resolutions` entry overrides the entire tree, so bumping only
the `dependencies` line leaves the old version installed and the advisory
unresolved, while the diff looks complete.

`lodash` needs `>=4.18.0` to clear its advisory and is pinned at 4.17.21 in both
places. `pbkdf2` has a fix available at 3.1.6 and is pinned at 3.1.2 in both
places.

### The crypto path has no known-answer test

`source/renderer/app/utils/crypto.ts` imports `bip39`, `blakejs`, `pbkdf2` and
`lodash` together and derives mnemonics and seeds. It has no colocated Jest
spec. The only automated coverage is Cucumber `@unit`, through
`tests/wallets/unit/steps/mnemonics.ts`, which generates a mnemonic and then
validates what it generated.

That shape cannot see a consistent change in derivation. A bump that altered
the output for every input would produce a different seed, validate it happily,
and leave every check green. `bip39` 3.1.0 replaced its internal hashing
implementation, which is precisely that kind of change.

### 58 runtime dependencies are behind, and the reasons differ

Of 93 declared runtime dependencies, 58 have a non-major upgrade available.
They are not one population. Some are utility packages the check set fully
covers; some touch key material or user balances; some cannot be verified
without physical hardware. A single sweep would either move nothing risky or
move everything at once. The inventory in
[`research/02-dependency-inventory.md`](./research/02-dependency-inventory.md)
sorts them, and this branch takes two of the tiers.

## Goals

- One prettier version governs the repository, and it is the one the required
  check already enforces.
- `yarn check:all` and the flake check set agree on a clean checkout.
- Formatting options have exactly one source of truth, and a future version
  drift fails a check that names the cause.
- The `lodash` and `pbkdf2` advisories are cleared in both places they are
  pinned, with the advisory count measured before and after.
- Mnemonic and seed derivation has a fixed input with a known expected output,
  so a future dependency change cannot silently alter it.
- The mechanical and consequential runtime bumps are taken, and everything not
  taken has a written reason.

## Non-Goals

- Changing any formatting option. `arrowParens`, print width, quote style and
  trailing commas stay as they are. The open `arrowParens` question is
  deliberately excluded so a style decision and a version artefact never appear
  in the same diff.
- Reformatting the tree. The prettier change is expected to produce zero file
  changes outside `package.json` and `yarn.lock`.
- Widening prettier's scope to Markdown, YAML, or top-level JSON.
- The UI tier. 16 packages where a minor bump can shift rendering with every
  check still green. They need a Storybook pass per component.
- The hardware wallet tier. `@trezor/connect`, `@trezor/transport` and
  `@ledgerhq/hw-transport-node-hid` cannot be verified in CI, because
  `hardware-wallet-tests/index.ts` is an interactive driver needing physical
  devices. Including them would mean the branch could not merge on a green
  check set.
- Native modules. `usb` and `node-hid` are rebuilt by
  `scripts/rebuild-native-modules.sh` and bundled through per-platform N-API
  paths in `nix/internal/any-darwin.nix`. Bumping them is a packaging change.
- Pre-1.0 packages. `@cardano-sdk/core` and `pdfkit` take breaking changes in
  the minor position and are treated as majors regardless of semver.
- `electron`, which moved from 24.2.0 to 41.3.0 only at 11.2.0.
- The `elliptic` pin. Raising it needs three questions answered about the Trezor
  path first, and it is not a bump.
- **Removing** any dependency. The unused-looking set is investigated on this
  branch and acted on separately.
- Framework migrations. `react-intl`, `mobx`, `react`, `storybook`, `jest` and
  `cucumber` each need their own scope.

## Inputs And Source Material

- `.agent/plans/readme.md`
- `.agent/workflows/build.md`, `.agent/workflows/test.md`, `.agent/workflows/nix.md`
- `.agent/skills/git-commit-formatter/SKILL.md`
- `research/01-measured-divergence.md`, `research/02-dependency-inventory.md`
- `package.json`, `yarn.lock`
- `.prettierrc`, `.prettierignore`, `.editorconfig`
- `perSystem/formatter.nix`, `perSystem/checks.nix`, `flake.nix`
- `nix/internal/common.nix`, `nix/internal/any-darwin.nix`
- `source/renderer/app/utils/crypto.ts`
- `source/renderer/webpack.config.js`
- `tests/wallets/unit/steps/mnemonics.ts`
- `treefmt-nix/programs/prettier.nix` from the pinned flake input

## Locked Planning Decisions

- The work lands on a fresh branch cut from `master`, named
  `chore/dependency-hygiene`. There is no `develop` branch.
- Prettier moves to `3.6.2`, matching what treefmt runs. The tree is not
  reformatted to meet it; it already does.
- If the prettier bump produces any file change beyond `package.json` and
  `yarn.lock`, the premise is wrong and the work stops for a re-measure rather
  than committing a reformat.
- `.prettierrc` becomes the single source of formatting options, and
  `.prettierignore` the single scope gate. The duplicated blocks in
  `perSystem/formatter.nix` are removed rather than kept in sync.
- Version parity is asserted by a check, not by a comment.
- The crypto known-answer tests land **before** the tier 2 bumps, so they
  witness the change rather than being written to match whatever it produced.
- `pbkdf2` and `lodash` move in `dependencies` and `resolutions` in the same
  commit. Neither line moves alone.
- Bumps are grouped by tier, one commit per group, so a bisect lands on a
  category rather than on a 20 package diff.
- The unused-dependency question is investigated and written down on this
  branch. No dependency is removed here.

## Requirements

### Functional Requirements

Formatter parity:

- [ ] `package.json` names `prettier` at `3.6.2`, and `yarn.lock` resolves to it
- [ ] The prettier commit touches only `package.json` and `yarn.lock`
- [ ] `yarn prettier:check` and `nix fmt -- --ci` both exit 0 on a clean checkout
- [ ] `yarn prettier:format` produces an empty diff
- [ ] No script or document passes `--loglevel`
- [ ] `perSystem/formatter.nix` sets no `programs.prettier.settings`, and the
      generated treefmt config passes no `--config` for prettier
- [ ] A check fails, naming both versions, when the prettier in `package.json`
      and the prettier treefmt runs differ

Test coverage:

- [ ] `source/renderer/app/utils/crypto.ts` has a colocated spec asserting
      `mnemonicToSeedHex` against fixed mnemonic and password inputs with known
      expected output
- [ ] That spec is committed and green **before** any tier 2 bump

Dependencies:

- [ ] The 13 tier 1 packages are at the versions in the inventory
- [ ] The 8 tier 2 packages are at the versions in the inventory
- [ ] `pbkdf2` and `lodash` are updated in `resolutions` as well as
      `dependencies`, and `yarn why` confirms the installed version matches
- [ ] `yarn audit` distinct-advisory counts are recorded before and after
- [ ] Every deferred package has a written reason in the inventory
- [ ] The unused-looking dependencies are each classified, and the finding is
      recorded under `.agent/findings/`
- [ ] No dependency is removed on this branch

Acceptance:

- [ ] The full check set is green on `x86_64-linux`
- [ ] A production build and a packaged installer are produced, and the
      installed application starts and syncs

### Non-Functional Requirements

- **No user-visible behaviour change.** Nothing about the wallet's function,
  appearance, or output should differ. Anything that does is a defect in this
  branch, not an accepted consequence.
- **Reviewability.** Commits are grouped by category, so a reviewer reads one
  kind of risk at a time and a bisect lands on a category.
- **Reversibility.** Each commit reverts independently.
- **Speed.** `nix fmt` runs constantly and must stay fast. The version parity
  check must not make the formatter depend on building `node_modules`.
- **Evidence over prediction.** Advisory counts are measured, not asserted. A
  claim that a bump changes nothing is backed by a check that would have failed
  if it did.

## Technical Design

### Components Affected

- `package.json`: prettier version, `--loglevel` spelling in three scripts, 21
  dependency versions, 2 `resolutions` entries
- `yarn.lock`: regenerated
- `perSystem/formatter.nix`: drop `programs.prettier.settings`, drop the inert
  prettier `includes` and `excludes`
- `perSystem/checks.nix`: add the prettier version parity check
- `source/renderer/app/utils/crypto.spec.ts`: new
- `.agent/skills/theme-management/SKILL.md`: `--loglevel` references
- `.agent/findings/`: new finding for the unused-dependency question
- `.agent/plans/readme.md`: plan index entry

No changes to renderer components, containers, stores, themes, i18n, IPC
contracts, or the installers.

### Data / IPC / API Changes

None. No IPC channel, cardano-wallet integration, or process boundary changes.

### UI / Store / Process Changes

None intended. `bignumber.js` and `classnames` reach rendering code, so the
storybook build and the Jest suite are what confirm nothing moved.

### Version parity check

The check compares the version in `package.json` against the prettier treefmt
invokes, reading the binary out of the treefmt settings rather than naming a
package a second time, so it cannot itself drift from what the formatter runs.
`config.treefmt.settings.formatter.prettier.command` is the literal path treefmt
executes; `perSystem` must take `config` in its argument set, which
`perSystem/checks.nix` does not do today.

```nix
prettier-version-parity =
  pkgs.runCommand "daedalus-prettier-version-parity" {
    nativeBuildInputs = [pkgs.jq];
  } ''
    pinned=$(jq -r '.devDependencies.prettier' ${inputs.self}/package.json)
    actual=$(${config.treefmt.settings.formatter.prettier.command} --version)
    if [ "$pinned" != "$actual" ]; then
      echo "ERROR: prettier version mismatch."
      echo "  package.json devDependencies.prettier: $pinned"
      echo "  prettier run by nix fmt:               $actual"
      echo
      echo "These format the same files and must be the same version."
      echo "Update package.json and yarn.lock to $actual, or pin the"
      echo "formatter's prettier to $pinned."
      exit 1
    fi
    touch $out
  '';
```

It belongs in the `x86_64-linux` block of `perSystem/checks.nix`, alongside the
other static analysis, since the answer is identical on every system.

The exact-match comparison assumes the pin stays an exact version rather than a
range, which holds for every `devDependencies` entry today.

An alternative was considered and rejected: pointing treefmt at the prettier in
`node_modules` would make the two identical by construction rather than by
assertion, but it would make `nix fmt` depend on building `node_modules`, which
is too slow for a formatter run on every save.

### Why removing the prettier `includes` block is safe

Removing entries from `settings.formatter.prettier.includes` cannot widen
prettier's scope, because the module defaults are a separate definition of the
same option and remain in place regardless, and every repository entry is
already a subset of them. Scope is decided by `.prettierignore`, which prettier
applies to explicitly passed paths. This is documented prettier behaviour and
the same mechanism that makes `yarn prettier "**/*.*"` correct.

The residual risk is that a future prettier could stop applying `.prettierignore`
to explicit paths, at which point treefmt would begin formatting every Markdown
file in the tree, `CHANGELOG.md` included. That would be a loud diff on the
first `nix fmt` after such a bump rather than a silent corruption, and the
parity check makes any prettier version move deliberate. Naming it once is the
proportionate response; a duplicated exclude list is what this change removes.

### The `resolutions` interaction

`resolutions` overrides the whole dependency tree, so it wins over
`dependencies`. Two entries currently hold packages below their advisory fix:

```
"**/**/lodash": "4.17.21",
"pbkdf2": "3.1.2"
```

Both lines move with their `dependencies` counterparts in the same commit.
`yarn why lodash` and `yarn why pbkdf2` after the change are what confirm the
installed version actually moved, because a stale `resolutions` entry produces
a diff that looks correct and installs the old version.

### Crypto known-answer tests

The new spec asserts `mnemonicToSeedHex` against a fixed mnemonic and password
with a known expected hex output, plus `generateMnemonic` and the scramble
round trip against fixed input. The expected values are captured from the
current implementation **before** any tier 2 bump, so the test witnesses the
change rather than being written to match its result.

If a tier 2 bump makes the spec fail, that is the test doing its job. The bump
stops and the change is investigated; the expected value is not updated to
match.

The spec is colocated as `source/renderer/app/utils/crypto.spec.ts`, per the
repository convention that Jest specs sit next to the unit and `tests/` is
Cucumber territory.

## Implementation Strategy

One branch, eight commits, each independently revertible.

1. **`chore(deps): move prettier to 3.6.2`.** Bump, regenerate the lockfile,
   rename `--loglevel` to `--log-level` in the three scripts. Verify the diff is
   only `package.json` and `yarn.lock`, then that `yarn prettier:check` and
   `nix fmt -- --ci` are both clean.
2. **`chore(nix): read .prettierrc instead of generating a second config`.**
   Remove `programs.prettier.settings`. Confirm no `--config` in the regenerated
   treefmt config and that `nix fmt -- --ci` is still clean.
3. **`chore(nix): drop the inert prettier include and exclude lists`.** A pure
   deletion, keeping `settings.global.excludes`, which alejandra and rustfmt
   depend on.
4. **`ci(nix): fail when prettier versions diverge`.** Add the parity check.
   Verify it passes, then verify it fails by temporarily editing the pin.
5. **`test(crypto): add known-answer vectors for seed derivation`.** The new
   spec, capturing current behaviour. Must be green before step 7.
6. **`chore(deps): take the mechanical runtime bumps`.** The 13 tier 1
   packages, one commit.
7. **`chore(deps): clear the lodash and pbkdf2 advisories`.** The 8 tier 2
   packages, including both `resolutions` edits. `yarn audit` counts recorded in
   the commit body.
8. **`docs(agent): record the unused runtime dependency question`.** The
   investigation result as a finding. No dependency removed.

Steps 2 and 3 both edit `perSystem/formatter.nix`; keeping them separate makes
the second reviewable as a pure deletion. Step 5 precedes step 7 by design.

## Testing Strategy

**Formatter steps (1 to 4):**

```bash
git diff --name-only                 # step 1: package.json and yarn.lock only
yarn prettier:check                  # exit 0
nix fmt -- --ci                      # "0 changed", exit 0
yarn prettier:format && git diff --name-only   # empty
nix build -L .#checks.x86_64-linux.prettier-version-parity
# then with the pin set to 3.6.1, the same build must fail
```

**Crypto spec (step 5):**

```bash
yarn jest source/renderer/app/utils/crypto.spec.ts --coverage=false
yarn test:unit                       # the existing Cucumber @unit features
```

**Dependency steps (6 and 7):**

```bash
yarn audit                           # before, record distinct advisory count
yarn why lodash && yarn why pbkdf2   # after, confirm the installed version moved
yarn audit                           # after, record the count
yarn compile && yarn lint && yarn test:jest && yarn test:unit
yarn storybook:build
```

**Whole branch:**

```bash
nix flake check -L                   # every check; x86_64-linux gates the merge
yarn check:all                       # the documented local pre-flight
yarn build                           # production build
yarn package                         # installer, then start it
```

The packaged build matters here in a way it does not for a formatting change.
Webpack decides what actually ships, so a dependency that resolves and compiles
can still fail to bundle. The check set does not build an installer.

Platform note: darwin checks are built and reported but do not gate. The Windows
workflow is reporting-only. Neither blocks a merge, but a red Windows Jest run
after a dependency bump is worth reading rather than dismissing.

## Rollout / Migration / Rollback

No feature flag, no migration, nothing user visible. Contributors need no new
setup; the next `nix develop` picks up the new `node_modules`.

Rollback is `git revert` of any single commit or the branch. Reverting step 1
alone restores the current formatter divergence, which is a known state rather
than a broken one. Reverting step 7 alone restores the advisories.

The one contributor-visible change is that `yarn prettier:format` stops
producing a 210 file diff. Anyone holding an in-flight branch containing
prettier 2 output will see those hunks disappear when they rebase and run the
formatter. That is the intended outcome.

## Open Questions

1. **Should `check:all` run the Nix formatter instead of `yarn prettier:check`?**
   Once the versions agree, `yarn prettier:check` is a strict subset of
   `checks.treefmt`, which also covers `.nix` and `.rs`. A contributor can pass
   `yarn check:all` today and still fail the merge gate on a misformatted
   `.nix` file. Replacing the step with `nix fmt -- --ci` closes that gap at the
   cost of making the local pre-flight require Nix. Not decided here.
2. **What happens to the unused-looking dependencies once classified?** Removal
   is the obvious answer for anything genuinely stranded, but it needs its own
   branch and a packaged build to verify, because a wrongly removed dependency
   breaks packaging in a way the check set does not catch.
3. **Who owns the nixpkgs bump that moves prettier next?** The parity check
   turns that bump into a failing build. The fix is one line plus a lockfile
   regeneration, but it must land in the same change as the nixpkgs bump.
4. **Does the `elliptic` pin block anything here?** It is excluded from this
   branch, but it is the only item in the wider audit with a plausible path to
   user funds, and it stays unquantified until the three Trezor questions are
   answered. Sequencing that work is a separate decision.

---

**Status:** Draft
**Date:** 2026-08-26
**Author:** Se7en Labs
