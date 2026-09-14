# Dependency Hygiene PRD

## Overview

One branch of dependency work with three parts that belong together, because
each one is a case of the same thing: a version or a default that governs
Daedalus without anything in the repository asserting it.

The first part makes a single prettier version govern the repository. The
version named in `package.json` and the version `nix fmt` runs are different
majors, and they disagree about 210 files on a clean checkout of `master`.

The second part makes wallet entropy and seed derivation provable. Recovery
phrases are generated in the renderer from an entropy source this repository
never names, and the `bip39` bump in the third part replaces that source. So the
conformance assertions are established first, against the published BIP39
vectors, and the entropy source becomes a first-party module before anything
underneath it moves.

The third part takes the runtime dependency bumps that are safe to take now and
clears two advisories a `resolutions` block is currently holding shut, one of
which is a cryptographic correctness bug in `pbkdf2`.

No formatting option changes. No framework migrations. Nothing that needs
physical hardware or a human eye to verify. The one source change outside the
test tree is the entropy argument at `crypto.ts:54`, and the reasoning for
taking it is in Locked Planning Decisions.

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

### Wallet entropy is a transitive default, and this branch changes it

`AdaApi.getWalletRecoveryPhrase` (`source/renderer/app/api/api.ts:1533`)
generates the 24-word recovery phrase in the renderer. That phrase is the seed
for every wallet Daedalus creates. `crypto.ts:54` produces it with
`bip39.generateMnemonic(ent, null, validWords)`, and the `null` in the second
position is the entropy source.

The chain behind that `null` is four hops deep, and Daedalus asserts none of it:

```
crypto.ts:54   bip39.generateMnemonic(256, null, validWords)
bip39 3.0.4    rng = rng || randomBytes            require('randombytes')
randombytes    browser.js, because webpack is target: 'web'
               global.crypto.getRandomValues       Chromium CSPRNG
               or module.exports = oldBrowser, which throws
```

Current behaviour is sound. `randombytes/browser.js` fails closed: absent
`crypto.getRandomValues` it exports a function that throws, and no weaker
fallback exists anywhere on the path. The problem is that this is inherited
rather than asserted, and the tier 2 bump replaces it wholesale:

```
3.0.4:  rng = rng || randomBytes
3.1.0:  rng = rng || (size => Buffer.from(utils_1.randomBytes(size)))
```

`bip39` 3.1.0 drops `create-hash`, `pbkdf2` and `randombytes` for
`@noble/hashes`, whose `randomBytes` also fails closed, so the substitution is
not itself a weakening. It is a silent replacement of the entropy source for
every wallet the application creates, arriving as one line of a lockfile diff,
observed by nothing.

### A skipped test has hidden a broken crypto path since 2021

`generateMnemonic(9)` maps a 9-word request to 96 bits of entropy. BIP39's
minimum is 128, so `entropyToMnemonic` rejects it and the call throws
`Invalid entropy`. `generateAdditionalMnemonics` is that call, and
`WalletsStore._generateCertificate` reaches it at line 1433, so paper wallet
certificate creation fails.

The scenario covering it,
`tests/wallets/unit/features/scrambling-and-unscrambling-mnemonics.feature`,
was retagged from `@unit` to `@unit @skip` on 2021-06-13 in `c6dd7d9fe`, a
pull request titled "Fix automated tests setup". `test:unit` runs
`--tags '@unit and not @skip and not @wip'`, so it has not executed since. A
crypto test was switched off during a test-setup repair and the defect
underneath it survived five years of green builds.

That is the failure mode this branch is being widened to prevent, already
realised in this repository.

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
- Mnemonic and seed derivation is asserted against the published BIP39 test
  vectors, on both pbkdf2 resolutions, before the bumps and after them.
- The entropy behind wallet generation is a first-party module with explicit
  failure behaviour, not whatever a transitive dependency currently defaults to.
- Weakening the entropy path cannot happen quietly. It requires visibly editing
  files named for that purpose, and it fails checks named for that purpose.
- The crypto assertions run on every CI build, as constituents of `required`.
- The mechanical and consequential runtime bumps are taken, and everything not
  taken has a written reason.

## Non-Goals

- Changing any formatting option. `arrowParens`, print width, quote style and
  trailing commas stay as they are. `.prettierrc` does gain `printWidth`,
  `tabWidth`, `useTabs` and `endOfLine`, all at the values already in force and
  verified to reformat nothing, because prettier's CLI reads `.editorconfig` by
  default and those are the four options it can reach. Writing down a value that
  is already in effect is not a change to it. The open `arrowParens` question is
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
- Removing the paper wallet creation feature. The decision to retire it is
  recorded here and the certificate restore vector is captured here, because it
  must be captured while the scrambling code still exists. Deleting the code
  removes an IPC channel and is separate work.
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
- `source/renderer/app/api/api.ts`, `source/renderer/app/api/utils/mnemonics.ts`
- `source/renderer/app/stores/WalletsStore.ts`
- `source/common/config/crypto/valid-words.en.ts`
- `source/renderer/webpack.config.js`, `jest.config.js`, `.eslintrc`
- `tests/wallets/unit/steps/mnemonics.ts` and the two `@unit` feature files
- `node_modules/bip39@3.0.4/src/index.js` and `bip39@3.1.0/src/index.js`
- `node_modules/randombytes/browser.js`, `@noble/hashes/utils.js`
- `node_modules/pbkdf2/package.json`, its `browser` field, and both
  implementations
- The published BIP39 vectors, `trezor/python-mnemonic`, `vectors.json`
- The OSV advisory records for `pbkdf2@3.1.2` and `lodash@4.17.21`
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
- The branch takes one source change to the crypto path, and takes it before the
  tier 2 bumps: entropy generation becomes an explicit first-party dependency of
  `generateMnemonic` rather than a bip39 default. The earlier constraint
  excluding source changes was written for a formatter bump, and is wrong for a
  branch that replaces the entropy source underneath wallet creation.
- Known-answer values come from the published BIP39 vectors, not from this
  implementation's current output. A self-captured value proves the output did
  not change; it cannot prove the output was ever right. Both are available
  here, so the standard is what the suite asserts.
- The crypto assertions cover both pbkdf2 resolutions, because
  `webpack.config.js` is `target: 'web'` and ships `pbkdf2/browser.js`, while
  Jest resolves `main` and exercises the Node implementation.
- No crypto test on this branch may be skipped, and deleting one must fail a
  check rather than reduce coverage silently.

## Requirements

### Functional Requirements

Formatter parity:

- [x] `package.json` names `prettier` at `3.6.2`, and `yarn.lock` resolves to it
- [x] The prettier commit touches only `package.json` and `yarn.lock`
- [x] `yarn prettier:check` and `nix fmt -- --ci` both exit 0 on a clean checkout
- [x] `yarn prettier:format` produces an empty diff
- [ ] No script or document passes `--loglevel`
- [x] `perSystem/formatter.nix` sets no `programs.prettier.settings`, and the
      generated treefmt config passes no `--config` for prettier
- [x] A check fails, naming both versions, when the prettier in `package.json`
      and the prettier treefmt runs differ

Crypto assurance:

- [x] The 24 published BIP39 English vectors are committed as a fixture with
      their upstream source recorded
- [x] The fixture's entry count is asserted, so removing a vector fails the
      suite rather than quietly reducing coverage
- [x] Every vector is asserted for entropy to mnemonic and for mnemonic to seed
- [x] Seed assertions run against both the Node and the browser pbkdf2
      resolution
- [x] `secureRandomBytes` exists as a first-party module, throws when no
      platform CSPRNG is present, and never falls back to a weaker source
- [x] `secureRandomBytes` rejects all-zero output and a repeated draw
- [x] `generateMnemonic` takes its entropy from `secureRandomBytes` by explicit
      argument, not from a bip39 default
- [x] A provenance test proves the words shown to the user decode back to
      exactly the bytes the platform CSPRNG produced, with nothing transformed,
      truncated or discarded on the way
- [x] Lint rejects `Math.random`, `Buffer.allocUnsafe`, and
      `bip39.generateMnemonic` called anywhere but the entropy module
- [x] Coverage of `crypto.ts` and `entropy.ts` is thresholded, so deleting a
      test fails the build
- [x] The crypto assertions are a separately named check inside `required`
- [x] A recorded paper wallet certificate restores to its known phrase, and the
      assertion does not depend on the scrambling code existing
- [x] No crypto scenario carries `@skip` or `@wip`, and none is left skipped by
      deleting the coverage rather than moving it
- [x] Every assertion above has been observed to fail when the thing it protects
      is broken
- [ ] All of the above are committed and green **before** any tier 2 bump

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
- **Crypto assertions are not negotiable.** A failing conformance or provenance
  assertion stops the branch. It is never resolved by updating the expected
  value, relaxing a threshold, or skipping the scenario. This branch exists in
  part because that resolution was chosen once before, in 2021, and held for
  five years.

## Technical Design

### Components Affected

- `package.json`: prettier version, `--loglevel` spelling in three scripts, 21
  dependency versions, 2 `resolutions` entries
- `yarn.lock`: regenerated
- `perSystem/formatter.nix`: drop `programs.prettier.settings`, drop the inert
  prettier `includes` and `excludes`
- `perSystem/checks.nix`: add the prettier version parity check
- `source/renderer/app/utils/entropy.ts`: new, the first-party CSPRNG wrapper
- `source/renderer/app/utils/entropy.spec.ts`: new, guard and failure-mode tests
- `source/renderer/app/utils/crypto.ts`: one line, passing `secureRandomBytes`
  to `bip39.generateMnemonic` in place of `null`
- `source/renderer/app/utils/crypto.spec.ts`: new, the vector and provenance
  suite
- `source/renderer/app/utils/__fixtures__/bip39-vectors.json`: new, the
  published English vectors
- `source/renderer/app/utils/__fixtures__/paper-wallet-certificate.json`: new,
  the recorded restore vector
- `tests/wallets/unit/features/scrambling-and-unscrambling-mnemonics.feature`
  and the creation-side steps in `tests/wallets/unit/steps/mnemonics.ts`:
  removed, their coverage moved to the Jest vector suite
- `.eslintrc`: restricted globals and properties on the crypto path
- `jest.config.js`: coverage thresholds for the crypto path
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

Implemented with a deviation from the sketch below: the pinned version is read
with `builtins.readFile ../package.json` at evaluation time rather than with
`jq` at build time. That drops the `jq` dependency, avoids taking a dependency
on the whole flake source through `${inputs.self}`, and lets the failure message
embed the pinned version directly.

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

Measured after the removal, and larger than this section originally assumed.
treefmt now hands prettier 1969 files rather than 1955; the 14 are the locale,
newsfeed and e2e document files the deleted `excludes` had covered, which
prettier declines through `.prettierignore`. More significant, the directory
entries in `settings.global.excludes` do not exclude directory contents at all:
treefmt matches prettier against all 326 tracked files under `.agent`, and
against `CHANGELOG.md`. `prettier --file-info` reports `ignored: true` for every
one of them, so `.prettierignore` is the only thing holding that line, and it
was the only thing holding it before this change too.

The residual risk is therefore not hypothetical bookkeeping. If a future prettier
stopped applying `.prettierignore` to explicit paths, treefmt would begin
formatting every Markdown file in the tree, `CHANGELOG.md` and this plan
included. That would be a loud diff on the
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

### Crypto assurance

The phase opens by asserting where the implementation stands today, against the
standard rather than against itself, and every later step preserves that
assertion.

**Conformance, measured before anything moves.** The published English vectors
from `trezor/python-mnemonic` are 24 entries at 12, 18 and 24 words, each giving
entropy, mnemonic and seed. Current behaviour matches them:

| Assertion | Result |
|---|---|
| `mnemonicToSeedHex(vector, 'TREZOR')` | first 32 bytes of the published seed |
| The same through `pbkdf2/browser.js` | identical |
| The same with raw `Uint8Array` input | identical |
| No-password path, `unorm.nfkd(undefined)` | `""`, so the salt is `mnemonic` |
| `valid-words.en.ts` against `bip39.wordlists.english` | 2048 words, 0 differences |

`mnemonicToSeedHex` returns 32 bytes where BIP39's seed is 64, so the assertion
is against the leading half of the published value. That truncation is existing
behaviour and is not changed here.

**Both resolutions.** `webpack.config.js` is `target: 'web'`, so the shipped
renderer bundles `pbkdf2/browser.js`. Jest has no `browser: true`, so it
resolves `main` and exercises the Node implementation. A suite that tests only
what Jest resolves says nothing about what ships. The spec requires
`pbkdf2/browser.js` explicitly alongside the default resolution and asserts the
same vectors on both.

The spec is colocated as `source/renderer/app/utils/crypto.spec.ts`, per the
repository convention that Jest specs sit next to the unit and `tests/` is
Cucumber territory.

### The paper wallet restore vector

Paper wallet creation is retired and restore is kept, so restore needs coverage
that does not depend on the creation path existing. A round trip cannot provide
it: scrambling and unscrambling with the same code proves self-consistency, not
that a certificate printed years ago still opens.

A recorded certificate does. Captured on this branch, while the scrambling code
is still present, from the published `abandon ... about` BIP39 vector as the
wallet phrase and nine fixed words as the certificate password half:

```
certificate  soccer cruel cloth apple witness mimic hero resemble entry chase
             fruit hurry close riot educate idea mom moral
             legal winner thank year wave sausage worth useful legal
restores to  abandon abandon abandon abandon abandon abandon
             abandon abandon abandon abandon abandon about
passphrase   8c58fb2e030c9664c3cb95097e62755a462fd5a2ed7196fc656f0c67bd446200
```

Verified deterministic across repeated runs. `getScrambledInput` splits the 27
words into 18 and 9, derives the passphrase from the second half, and
`unscramblePaperWalletMnemonic` returns the original phrase. That is exactly the
path `StepMnemonicsContainer` takes at lines 57 to 60, so the vector exercises
what a real restore exercises. The scrambling that produced it lives in
`rust-cardano-crypto`, which this branch does not bump, so the recorded value
pins the same implementation that produced the historical certificates.

The `abandon ... about` phrase is the canonical published BIP39 test vector and
is universally recognised as one, which is the property a committed fixture
needs.

**Module readiness.** `rust-cardano-crypto` populates its exported `RustModule`
object from a promise registered at import time, so `await loadRustModule()` is
not sufficient: the object is still empty when that await resolves. A test must
wait for `RustModule` to be populated. The application calls
`unscrambleStrings` synchronously with no such guarantee, which works in
practice because the module loads during startup long before a restore, but it
is an unguarded race and is recorded as a finding.

### The entropy module

`source/renderer/app/utils/entropy.ts` exports one function and states its
guarantee where a reader will find it:

```ts
export const secureRandomBytes = (size: number): Buffer => {
  if (!Number.isInteger(size) || size <= 0 || size > MAX_BYTES) throw ...;
  const source = globalThis.crypto;
  if (!source || typeof source.getRandomValues !== 'function') throw ...;
  const bytes = Buffer.alloc(size);
  source.getRandomValues(bytes);
  if (bytes.every((b) => b === 0)) throw ...;
  if (hex === previous) throw ...;
  previous = hex;
  return bytes;
};
```

`Buffer.alloc` rather than `allocUnsafe` is deliberate. `randombytes` uses
`allocUnsafe`, so a fill that silently no-ops yields whatever occupied that heap
page, which looks random and is not. Zero-filled memory trips the next guard
instead. The all-zero and repeated-draw checks are the continuous health test a
DRBG is expected to carry. At 32 bytes a genuine all-zero draw has probability
2^-256, so rejecting it costs nothing and catches the exact failure named in the
`pbkdf2` advisory this branch clears.

`crypto.ts:54` then reads:

```ts
return bip39.generateMnemonic(ent, secureRandomBytes, validWords);
```

After which no dependency bump can change where wallet entropy comes from.

### Proving the part no vector can reach

Randomness cannot be asserted by a known-answer test. The seam around it can,
and that is where the assurance lives.

The provenance test stubs `crypto.getRandomValues` to emit a known pattern,
calls `generateMnemonic(24)`, and asserts that `bip39.mnemonicToEntropy` of the
result equals exactly those bytes. One assertion proves that the entropy came
from the platform CSPRNG, that all 256 bits reached the phrase, and that nothing
transformed, truncated or discarded them on the way. It cannot be passed by an
implementation that sources entropy anywhere else.

Around it sit the failure-mode tests: `secureRandomBytes` throws with no
`getRandomValues`, throws on all-zero output, throws on a repeated draw, and
returns the source bytes unmodified. `generateMnemonic(24)` requests exactly 32
bytes. Fed a vector's entropy, it returns that vector's exact mnemonic, which
joins the random path to the proven one.

### What stops a single commit from weakening this

No check in a repository can defend itself against a commit that also edits the
check. The achievable guarantees are narrower than that, and worth stating
precisely, because overstating them is its own risk.

**Nothing weakens quietly.** Four independent controls have to be defeated, and
each is named for what it protects:

1. The vector suite fails if derivation output moves.
2. The provenance test fails if entropy stops coming from the platform CSPRNG.
3. Lint rejects `Math.random`, `Buffer.allocUnsafe`, and
   `bip39.generateMnemonic` called anywhere but the entropy module, so the
   obvious substitutions do not lint clean.
4. A coverage threshold on `crypto.ts` and `entropy.ts` fails the build when a
   test is deleted rather than fixed, which is the specific move that hid the
   9-word defect for five years.

Defeating them means editing `entropy.ts`, `crypto.spec.ts`, `.eslintrc`,
`jest.config.js` and `perSystem/checks.nix` in one commit. That is not a diff a
reviewer reads as routine, which is the point. The control is not that
weakening is impossible; it is that weakening cannot be mistaken for something
else.

**Weakening requires a second party.** The only control a single commit
genuinely cannot pass is a human one. This repository has no `CODEOWNERS` file.
Adding one that names the crypto paths, backed by branch protection, converts
"conspicuous" into "requires a second approver". It needs repository
administrator rights, so it is proposed here rather than implemented here.

## Implementation Strategy

One branch, thirteen commits, each independently revertible.

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
5. **`chore(scripts): run the merge gate in check:all`.** `check:all` calls
   `nix fmt -- --ci` in place of `yarn prettier:check`, so the documented local
   pre-flight and the required check become the same command.
6. **`test(crypto): add the published BIP39 vectors as a fixture`.** The 24
   English vectors with their upstream source recorded, and the count asserted.
7. **`test(crypto): assert BIP39 conformance on both pbkdf2 resolutions`.** The
   vector suite, green on the current dependency set. This is the baseline the
   rest of the branch preserves.
8. **`feat(crypto): generate wallet entropy through a first-party CSPRNG`.**
   `entropy.ts`, its failure-mode spec, and the one-line change at
   `crypto.ts:54`.
9. **`test(crypto): prove recovery phrase entropy reaches the phrase intact`.**
   The provenance test.
10. **`ci: reject weak randomness and untested crypto`.** Lint restrictions,
    coverage thresholds, and the separately named `crypto-vectors` check.
11. **`chore(deps): take the mechanical runtime bumps`.** The 13 tier 1
    packages, one commit.
12. **`chore(deps): clear the lodash and pbkdf2 advisories`.** The 8 tier 2
    packages, including both `resolutions` edits. `yarn audit` counts recorded
    in the commit body. The suites from steps 7 and 9 pass unchanged, or the
    bump stops.
13. **`docs(agent): record the unused runtime dependency question`.** The
    investigation result as a finding. No dependency removed.

Steps 2 and 3 both edit `perSystem/formatter.nix`; keeping them separate makes
the second reviewable as a pure deletion. Steps 6 through 10 all precede step 12
by design, so the assertions witness the bump rather than being written around
whatever it produced.

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

**Crypto assurance (steps 6 to 10):**

```bash
yarn jest source/renderer/app/utils/crypto.spec.ts --coverage=false
yarn jest source/renderer/app/utils/entropy.spec.ts --coverage=false
yarn test:jest                       # coverage thresholds apply here
yarn test:unit                       # the Cucumber @unit features
nix build -L .#checks.x86_64-linux.crypto-vectors
```

Each assertion is also verified to fail: alter one expected vector, stub the
CSPRNG to return zeros, and route entropy around `secureRandomBytes`. A test
that has never been observed failing is not evidence.

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
   **Decided on 2026-08-27: yes.** CI gates on `required`, which collects every
   derivation in `checks.x86_64-linux`, which is a longer list than this document
   originally recorded: `treefmt`, `lint`, `compile`, `stylelint`, `i18n`,
   `storybook`, `shellcheck`, `jest`, `cucumber-unit`, `bundle-integrity`,
   `drt-clippy`, `watchdog-clippy` and `watchdog-test`.
   Nothing in CI runs `yarn check:all` or `yarn prettier:check`. So `check:all`
   is already a local mirror of the required set, and the formatter was the one
   member it mirrored with the wrong tool. The accepted cost is that
   `nix fmt -- --ci` writes to the working tree where `yarn prettier:check` only
   reads, and that the local pre-flight now requires Nix, which every other
   workflow in this repository already does.
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
5. **What happens to the 9-word paper wallet defect?**
   **Decided on 2026-08-27: retire creation, keep and prove restore.** The
   sidebar category is hard-disabled at `SidebarStore.ts:122`
   (`PAPER_WALLET_CREATE_CERTIFICATE: false`), so the flow has had no entry
   point for years, which is why a function that throws went unnoticed. Restore
   stays, because a certificate printed years ago may still be held by someone,
   and it gains the fixed vector it never had.

   The creation code itself is not removed on this branch. Removing it means
   deleting an IPC channel (`generatePaperWalletChannel`), a PDF generator, six
   PNG assets and a font, roughly 1,900 lines across 24 renderer files, plus
   i18n keys, Storybook stories, and store, action, route and sidebar wiring.
   This PRD states that it makes no IPC changes, and that constraint is right:
   feature removal is not dependency work and deserves its own review and its
   own QA. It is recorded as a finding instead.

## Status Log

Append-only. New entries go at the end.

| Date | Entry |
|---|---|
| 2026-08-26 | Plan written, status Draft. Formatter divergence and dependency inventory measured at `50e6b84b3`. |
| 2026-08-27 | Pre-flight re-measured on `chore/dependency-hygiene` at `27f133935`. `yarn prettier:check` reports 210 files and `nix fmt -- --ci` reports 0 changed on the same tree, so the premise holds. `yarn compile` clean, `yarn lint` 0 errors, Jest harness working, `crypto.ts` loads under Jest. |
| 2026-08-27 | Found that the dev shell's `yarn build:electron` has been aborting on every shell entry: `scripts/rebuild-native-modules.sh` opens with `chmod -R +w node_modules/` under `set -o errexit`, and `node_modules/.cache/storybook/10.5.10` is root-owned. Native modules are not being rebuilt against Electron's ABI. Blocks the packaged-build acceptance step, not the earlier phases. |
| 2026-08-27 | Open Question 1 decided: `check:all` runs `nix fmt -- --ci`. Recorded there with the reasoning and the accepted cost. |
| 2026-08-27 | Open Question 5 decided: retire paper wallet creation, keep restore, prove restore with a recorded certificate vector rather than a round trip. Creation code removal is a separate branch, because it deletes an IPC channel and roughly 1,900 lines across 24 files. |
| 2026-08-27 | Restore vector captured and verified deterministic. Recorded in Technical Design. Captured now because it cannot be captured once the scrambling code is gone. |
| 2026-08-27 | Root-owned `node_modules/.cache/storybook/10.5.10` cleared, so `yarn build:electron` is no longer aborting on dev shell entry. The task-015 blocker is lifted. |
| 2026-08-27 | Phase 1 complete. prettier moved to 3.6.2, the three `--loglevel` scripts renamed, lockfile regenerated in the Nix dev shell. `yarn prettier:check` exits 0, `nix fmt -- --ci` reports 0 changed, and `yarn prettier:format` leaves only `package.json` and `yarn.lock` changed. The premise held; no re-measure was needed. |
| 2026-08-27 | Phase 1 verified green in CI. `ci/hydra-build:required`, `ci/hydra-build:nonrequired`, `ci/eval`, Jest on Windows and Cargo on Windows all SUCCESS on the prettier bump, so `checks.treefmt` passes with prettier 3.6.2. |
| 2026-08-27 | Phase 2 complete. treefmt reads `.prettierrc`, the inert prettier include and exclude lists are gone, `prettier-version-parity` is in the required set and verified failing as well as passing, and `check:all` runs `nix fmt -- --ci` through a new `fmt:check` script. |
| 2026-08-27 | Finding while measuring task-005: `settings.global.excludes` directory entries are inert, and `.prettierignore` is the sole gate on prettier's scope. Recorded under Why removing the prettier `includes` block is safe. Not acted on. |
| 2026-08-27 | `.prettierrc` pinned the four options `.editorconfig` can reach. Measured: prettier's CLI honours `.editorconfig` by default, `.prettierrc` overrides it, and the four keys are `indent_style`, `indent_size`, `max_line_length` and `end_of_line`. With the pins, `indent_size = 4` reformats nothing; without them it puts 1488 files out of conformance. Non-Goals updated. |
| 2026-08-27 | Measured what fixing `settings.global.excludes` would buy, without doing it: suffixing the ten directory entries with `/**` drops treefmt's emitted set from 1969 files to 1639 and takes `.agent` matches from 326 to 0, with `nix fmt -- --ci` still reporting 0 changed. Awaiting a decision. |
| 2026-08-27 | Phase 3 started. BIP39 vector fixture committed with provenance pinned to an upstream commit, and the conformance baseline measured: 24/24 on every axis, both pbkdf2 resolutions. |
| 2026-08-27 | Vector suite landed. 90 assertions, all 24 vectors on both pbkdf2 resolutions plus the entropy mapping, blake2b224, bech32 and both stake address branches. Verified failing under four separate injections before being trusted. |
| 2026-08-27 | Entropy module landed and wired in. Deviation: `jest.setup.js` was needed, because `jest-environment-jsdom` at this version provides no `globalThis.crypto`. That is also a prediction about the `bip39` 3.1.0 bump, which needs the same global through `@noble/hashes`. |
| 2026-08-27 | Provenance test landed, verified against three separate weakenings of `crypto.ts`. Three published vectors use all-zero entropy, which `secureRandomBytes` refuses; asserted as refusals rather than excepted from the guard. |
| 2026-08-27 | Phase 3 complete. Lint restrictions, the `crypto-vectors` check with its coverage floor, and the paper wallet restore vector all landed and were verified failing as well as passing. No crypto scenario is skipped. |
| 2026-08-27 | `settings.global.excludes` fixed with `/**` suffixes, on Adam's decision. Emitted set 1976 to 1645, prettier matches under `.agent` 326 to 0, 0 changed, and alejandra and rustfmt still match all 35 `.nix` and 33 `.rs` files. |
| 2026-08-27 | Scope widened, and the decision to exclude source changes reversed. Investigation found wallet entropy sourced from a `bip39` default that this branch's own bump replaces, and a crypto scenario skipped since 2021 hiding a throwing `generateMnemonic(9)`. Phase 3 becomes a crypto assurance phase, beginning by asserting current conformance against the published BIP39 vectors and ending with controls that make a later weakening conspicuous. Status In Progress. |

---

**Status:** In Progress
**Date:** 2026-08-27
**Author:** Se7en Labs
