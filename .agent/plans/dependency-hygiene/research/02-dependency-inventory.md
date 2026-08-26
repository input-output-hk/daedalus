# 02. Runtime Dependency Inventory

Computed against `master` at `50e6b84b3` on 2026-08-26, by resolving every
entry in `dependencies` from the npm registry and taking the highest release
that stays within the pinned major.

`package.json` declares 93 runtime dependencies. 58 have a non-major upgrade
available. This note sorts them into what this branch takes, what it defers and
why, and what needs a focused look before anyone touches it.

## 1. Tier 1, mechanical

Utility packages with no rendering surface, no key material, and no arithmetic
on user balances. The existing check set covers them: a break shows up as a
failed compile, lint, Jest, or Cucumber run.

| Package | From | To |
|---|---|---|
| `check-disk-space` | 3.2.0 | 3.4.0 |
| `classnames` | 2.2.6 | 2.5.1 |
| `csv-stringify` | 5.5.1 | 5.6.5 |
| `electron-store` | 8.0.1 | 8.2.0 |
| `form-data` | 3.0.0 | 3.0.5 |
| `fs-extra` | 9.0.1 | 9.1.0 |
| `humanize-duration` | 3.23.1 | 3.34.1 |
| `mime-types` | 2.1.27 | 2.1.35 |
| `node-downloader-helper` | 1.0.18 | 1.0.19 |
| `sanitize-filename` | 1.6.3 | 1.6.4 |
| `semver` | 7.3.5 | 7.8.5 |
| `url` | 0.11.0 | 0.11.4 |
| `validator` | 13.7.0 | 13.15.35 |

Two of these are reached in ways a plain import search misses, and both are
genuinely used:

- `validator` through a deep import,
  `source/renderer/app/utils/validations.ts:2`:
  `import isInt from 'validator/lib/isInt'`
- `form-data` likewise, `source/main/ipc/bugReportRequestChannel.ts:2`:
  `import FormData from 'form-data/lib/form_data'`
- `url` is not imported at all. It is a webpack `resolve.fallback` entry,
  `source/renderer/webpack.config.js:120`: `url: require.resolve('url')`.

`electron-store` is listed here but persists application configuration, so a
release note read is worth the two minutes before taking it.

## 2. Tier 2, consequential and CI-verifiable

These carry real consequence. Each is still verifiable without hardware or a
human eye, which is what separates them from the deferred tiers.

| Package | From | To | Why it needs care |
|---|---|---|---|
| `pbkdf2` | 3.1.2 | 3.1.6 | Mnemonic-to-seed derivation. Also a `resolutions` entry |
| `lodash` | 4.17.21 | 4.18.1 | Advisory needs `>=4.18.0`. Also a `resolutions` entry. 135 import sites |
| `lodash-es` | 4.17.15 | 4.18.1 | Renderer half of the same package |
| `bip39` | 3.0.4 | 3.1.0 | Mnemonic generation and validation |
| `blakejs` | 1.1.0 | 1.2.1 | Hashing in address derivation |
| `blake2b` | 2.1.3 | 2.1.4 | As above |
| `bignumber.js` | 9.0.1 | 9.3.1 | Every displayed and submitted amount. 91 import sites |
| `cbor` | 5.0.2 | 5.2.0 | Transaction and metadata serialization |

### The `resolutions` entries

`pbkdf2` and `lodash` appear twice in `package.json`, once under
`dependencies` and once under `resolutions`:

```
"resolutions": {
  ...
  "**/**/lodash": "4.17.21",
  "pbkdf2": "3.1.2"
}
```

A `resolutions` entry overrides the whole tree. Bumping only the `dependencies`
line leaves the old version installed and the advisory unresolved, with the
change looking complete in the diff. Both lines move together or neither does.

### The gap these bumps sit on top of

Four of the eight land in one file. `source/renderer/app/utils/crypto.ts`
imports `bip39`, `blakejs`, `pbkdf2` and `lodash` together, and derives
mnemonics and seeds:

```ts
import * as bip39 from 'bip39';
import { blake2b } from 'blakejs';
import { chunk } from 'lodash';
import { pbkdf2Sync as pbkdf2 } from 'pbkdf2';
```

That file has **no colocated Jest spec**. The only automated coverage is
Cucumber `@unit`, through
`tests/wallets/unit/steps/mnemonics.ts` and the two features
`mnemonics-generation-and-validation.feature` and
`scrambling-and-unscrambling-mnemonics.feature`, which exercise
`generateMnemonic`, `mnemonicToSeedHex` and the scramble round trip.

Those tests are property shaped: they generate, then validate what they
generated. A change that altered derivation *consistently* would generate a
different seed and still validate it, and every check would stay green. There
is no fixed input with a known expected output anywhere in the suite.

`bip39` 3.1.0 in particular replaced its internal hashing implementation, so
it is exactly the kind of change a round-trip test cannot see.

Closing that gap with known-answer vectors is the one piece of new test code
this branch adds, and it lands before the bumps rather than after.

## 3. Deferred, with the reason

| Package or group | Available | Why not here |
|---|---|---|
| `@cardano-sdk/core` | 0.41.4 to 0.47.0 | Pre-1.0, so minors carry breaking changes. Cardano protocol logic |
| `pdfkit` | 0.8.3 to 0.20.1 | Pre-1.0, twelve minors |
| `usb`, `node-hid` | 2.15.0 to 2.18.0, 3.3.0 to 3.4.0 | Native modules. Rebuilt by `scripts/rebuild-native-modules.sh` and bundled through per-platform N-API paths in `nix/internal/any-darwin.nix`. Both also have `resolutions` entries. Not a version bump |
| `electron` | 41.3.0 to 41.10.7 | Moved from 24.2.0 to 41.3.0 at 11.2.0. Its own change, with its own verification |
| `cardano-crypto.js` | pinned 5.3.6-rc.6 | The pin is *ahead* of the latest stable 5.3.5. Taking "the latest 5.3.x" would be a downgrade |
| `cardano-launcher` | 0.20220119.0 | Already the latest published version |
| `@iohk-jormungandr/wallet-js` | 0.5.0-pre7 | Prerelease line, no stable successor |
| `elliptic` (resolutions, 6.5.4) | 6.6.1 clears the advisory | Separate decision. It arrives through the Trezor path and the pin exists for a reason nobody has recovered yet |
| UI tier, 16 packages | `react-datetime`, `react-virtualized`, `react-table`, `rc-slider`, `recharts`, `chroma-js`, `fuse.js`, `qrcode.react`, `highlight-words`, `fireworks-js`, `mobx-react-form`, `react-router`, `react-router-dom`, `react-animate-height`, `react-copy-to-clipboard`, `react-lottie` | A minor bump can shift rendering with every check still green. Needs a Storybook pass per component |
| Hardware wallet tier, 3 packages | `@trezor/connect` 9.7.2 to 9.7.3, `@trezor/transport` 1.5.4 to 1.6.3, `@ledgerhq/hw-transport-node-hid` 6.33.0 to 6.33.5 | Not verifiable in CI. `hardware-wallet-tests/index.ts` is an interactive prompt driver requiring physical Ledger and Trezor devices |

## 4. Declared but not imported: needs investigation, not action

17 of the 93 runtime dependencies have no `import` or `require` of the package
or any of its subpaths anywhere in `source/`, `storybook/`, `utils/`, `tests/`,
`scripts/`, `installers/`, `hardware-wallet-tests/`, `gulpfile.js` or
`jest.config.js`:

```
@ledgerhq/hw-transport-node-hid   graceful-fs      prop-types
buffer                            nanoid           rotating-file-stream
cardano-launcher                  node-hid         source-map-support
find-process                      omit-deep-lodash tail
glob                              process          tcp-port-used
                                  url              util
```

**This list is a starting point, not a verdict.** At least four entries are
already explained and are correctly declared:

- `buffer`, `process` and `url` are webpack `resolve.fallback` and
  `ProvidePlugin` entries, `source/renderer/webpack.config.js:113-130`. They are
  named in build configuration rather than imported.
- `node-hid` and `@ledgerhq/hw-transport-node-hid` are reached through the
  hardware wallet transport layer, which webpack rewrites via
  `NormalModuleReplacementPlugin` (`source/renderer/webpack.config.js:164`).

Others look like genuine misclassification of the same kind already corrected
for `gulp`, `inquirer` and `cucumber-html-reporter`. Two have positive evidence
of being stranded:

- `find-process` is a declared production dependency with no import. It appears
  only in planning documents, as a helper that was considered and not used.
- `omit-deep-lodash` is named in a comment at
  `source/common/utils/logging.ts:42` describing how `filterLogData` redacts by
  key, but that file imports only types. The comment outlived the code.

Each entry needs its own answer: reached by a build-configuration mechanism,
required transitively at runtime and tracked in
`nix/internal/runtime-nodejs-deps.json`, or genuinely stranded. Note that
absence from `runtime-nodejs-deps.json` proves nothing on its own, because that
file describes the main-process runtime closure and anything webpack bundles
into the renderer will not appear in it.

Resolving this is a separate task with its own verification, because removing a
dependency that turns out to be reachable breaks a packaged build in a way the
check set does not catch.

## What this inventory does not establish

- Whether any of the tier 1 or tier 2 targets has a behavioural change in its
  release notes. Versions were resolved from the registry; changelogs were not
  read. That reading is part of taking each bump, not part of listing them.
- Whether the advisory count actually falls after the `lodash` and `pbkdf2`
  moves. That is measured by running `yarn audit` before and after, not
  predicted here.
- What ships. Section 2 of the earlier audit noted that webpack decides what
  reaches a user, and an installed version is not automatically a shipped one.
- Anything about `devDependencies`. Only the 93 runtime entries were resolved.
