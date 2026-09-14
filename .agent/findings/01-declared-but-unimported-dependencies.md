# 01. Runtime dependencies with no first-party import

Measured on `chore/dependency-hygiene` at `3fc093098`. 17 of 93 runtime
dependencies have no `import`, `require`, dynamic `import()` or `jest.mock` of
the package or any subpath anywhere in `source/`, `storybook/`, `utils/`,
`tests/`, `scripts/`, `installers/`, `hardware-wallet-tests/`, `gulpfile.js`,
`jest.config.js`, `jest.setup.js`, `.eslintrc` or the webpack configs.

**Nothing was removed.** Removing a dependency that turns out to be reachable
breaks a packaged build in a way the check set does not catch, which this branch
demonstrated the hard way for an unrelated reason.

## Two corrections to the earlier inventory

`research/02-dependency-inventory.md` listed 17 packages and drew two
conclusions that this classification contradicts.

**`omit-deep-lodash` is imported.** The inventory recorded that it appears only
in a comment and that "the comment outlived the code". It is imported at
`source/common/utils/logging.ts:1`:

```ts
import omitDeep from 'omit-deep-lodash';
```

**`@iohk-jormungandr/wallet-js` is reached by a dynamic import**, at
`source/renderer/app/utils/walletUtils.ts:4`, so a search for `from` and
`require` misses it. Any classifier for this question has to cover `import()`
and `jest.mock` as well, or it produces false positives that look like findings.

## Classification

### Reached by build configuration, correctly declared

| Package | Mechanism |
|---|---|
| `buffer` | `resolve.fallback` and `ProvidePlugin`, `source/renderer/webpack.config.js:121,130` |
| `process` | `resolve.fallback` and `ProvidePlugin`, same file |
| `url` | `resolve.fallback`, same file |
| `node-hid` | `externals` in `source/main/webpack.config.js:100`, and `scripts/rebuild-native-modules.sh:71` |
| `cardano-launcher` | bundled by `nix/internal/any-darwin.nix:238` and named by the Haskell installers |

These are named in build configuration rather than imported, which is why a
source search does not see them. They are correctly declared.

### Present in the Nix runtime closure

`graceful-fs`, `safe-buffer`, `source-map-support` and `glob` appear in
`nix/internal/runtime-nodejs-deps.json`. That file is a 437-entry transitive
closure, so membership says some package needs them, not that Daedalus needs to
declare them directly. It is evidence against removal, not evidence for the
declaration.

### A version mismatch worth its own attention

`@ledgerhq/hw-transport-node-hid` is declared at 6.33.0 and never imported. What
the code imports, in three files under
`source/main/ipc/hardwareWallets/ledger/`, is
`@ledgerhq/hw-transport-node-hid-noevents`, which is **not declared** and
arrives transitively from the declared package at 6.35.1.

So the declaration is load-bearing, because removing it removes the package that
is actually used. But the arrangement is fragile in a way worth naming: the
hardware wallet code depends on a package the manifest does not mention, at a
version that floats with whatever its parent resolves. This belongs to the
hardware wallet compatibility arc.

### No reference of any kind

`find-process`, `nanoid`, `prop-types`, `rotating-file-stream` and
`tcp-port-used` have no quoted reference anywhere outside `package.json`.

`prop-types` is worth calling out because it looks referenced and is not: the
only match is `/* eslint-disable react/prop-types */` in
`source/renderer/app/ThemeManager.tsx:1`, which names an ESLint rule, not the
package.

These five are the strandedness candidates. None is proven stranded, because a
package can still be required at runtime by something webpack bundles without
appearing in a source search.

## What would settle it

`scripts/find-missing-runtime-deps.py` already exists and computes the runtime
`require()` closure of the distributed `node_modules` against a built
`daedalus-js`. It solves the inverse problem, finding packages missing **from**
`runtime-nodejs-deps.json`, and it is wired into nothing: no reference in
`package.json`, `nix/`, `perSystem/` or `gulpfile.js`.

Pointing that closure at the five candidates would answer the question with
evidence rather than absence of evidence. That needs a built installer, which is
why it is a separate task rather than part of a dependency bump.

## One that this branch created

`safe-buffer` is newly unimported. The tier 2 commit removed the last
first-party import of it, from `source/renderer/app/utils/crypto.ts` and both
crypto specs, because `pbkdf2` 3.1.6 and `blakejs` 1.2.1 type against Node's
`Buffer` and `safe-buffer` ships its own narrower declarations. It remains
declared at 5.2.1 and remains in the runtime closure.

Recorded rather than removed, for the same reason as the rest.
