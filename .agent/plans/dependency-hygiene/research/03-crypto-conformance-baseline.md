# 03. Crypto conformance baseline

Measured on `chore/dependency-hygiene` at `9e46e228a`, on the dependency set as
it stands before any tier 1 or tier 2 bump. Everything here is the state the
rest of the branch must preserve.

Fixture: `source/renderer/app/utils/__fixtures__/bip39-vectors.json`, the 24
published English BIP39 vectors, 8 each at 12, 18 and 24 words, all with the
passphrase `TREZOR`.

## 1. The implementation already matches the standard

| Assertion | Result |
|---|---|
| `mnemonicToSeedHex` against the published seed | 24 / 24 |
| The same through the `pbkdf2` Node resolution | 24 / 24 |
| The same through `pbkdf2/browser.js`, which is what ships | 24 / 24 |
| `entropyToMnemonic` against the published mnemonic | 24 / 24 |
| `mnemonicToEntropy` round trip against the published entropy | 24 / 24 |

This matters for what the branch is allowed to conclude later. Because current
behaviour is conformant, a vector that fails after a bump is the bump being
wrong, not the vector being a snapshot of a pre-existing defect.

## 2. Two pbkdf2 implementations, and Jest tests the wrong one

`source/renderer/webpack.config.js` is `target: 'web'`, so the `browser` field
in `pbkdf2/package.json` applies and the renderer bundles `browser.js`, the
pure-JS implementation. `jest.config.js` sets no `browser: true`, so Jest
resolves `main` and exercises the Node implementation, which delegates to
`crypto.pbkdf2Sync`.

A suite that asserts only what Jest resolves says nothing about what ships. Both
are measured above and both conform, and the spec must keep asserting both.

This is also where the `pbkdf2` advisories live. GHSA-h7cp-r72f-jxh6 concerns
predictable zero-filled output for non-normalized or unimplemented digest names,
and GHSA-v62p-rq8g-8h59 concerns static keys returned for `Uint8Array` input.
`crypto.ts` passes a normalized `'sha512'` and `safe-buffer` Buffers, so neither
applies to this call site. Confirmed directly: the browser implementation
returns the published vector for our inputs, and also for the same inputs passed
as raw `Uint8Array`.

## 3. `mnemonicToSeedHex` returns half of a BIP39 seed

BIP39's seed is 64 bytes. `crypto.ts` derives 32:

```ts
return pbkdf2(mnemonicBuffer, saltBuffer, 2048, 32, 'sha512').toString('hex');
```

Its output is therefore the leading half of the published value, and the
assertions compare against `seedHex.slice(0, 64)`. The value is used as the
paper wallet certificate passphrase, not as a wallet seed. This is existing
behaviour and is recorded, not changed.

## 4. The no-password path is correct, and looks like it is not

`getScrambledInput` calls `mnemonicToSeedHex` with one argument, hence the
`@ts-ignore` at `crypto.ts:83`. The salt line is:

```ts
const salt = `mnemonic${unorm.nfkd(password) || ''}`;
```

`unorm.nfkd(undefined)` returns the empty string rather than the string
`"undefined"`, so the salt is `mnemonic` and the output matches the published
empty-passphrase seed. Recorded because it reads like a defect and is not.

## 5. The wordlist is the standard one

`source/common/config/crypto/valid-words.en.ts` compared positionally against
`bip39.wordlists.english`: 2048 entries, 0 differences. Compared by position
rather than as a set, since BIP39 encodes indices.

## 6. The entropy source is inherited, not asserted

`crypto.ts:54` passes `null` as bip39's rng argument, which resolves as:

```
bip39 3.0.4    rng = rng || randomBytes            require('randombytes')
randombytes    browser.js, because webpack is target: 'web'
               global.crypto.getRandomValues       Chromium CSPRNG
               or module.exports = oldBrowser, which throws
```

Sound today, and fails closed: with no `crypto.getRandomValues` the module
exports a function that throws, and there is no weaker fallback on the path.

`bip39` 3.1.0 replaces it:

```
3.1.0:  rng = rng || (size => Buffer.from(utils_1.randomBytes(size)))
```

`@noble/hashes` `randomBytes` also fails closed, so the substitution is not a
weakening. It is a silent replacement of the entropy source for every wallet the
application creates, and no known-answer test can observe it, because the output
is random by construction. That is what the entropy module and the provenance
test exist to close.

## 7. Reproducing this

```bash
yarn jest source/renderer/app/utils/crypto.spec.ts --coverage=false
```

The fixture's provenance is inside the fixture. Fetch `vectors.json` from
`trezor/python-mnemonic` at commit `b57a5ad77a981e743f4167ab2f7927a55c1e82a8`
and compare its `english` array against the `vectors` array.
