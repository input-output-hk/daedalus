# 03. Two fragilities on the crypto path

Both found during the dependency hygiene branch. Neither affects Daedalus as it
ships today. Neither was fixed, because both are outside what a dependency
branch should change.

## `rust-cardano-crypto` has an unguarded readiness race

The package exports a `RustModule` object and populates it from a promise
registered at import time:

```js
let RustModule = {}
loadRustModule().then(module => Object.assign(RustModule, module))
export default RustModule
```

`await loadRustModule()` returns while that object is still empty, measured. Any
call made before it is populated fails with `module.alloc is not a function`,
which points at the input rather than at the timing.

`source/renderer/app/utils/crypto.ts` calls
`CardanoCrypto.PaperWallet.unscrambleStrings` synchronously against that object
with no readiness guarantee. It works because the module loads during
application startup long before anyone restores a paper wallet, but nothing
enforces that ordering.

`source/renderer/app/utils/crypto.spec.ts` waits for the object to be populated
in a `beforeAll`. The application does not.

## `blakejs` 1.2.1 dropped explicit Buffer support

1.1.0 accepted a Buffer through its own branch:

```js
if (input instanceof Uint8Array) { ret = input }
else if (input instanceof Buffer) { ret = new Uint8Array(input) }   // removed in 1.2.1
else if (typeof input === 'string') { ... }
```

1.2.1 relies solely on `instanceof Uint8Array`. That holds wherever `Buffer` and
`Uint8Array` come from the same realm, which is true in the Electron renderer
and in the main process. It fails across a realm boundary, and the error names
the input rather than the realm.

This surfaced under `jest-environment-jsdom`, which injects Node's `Buffer` into
a context owning its own `Uint8Array`. The crypto specs now declare
`@jest-environment node`, which models both shipping environments.

Anything that later runs this code across a realm boundary, a worker, a `vm`
context, or a preload script under `contextIsolation`, will hit it.

## A latent defect the same bump revealed, and which was fixed

`blakejs` 1.2.1 ships TypeScript declarations for the first time. They showed
that `getId` in `source/renderer/app/utils/dataSerialization.ts` called
`.toString('hex')` on the result of `blakejs.blake2b`, which is a `Uint8Array`,
whose `toString` ignores an encoding argument:

```
.toString('hex') gave  "189,221,129,60,99,66,57,114,..."
correct hex would be   bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319
```

A function meant to return a 32-byte hex transaction id returned a
comma-separated decimal list. Pre-existing, since 1.1.0 also returned a
`Uint8Array`; invisible because the package was untyped and the call
type-checked as `any`. `getId` has no callers anywhere in the repository, so
nothing consumed the wrong value.

Fixed in `456460c39` rather than left compiling while knowingly wrong. Recorded
here because the shape generalises: an untyped dependency makes every call
against it unverifiable, and adding types to a dependency is a way to discover
bugs that have been sitting still.
