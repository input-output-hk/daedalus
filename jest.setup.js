// `jest-environment-jsdom` at this version provides no `globalThis.crypto`, so
// the platform CSPRNG that the renderer and the Electron main process both have
// is missing under test. Install Node's WebCrypto to stand in for it.
//
// This belongs in test configuration rather than in the code under test. The
// alternative, widening `secureRandomBytes` to accept a second source, would
// add a fallback branch to the one function whose whole purpose is not having
// one. It also matters for `@noble/hashes`, which `bip39` 3.1.0 depends on and
// which reads `globalThis.crypto` the same way.
const { webcrypto } = require('crypto');

if (!globalThis.crypto) {
  Object.defineProperty(globalThis, 'crypto', {
    value: webcrypto,
    configurable: true,
    writable: true,
  });
}
