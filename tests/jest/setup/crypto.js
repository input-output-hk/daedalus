// jsdom 16 (bundled with jest-environment-jsdom@27) does not implement
// crypto.getRandomValues. Polyfill with Node's WebCrypto so tests that call
// secureRandomBytes() work without a real browser environment.
if (typeof globalThis.crypto?.getRandomValues !== 'function') {
  const { webcrypto } = require('node:crypto');
  Object.defineProperty(globalThis, 'crypto', {
    value: webcrypto,
    configurable: true,
    writable: true,
  });
}
