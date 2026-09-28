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

// Jest's jsdom Uint8Array lives in a different realm from Node Buffer.
// blakejs and exact-CBOR parsers require Buffer to satisfy instanceof Uint8Array.
globalThis.Uint8Array = Object.getPrototypeOf(
  require('node:buffer').Buffer.prototype
).constructor;
