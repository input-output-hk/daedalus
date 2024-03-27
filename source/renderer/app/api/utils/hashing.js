'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getSHA256HexForString = void 0;
// From: https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest
const getSHA256HexForString = async (str) => {
  const data = new TextEncoder().encode(str);
  const hashBuffer = await window.crypto.subtle.digest('SHA-256', data);
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  return hashArray.map((b) => b.toString(16).padStart(2, '0')).join('');
};
exports.getSHA256HexForString = getSHA256HexForString;
//# sourceMappingURL=hashing.js.map
