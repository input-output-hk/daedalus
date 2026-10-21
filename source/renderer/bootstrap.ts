// Preserve the preload's shared global object; browser-only development uses window.
// Keep this in the external bundle so CSP never needs to permit inline scripts.
if (typeof globalThis.global === 'undefined') {
  Object.defineProperty(globalThis, 'global', {
    value: globalThis,
    writable: true,
    configurable: true,
  });
}
