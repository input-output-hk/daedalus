'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.default = (obj) =>
  Object.entries(obj)
    .map(
      ([key, val]) => `${encodeURIComponent(key)}=${encodeURIComponent(val)}`
    )
    .join('&');
//# sourceMappingURL=serialize.js.map
