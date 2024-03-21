'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.hexToString = exports.ellipsis = void 0;
const ellipsis = (str, minCharsInit, minCharsEnd) => {
  if (str.length <= minCharsInit) return str;
  const initStr = str.substr(0, minCharsInit);
  const shouldHaveEndStr = () =>
    minCharsEnd && str.length - minCharsInit > minCharsEnd;
  const endStr =
    minCharsEnd && shouldHaveEndStr()
      ? str.substr(str.length - minCharsEnd)
      : '';
  return `${initStr}\u2026${endStr}`;
};
exports.ellipsis = ellipsis;
const hexToString = (valueInHex) => Buffer.from(valueInHex, 'hex').toString();
exports.hexToString = hexToString;
//# sourceMappingURL=strings.js.map
