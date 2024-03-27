'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.defaultReplacer = void 0;
const defaultReplacer = () => {
  return (isDiscreetMode, symbol, value) => {
    return isDiscreetMode ? symbol : value;
  };
};
exports.defaultReplacer = defaultReplacer;
//# sourceMappingURL=defaultReplacer.js.map
