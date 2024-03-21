'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.ensureXDGDataIsSet = void 0;
const ensureXDGDataIsSet = () => {
  if (process.env.HOME && process.env.XDG_DATA_HOME === undefined) {
    process.env.XDG_DATA_HOME = `${process.env.HOME}/.local/share/`;
  }
};
exports.ensureXDGDataIsSet = ensureXDGDataIsSet;
//# sourceMappingURL=config.js.map
