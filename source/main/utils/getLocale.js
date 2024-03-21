'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getLocale = void 0;
const electron_store_1 = __importDefault(require('electron-store'));
const detectSystemLocale_1 = require('./detectSystemLocale');
const store = new electron_store_1.default();
const getLocale = (network) => {
  const systemLocale = (0, detectSystemLocale_1.detectSystemLocale)();
  try {
    const locale = store.get(`${network}-USER-LOCALE`);
    if (locale) {
      return locale;
    }
    return systemLocale;
  } catch (error) {
    return systemLocale;
  }
};
exports.getLocale = getLocale;
//# sourceMappingURL=getLocale.js.map
