'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.BrowserLocalStorageBridge = void 0;
const react_1 = __importDefault(require('react'));
const context_1 = require('../context');
function BrowserLocalStorageBridge({ children }) {
  return react_1.default.createElement(
    context_1.LocalStorageFeatureProvider,
    {
      localStorage: {
        get: (key, defaultValue) =>
          Promise.resolve(localStorage.getItem(key) || defaultValue),
        set: (key, value) => Promise.resolve(localStorage.setItem(key, value)),
        unset: (key) => Promise.resolve(localStorage.removeItem(key)),
      },
    },
    children
  );
}
exports.BrowserLocalStorageBridge = BrowserLocalStorageBridge;
//# sourceMappingURL=BrowserLocalStorageBridge.js.map
