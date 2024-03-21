'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.BrowserLocalStorageBridge = exports.LocalStorageFeatureProvider = exports.useLocalStorageFeature = void 0;
var context_1 = require('./context');
Object.defineProperty(exports, 'useLocalStorageFeature', {
  enumerable: true,
  get: function () {
    return context_1.useLocalStorageFeature;
  },
});
Object.defineProperty(exports, 'LocalStorageFeatureProvider', {
  enumerable: true,
  get: function () {
    return context_1.LocalStorageFeatureProvider;
  },
});
var ui_1 = require('./ui');
Object.defineProperty(exports, 'BrowserLocalStorageBridge', {
  enumerable: true,
  get: function () {
    return ui_1.BrowserLocalStorageBridge;
  },
});
//# sourceMappingURL=index.js.map
