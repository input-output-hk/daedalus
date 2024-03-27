'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.importWalletAsJSON = void 0;
const requestV0_1 = require('../../utils/requestV0');
const importWalletAsJSON = (config, filePath) =>
  (0, requestV0_1.request)(
    {
      method: 'POST',
      path: '/api/backup/import',
      ...config,
    },
    {},
    filePath
  );
exports.importWalletAsJSON = importWalletAsJSON;
//# sourceMappingURL=importWalletAsJSON.js.map
