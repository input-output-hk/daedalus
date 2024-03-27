'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.importWalletAsKey = void 0;
const request_1 = require('../../utils/request');
const importWalletAsKey = (config, walletImportData) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: '/api/internal/import-wallet',
      ...config,
    },
    {},
    walletImportData
  );
exports.importWalletAsKey = importWalletAsKey;
//# sourceMappingURL=importWalletAsKey.js.map
