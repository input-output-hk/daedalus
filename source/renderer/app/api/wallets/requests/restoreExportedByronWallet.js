'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.restoreExportedByronWallet = void 0;
const request_1 = require('../../utils/request');
const restoreExportedByronWallet = (config, { walletInitData }) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: '/v2/byron-wallets',
      ...config,
    },
    {},
    { ...walletInitData, style: 'random' }
  );
exports.restoreExportedByronWallet = restoreExportedByronWallet;
//# sourceMappingURL=restoreExportedByronWallet.js.map
