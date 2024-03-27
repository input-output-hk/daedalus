'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.createHardwareWallet = void 0;
const request_1 = require('../../utils/request');
const createHardwareWallet = (config, { walletInitData }) => {
  return (0, request_1.request)(
    {
      method: 'POST',
      path: '/v2/wallets',
      ...config,
    },
    {},
    walletInitData
  );
};
exports.createHardwareWallet = createHardwareWallet;
//# sourceMappingURL=createHardwareWallet.js.map
