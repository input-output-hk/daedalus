'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const hd_1 = require('cardano-js/dist/hd');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
const getRecoveryWalletIdChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GET_WASM_BINARY_CHANNEL
);
exports.default = () => {
  getRecoveryWalletIdChannel.onRequest(async (recoveryPhrase) => {
    try {
      let xprv;
      let cc;
      if (recoveryPhrase.length === 12) {
        [xprv, cc] = await hd_1.Byron.generateMasterKey(recoveryPhrase);
      } else {
        [xprv, cc] = await hd_1.Icarus.generateMasterKey(recoveryPhrase);
      }
      const walletId = (0, hd_1.newPublicId)(xprv.to_public(), cc);
      return Promise.resolve(walletId);
    } catch (err) {
      return Promise.resolve('');
    }
  });
};
//# sourceMappingURL=getRecoveryWalletIdChannel.js.map
