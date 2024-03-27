'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.exportWalletAsJSON = void 0;
const requestV0_1 = require('../../utils/requestV0');
const exportWalletAsJSON = (config, { walletId, filePath }) =>
  (0, requestV0_1.request)(
    {
      method: 'POST',
      path: `/api/backup/export/${walletId}`,
      ...config,
    },
    {},
    filePath
  );
exports.exportWalletAsJSON = exportWalletAsJSON;
//# sourceMappingURL=exportWalletAsJSON.js.map
