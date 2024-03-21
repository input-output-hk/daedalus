'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getEventNameFromWallet = void 0;
const getEventNameFromWallet = (wallet) =>
  wallet.isHardwareWallet ? 'Hardware wallet' : 'Software wallet';
exports.getEventNameFromWallet = getEventNameFromWallet;
//# sourceMappingURL=getEventNameFromWallet.js.map
