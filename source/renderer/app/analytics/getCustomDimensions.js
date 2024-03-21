'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getCustomDimensions = void 0;
const analyticsConfig_1 = require('../config/analyticsConfig');
const getShortCpuDescription_1 = require('../utils/getShortCpuDescription');
const formatters_1 = require('../utils/formatters');
const booleanToText = (flag) => (flag ? 'yes' : 'no');
const getCustomDimensions = async (environment, adaApi) => {
  const userWallets = await adaApi.getWallets();
  const usesByronWallets = userWallets.some((wallet) => wallet.isLegacy);
  const usesHardwareWallets = userWallets.some(
    (wallet) => wallet.isHardwareWallet
  );
  return {
    [analyticsConfig_1.CPU_DIMENSION_KEY]: (0,
    getShortCpuDescription_1.getShortCpuDescription)(environment.cpu[0]?.model),
    [analyticsConfig_1.RAM_DIMENSION_KEY]: (0,
    formatters_1.formattedBytesToSize)(environment.ram, 0),
    [analyticsConfig_1.OS_DIMENSION_KEY]: environment.os,
    [analyticsConfig_1.VERSION_DIMENSION_KEY]: environment.version,
    [analyticsConfig_1.USES_LEGACY_WALLET_DIMENSION_KEY]: booleanToText(
      usesByronWallets
    ),
    [analyticsConfig_1.USES_HARDWARE_WALLET_DIMENSION_KEY]: booleanToText(
      usesHardwareWallets
    ),
  };
};
exports.getCustomDimensions = getCustomDimensions;
//# sourceMappingURL=getCustomDimensions.js.map
