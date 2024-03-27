'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.USES_HARDWARE_WALLET_DIMENSION_KEY = exports.USES_LEGACY_WALLET_DIMENSION_KEY = exports.VERSION_DIMENSION_KEY = exports.OS_DIMENSION_KEY = exports.RAM_DIMENSION_KEY = exports.CPU_DIMENSION_KEY = exports.NETWORK_TO_ANALYTICS_SITE_ID_MAP = exports.DEV_MODE_SITE_MAP_ID = exports.PRIVACY_POLICY_LINK = exports.ANALYTICS_API_ENDPOINT = void 0;
exports.ANALYTICS_API_ENDPOINT = 'https://matomo.cw.iog.io/matomo.php';
exports.PRIVACY_POLICY_LINK =
  'https://static.iohk.io/terms/iog-privacy-policy.pdf';
// ID used when Daedalus is launched from nix-shell
exports.DEV_MODE_SITE_MAP_ID = 11;
// IDs used when Daedalus is launched as a binary (installed with installer)
exports.NETWORK_TO_ANALYTICS_SITE_ID_MAP = {
  mainnet: 2,
  mainnet_flight: 12,
  testnet: 3,
  preprod: 9,
  preview: 10,
  staging: 4,
  shelley_qa: 6,
  alonzo_purple: 7,
  selfnode: 11,
  development: 11,
  vasil_dev: 8,
};
exports.CPU_DIMENSION_KEY = 'dimension1';
exports.RAM_DIMENSION_KEY = 'dimension2';
exports.OS_DIMENSION_KEY = 'dimension3';
exports.VERSION_DIMENSION_KEY = 'dimension4';
exports.USES_LEGACY_WALLET_DIMENSION_KEY = 'dimension5';
exports.USES_HARDWARE_WALLET_DIMENSION_KEY = 'dimension6';
//# sourceMappingURL=analyticsConfig.js.map
