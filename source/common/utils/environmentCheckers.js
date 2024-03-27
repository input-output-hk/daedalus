'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.checkIsLinux = exports.checkIsWindows = exports.checkIsMacOS = exports.checkIsDevelopment = exports.checkIsSelfnode = exports.checkIsStaging = exports.checkIsShelleyQA = exports.checkIsPreview = exports.checkIsPreprod = exports.checkIsVasilDev = exports.checkIsAlonzoPurple = exports.checkIsTestnet = exports.checkIsMainnet = exports.checkIsProduction = exports.checkIsTest = exports.checkIsDev = exports.getBuildLabel = exports.evaluateNetwork = void 0;
const lodash_1 = require('lodash');
const environment_types_1 = require('../types/environment.types');
/* ==================================================================
=                    Static checks and generators                   =
================================================================== */
const evaluateNetwork = (network) => {
  let currentNetwork = network || environment_types_1.DEVELOPMENT;
  if (network === environment_types_1.MAINNET_FLIGHT) {
    currentNetwork = environment_types_1.MAINNET;
  }
  if (network === 'alonzo-purple') {
    currentNetwork = environment_types_1.ALONZO_PURPLE;
  }
  if (network === 'vasil-dev') {
    currentNetwork = environment_types_1.VASIL_DEV;
  }
  if (network === 'pre-prod') {
    currentNetwork = environment_types_1.PREPROD;
  }
  if (network === 'preview') {
    currentNetwork = environment_types_1.PREVIEW;
  }
  return currentNetwork;
};
exports.evaluateNetwork = evaluateNetwork;
const getBuildLabel = (build, network, currentNodeEnv, isFlight, version) => {
  const networkLabel = isFlight
    ? 'Flight'
    : environment_types_1.networkPrettyNames[network];
  let buildLabel = `Daedalus ${networkLabel} (${version}#${build})`;
  if (!(0, exports.checkIsProduction)(currentNodeEnv))
    buildLabel += ` ${(0, lodash_1.upperFirst)(currentNodeEnv)}`;
  return buildLabel;
};
exports.getBuildLabel = getBuildLabel;
const checkIsDev = (currentNodeEnv) =>
  currentNodeEnv === environment_types_1.DEVELOPMENT;
exports.checkIsDev = checkIsDev;
const checkIsTest = (currentNodeEnv) =>
  currentNodeEnv === environment_types_1.TEST;
exports.checkIsTest = checkIsTest;
const checkIsProduction = (currentNodeEnv) =>
  currentNodeEnv === environment_types_1.PRODUCTION;
exports.checkIsProduction = checkIsProduction;
const checkIsMainnet = (network) => network === environment_types_1.MAINNET;
exports.checkIsMainnet = checkIsMainnet;
const checkIsTestnet = (network) => network === environment_types_1.TESTNET;
exports.checkIsTestnet = checkIsTestnet;
const checkIsAlonzoPurple = (network) =>
  network === environment_types_1.ALONZO_PURPLE;
exports.checkIsAlonzoPurple = checkIsAlonzoPurple;
const checkIsVasilDev = (network) => network === environment_types_1.VASIL_DEV;
exports.checkIsVasilDev = checkIsVasilDev;
const checkIsPreprod = (network) => network === environment_types_1.PREPROD;
exports.checkIsPreprod = checkIsPreprod;
const checkIsPreview = (network) => network === environment_types_1.PREVIEW;
exports.checkIsPreview = checkIsPreview;
const checkIsShelleyQA = (network) =>
  network === environment_types_1.SHELLEY_QA;
exports.checkIsShelleyQA = checkIsShelleyQA;
const checkIsStaging = (network) => network === environment_types_1.STAGING;
exports.checkIsStaging = checkIsStaging;
const checkIsSelfnode = (network) => network === environment_types_1.SELFNODE;
exports.checkIsSelfnode = checkIsSelfnode;
const checkIsDevelopment = (network) =>
  network === environment_types_1.DEVELOPMENT;
exports.checkIsDevelopment = checkIsDevelopment;
const checkIsMacOS = (platform) => platform === environment_types_1.MAC_OS;
exports.checkIsMacOS = checkIsMacOS;
const checkIsWindows = (platform) => platform === environment_types_1.WINDOWS;
exports.checkIsWindows = checkIsWindows;
const checkIsLinux = (platform) => platform === environment_types_1.LINUX;
exports.checkIsLinux = checkIsLinux;
//# sourceMappingURL=environmentCheckers.js.map
