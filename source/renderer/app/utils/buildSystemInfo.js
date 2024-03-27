'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.buildSystemInfo = void 0;
const formatters_1 = require('./formatters');
const buildSystemInfo = (environment, networkStatus) => ({
  platform: environment.os,
  platformVersion: environment.platformVersion,
  cpu: Array.isArray(environment.cpu) ? environment.cpu[0].model : '',
  ram: (0, formatters_1.formattedBytesToSize)(environment.ram),
  hasMetHardwareRequirements: environment.hasMetHardwareRequirements,
  availableDiskSpace: networkStatus.diskSpaceAvailable,
  isRTSFlagsModeEnabled: networkStatus.isRTSFlagsModeEnabled,
});
exports.buildSystemInfo = buildSystemInfo;
//# sourceMappingURL=buildSystemInfo.js.map
