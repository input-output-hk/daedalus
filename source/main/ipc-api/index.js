// @flow
import compressLogsApi from './compress-logs';
import downloadLogsApi from './download-logs';
import getLogsApi from './get-logs';
import parseRedemptionCodeApi from './parse-redemption-code-from-pdf';
import resizeWindowApi from './resize-window';
import killProcess from './kill-process';
import loadAsset from './load-asset';
import getGpuStatus from './get-gpu-status';

export default (params: any) => {
  compressLogsApi();
  downloadLogsApi();
  getLogsApi();
  parseRedemptionCodeApi();
  resizeWindowApi(params);
  killProcess();
  loadAsset();
  getGpuStatus();
};
