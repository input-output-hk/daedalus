// @flow
import type { BrowserWindow } from 'electron';
import compressLogsApi from './compress-logs';
import downloadLogsApi from './download-logs';
import getLogsApi from './get-logs';
import parseRedemptionCodeApi from './parse-redemption-code-from-pdf';
import resizeWindowApi from './resize-window';
import loadAsset from './load-asset';
import getGpuStatus from './get-gpu-status';
import getAppEnvironment from './get-app-environment';

export default (window: BrowserWindow) => {
  compressLogsApi();
  downloadLogsApi();
  getLogsApi();
  parseRedemptionCodeApi();
  resizeWindowApi(window);
  loadAsset();
  getGpuStatus();
  getAppEnvironment();
};
