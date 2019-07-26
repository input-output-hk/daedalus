// @flow
import type { BrowserWindow } from 'electron';
import compressLogsApi from './compress-logs';
import downloadLogsApi from './download-logs';
import getLogsApi from './get-logs';
import resizeWindowApi from './resize-window';
import loadAsset from './load-asset';
import getGpuStatus from './get-gpu-status';
import { handleBugReportRequests } from './bugReportRequestChannel';
import { handleFileContentRequests } from './generateFileContentChannel';
import { handlePaperWalletRequests } from './generatePaperWalletChannel';
import { openExternalUrlChannel } from './open-external-url';
import { openLocalDirectoryChannel } from './open-local-directory';
import { setupParseRedemptionCodeHandler } from './parse-redemption-code';

export default (window: BrowserWindow) => {
  compressLogsApi();
  downloadLogsApi();
  getLogsApi();
  resizeWindowApi(window);
  loadAsset();
  getGpuStatus();
  handleBugReportRequests();
  handleFileContentRequests();
  handlePaperWalletRequests();
  // eslint-disable-next-line no-unused-expressions
  openExternalUrlChannel;
  // eslint-disable-next-line no-unused-expressions
  openLocalDirectoryChannel;
  setupParseRedemptionCodeHandler();
};
