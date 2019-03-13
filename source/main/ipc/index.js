// @flow
import type { BrowserWindow } from 'electron';
import compressLogsApi from './compress-logs';
import downloadLogsApi from './download-logs';
import getLogsApi from './get-logs';
import parseRedemptionCodeApi from './parse-redemption-code-from-pdf';
import resizeWindowApi from './resize-window';
import loadAsset from './load-asset';
import getGpuStatus from './get-gpu-status';
import { handleExtractWalletsRequests } from './extractWalletsChannel';
import { handleMatchWalletsPasswordsRequests } from './matchWalletsPasswordsChannel';
import { handleReportRequests } from './reportRequestChannel';
import { handlePaperWalletRequests } from './generatePaperWalletChannel';
import { openExternalUrlChannel } from './open-external-url';

export default (window: BrowserWindow) => {
  compressLogsApi();
  downloadLogsApi();
  getLogsApi();
  parseRedemptionCodeApi();
  resizeWindowApi(window);
  loadAsset();
  getGpuStatus();
  handleExtractWalletsRequests();
  handleMatchWalletsPasswordsRequests();
  handleReportRequests();
  handlePaperWalletRequests();
  openExternalUrlChannel;
};
