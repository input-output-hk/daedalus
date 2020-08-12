// @flow
import type { BrowserWindow } from 'electron';
import compressLogsApi from './compress-logs';
import downloadLogsApi from './download-logs';
import { handleElectronStoreChannel } from './electronStoreConversation';
import getLogsApi from './get-logs';
import resizeWindowApi from './resize-window';
import loadAsset from './load-asset';
import getGpuStatus from './get-gpu-status';
import { downloadManagerChannel } from './downloadManagerChannel';
import getRecoveryWalletIdChannel from './getRecoveryWalletIdChannel';
import { handleBugReportRequests } from './bugReportRequestChannel';
import { handleFileMetaRequests } from './generateFileMetaChannel';
import { handlePaperWalletRequests } from './generatePaperWalletChannel';
import { handleAddressPDFRequests } from './generateAddressPDFChannel';
import { handleRewardsCsvRequests } from './generateRewardsCsvChannel';
import { handleFileDialogRequests } from './show-file-dialog-channels';
import { handleAddressIntrospectionRequests } from './introspect-address';
import { openExternalUrlChannel } from './open-external-url';
import { openLocalDirectoryChannel } from './open-local-directory';

export default (window: BrowserWindow) => {
  compressLogsApi();
  downloadLogsApi();
  getLogsApi();
  resizeWindowApi(window);
  loadAsset();
  getGpuStatus();
  handleBugReportRequests();
  handleFileMetaRequests();
  handlePaperWalletRequests();
  handleAddressPDFRequests();
  handleRewardsCsvRequests();
  handleFileDialogRequests(window);
  handleAddressIntrospectionRequests();
  // eslint-disable-next-line no-unused-expressions
  openExternalUrlChannel;
  // eslint-disable-next-line no-unused-expressions
  openLocalDirectoryChannel;
  downloadManagerChannel(window);
  getRecoveryWalletIdChannel();
  handleElectronStoreChannel();
};
