import { BrowserWindow } from 'electron';
import compressLogsApi from './compress-logs';
import { getCachedBackendStatusChannel } from './backendStatusChannel';
import { mithrilCommandChannel } from './mithrilCommandChannel';
import { handleChainStorageRequests } from './chainStorageChannel';
import { backendLifecycle } from '../BackendLifecycle';
import downloadLogsApi from './download-logs';
import { handleElectronStoreChannel } from './electronStoreConversation';
import getLogsApi from './get-logs';
import resizeWindowApi from './resize-window';
import loadAsset from './load-asset';
import getGpuStatus from './get-gpu-status';
import { downloadManagerChannel } from './downloadManagerChannel';
import getRecoveryWalletIdChannel from './getRecoveryWalletIdChannel';
import { handleHardwareWalletRequests } from './getHardwareWalletChannel';
import { handleBugReportRequests } from './bugReportRequestChannel';
import { handleFileMetaRequests } from './generateFileMetaChannel';
import { handlePaperWalletRequests } from './generatePaperWalletChannel';
import { handleAddressPDFRequests } from './generateAddressPDFChannel';
import { handleVotingPDFRequests } from './generateVotingPDFChannel';
import { saveQRCodeImageRequests } from './saveQRCodeImageChannel';
import { handleRewardsCsvRequests } from './generateCsvChannel';
import { handleFileDialogRequests } from './show-file-dialog-channels';
import { handleAddressIntrospectionRequests } from './introspect-address';
import { handleManageAppUpdateRequests } from './manageAppUpdateChannel';
import { openExternalUrlChannel } from './open-external-url';
import { openLocalDirectoryChannel } from './open-local-directory';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { createChannels } from './createHardwareWalletIPCChannels';
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
  handleVotingPDFRequests();
  saveQRCodeImageRequests();
  handleRewardsCsvRequests();
  handleFileDialogRequests(window);
  handleAddressIntrospectionRequests();
  handleManageAppUpdateRequests(window);
  // eslint-disable-next-line no-unused-expressions
  openExternalUrlChannel;
  // eslint-disable-next-line no-unused-expressions
  openLocalDirectoryChannel;
  downloadManagerChannel(window);
  getRecoveryWalletIdChannel();
  handleElectronStoreChannel();
  handleHardwareWalletRequests(window, createChannels(MainIpcChannel));

  // Watchdog IPC
  handleChainStorageRequests();
  getCachedBackendStatusChannel.onRequest(() =>
    Promise.resolve(
      backendLifecycle.getState() ?? {
        watchdogPid: 0,
        nodePid: 0,
        walletPid: 0,
        nodeStartedAt: null,
        walletStartedAt: null,
        walletRestartCount: 0,
        walletPort: null,
        hasChain: null,
        nodeStartupPhase: null,
        blockSyncProgress: {
          replayedBlock: 0,
          validatingChunk: 0,
          pushingLedger: 0,
        },
        mithrilPhase: null,
        mithrilProgress: null,
        mithrilSignificantlyBehind: null,
        lastError: null,
        walletUnrecoverable: false,
        nodeSocketWaitMs: null,
        walletReadyWaitMs: null,
        nodeForceKilled: false,
        lastWalletExitCode: null,
        lastWalletExitSignal: null,
        defaultChainPath: null,
        customChainPath: null,
      }
    )
  );
  mithrilCommandChannel.onReceive((cmd) => {
    backendLifecycle.sendMithrilCommand(cmd);
    return Promise.resolve();
  });
};
