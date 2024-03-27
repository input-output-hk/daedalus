'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const compress_logs_1 = __importDefault(require('./compress-logs'));
const download_logs_1 = __importDefault(require('./download-logs'));
const electronStoreConversation_1 = require('./electronStoreConversation');
const get_logs_1 = __importDefault(require('./get-logs'));
const resize_window_1 = __importDefault(require('./resize-window'));
const load_asset_1 = __importDefault(require('./load-asset'));
const get_gpu_status_1 = __importDefault(require('./get-gpu-status'));
const downloadManagerChannel_1 = require('./downloadManagerChannel');
const getRecoveryWalletIdChannel_1 = __importDefault(
  require('./getRecoveryWalletIdChannel')
);
const getHardwareWalletChannel_1 = require('./getHardwareWalletChannel');
const bugReportRequestChannel_1 = require('./bugReportRequestChannel');
const generateFileMetaChannel_1 = require('./generateFileMetaChannel');
const generatePaperWalletChannel_1 = require('./generatePaperWalletChannel');
const generateAddressPDFChannel_1 = require('./generateAddressPDFChannel');
const generateVotingPDFChannel_1 = require('./generateVotingPDFChannel');
const saveQRCodeImageChannel_1 = require('./saveQRCodeImageChannel');
const generateCsvChannel_1 = require('./generateCsvChannel');
const show_file_dialog_channels_1 = require('./show-file-dialog-channels');
const introspect_address_1 = require('./introspect-address');
const manageAppUpdateChannel_1 = require('./manageAppUpdateChannel');
const open_external_url_1 = require('./open-external-url');
const open_local_directory_1 = require('./open-local-directory');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const createHardwareWalletIPCChannels_1 = require('./createHardwareWalletIPCChannels');
exports.default = (window) => {
  (0, compress_logs_1.default)();
  (0, download_logs_1.default)();
  (0, get_logs_1.default)();
  (0, resize_window_1.default)(window);
  (0, load_asset_1.default)();
  (0, get_gpu_status_1.default)();
  (0, bugReportRequestChannel_1.handleBugReportRequests)();
  (0, generateFileMetaChannel_1.handleFileMetaRequests)();
  (0, generatePaperWalletChannel_1.handlePaperWalletRequests)();
  (0, generateAddressPDFChannel_1.handleAddressPDFRequests)();
  (0, generateVotingPDFChannel_1.handleVotingPDFRequests)();
  (0, saveQRCodeImageChannel_1.saveQRCodeImageRequests)();
  (0, generateCsvChannel_1.handleRewardsCsvRequests)();
  (0, show_file_dialog_channels_1.handleFileDialogRequests)(window);
  (0, introspect_address_1.handleAddressIntrospectionRequests)();
  (0, manageAppUpdateChannel_1.handleManageAppUpdateRequests)(window);
  // eslint-disable-next-line no-unused-expressions
  open_external_url_1.openExternalUrlChannel;
  // eslint-disable-next-line no-unused-expressions
  open_local_directory_1.openLocalDirectoryChannel;
  (0, downloadManagerChannel_1.downloadManagerChannel)(window);
  (0, getRecoveryWalletIdChannel_1.default)();
  (0, electronStoreConversation_1.handleElectronStoreChannel)();
  (0, getHardwareWalletChannel_1.handleHardwareWalletRequests)(
    window,
    (0, createHardwareWalletIPCChannels_1.createChannels)(
      MainIpcChannel_1.MainIpcChannel
    )
  );
};
//# sourceMappingURL=index.js.map
