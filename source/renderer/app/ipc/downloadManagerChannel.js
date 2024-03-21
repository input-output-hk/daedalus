'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.checkFileExistsChannel = exports.clearDownloadLocalDataChannel = exports.getDownloadsLocalDataChannel = exports.getDownloadLocalDataChannel = exports.deleteDownloadedFile = exports.requestResumeDownloadChannel = exports.requestDownloadChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.requestDownloadChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.REQUEST_DOWNLOAD
);
exports.requestResumeDownloadChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.RESUME_DOWNLOAD
);
exports.deleteDownloadedFile = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.DELETE_DOWNLOADED_FILE
);
exports.getDownloadLocalDataChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_DOWNLOAD_LOCAL_DATA
);
exports.getDownloadsLocalDataChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_DOWNLOADS_LOCAL_DATA
);
exports.clearDownloadLocalDataChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.CLEAR_DOWNLOAD_LOCAL_DATA
);
exports.checkFileExistsChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.CHECK_FILE_EXISTS
);
//# sourceMappingURL=downloadManagerChannel.js.map
