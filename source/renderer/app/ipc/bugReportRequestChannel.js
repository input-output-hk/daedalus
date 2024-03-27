'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.bugReportRequestChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.bugReportRequestChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.SUBMIT_BUG_REPORT_REQUEST_CHANNEL
);
//# sourceMappingURL=bugReportRequestChannel.js.map
