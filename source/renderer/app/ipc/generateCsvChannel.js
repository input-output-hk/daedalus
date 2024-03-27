'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.generateCsvChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.generateCsvChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GENERATE_CSV_CHANNEL
);
//# sourceMappingURL=generateCsvChannel.js.map
