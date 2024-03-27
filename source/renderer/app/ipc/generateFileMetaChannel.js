'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.generateFileMetaChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.generateFileMetaChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GENERATE_FILE_META_CHANNEL
);
//# sourceMappingURL=generateFileMetaChannel.js.map
