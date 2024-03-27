'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.generateVotingPDFChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.generateVotingPDFChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GENERATE_VOTING_PDF_CHANNEL
);
//# sourceMappingURL=generateVotingPDFChannel.js.map
