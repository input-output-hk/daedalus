'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.generateWalletMigrationReportChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
exports.generateWalletMigrationReportChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GENERATE_WALLET_MIGRATION_REPORT_CHANNEL
);
//# sourceMappingURL=generateWalletMigrationReportChannel.js.map
