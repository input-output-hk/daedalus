'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.generateWalletMigrationReportChannel = void 0;
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
exports.generateWalletMigrationReportChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GENERATE_WALLET_MIGRATION_REPORT_CHANNEL
);
//# sourceMappingURL=generateWalletMigrationReportChannel.js.map
