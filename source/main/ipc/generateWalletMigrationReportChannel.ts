import { MainIpcChannel } from './lib/MainIpcChannel';
import { GENERATE_WALLET_MIGRATION_REPORT_CHANNEL } from '../../common/ipc/api';
import type {
  GenerateWalletMigrationReportRendererRequest,
  GenerateWalletMigrationReportMainResponse,
} from '../../common/ipc/api';

export const generateWalletMigrationReportChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  GenerateWalletMigrationReportRendererRequest,
  GenerateWalletMigrationReportMainResponse
> = new MainIpcChannel(GENERATE_WALLET_MIGRATION_REPORT_CHANNEL);
