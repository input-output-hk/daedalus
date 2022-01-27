import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { GENERATE_WALLET_MIGRATION_REPORT_CHANNEL } from '../../../common/ipc/api';
import type {
  GenerateWalletMigrationReportRendererRequest,
  GenerateWalletMigrationReportMainResponse,
} from '../../../common/ipc/api';

export const generateWalletMigrationReportChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GenerateWalletMigrationReportMainResponse,
  GenerateWalletMigrationReportRendererRequest
> = new RendererIpcChannel(GENERATE_WALLET_MIGRATION_REPORT_CHANNEL);
