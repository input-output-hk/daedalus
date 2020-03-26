// @flow
import { action, observable } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { exportWalletsChannel } from '../ipc/cardano.ipc';
import { generateWalletMigrationReportChannel } from '../ipc/generateWalletMigrationReportChannel';
import { logger } from '../utils/logging';
import type { ExportWalletsMainResponse } from '../../../common/ipc/api';
import type { WalletMigrationReportData } from '../../../common/types/logging.types';

export type WalletMigrationStatus =
  | 'unstarted'
  | 'running'
  | 'completed'
  | 'errored';

export const WalletMigrationStatuses: {
  UNSTARTED: WalletMigrationStatus,
  RUNNING: WalletMigrationStatus,
  COMPLETED: WalletMigrationStatus,
  ERRORED: WalletMigrationStatus,
} = {
  UNSTARTED: 'unstarted',
  RUNNING: 'running',
  COMPLETED: 'completed',
  ERRORED: 'errored',
};

export default class WalletMigrationStore extends Store {
  @observable
  getWalletMigrationStatusRequest: Request<WalletMigrationStatus> = new Request(
    this.api.localStorage.getWalletMigrationStatus
  );

  @observable
  setWalletMigrationStatusRequest: Request<void> = new Request(
    this.api.localStorage.setWalletMigrationStatus
  );

  setup() {
    // TODO: Remove after feature development is completed
    this.api.localStorage.unsetWalletMigrationStatus();
  }

  @action initMigration = async () => {
    const { isIncentivizedTestnet } = global;
    if (!isIncentivizedTestnet) {
      const walletMigrationStatus = await this.getWalletMigrationStatusRequest.execute()
        .promise;
      if (walletMigrationStatus === WalletMigrationStatuses.UNSTARTED) {
        logger.info('WalletMigrationStore: Starting wallet migration...');
        await this.setWalletMigrationStatusRequest.execute(
          WalletMigrationStatuses.RUNNING
        );

        // Trigger wallet export
        logger.info('WalletMigrationStore: Starting wallet export...');
        const {
          wallets: exportedWallets,
          errors: exportErrors,
        }: ExportWalletsMainResponse = await exportWalletsChannel.request();
        const exportedWalletsData = exportedWallets.map(w => ({
          name: w.name,
          hasPassword: w.passphrase_hash !== null,
        }));
        const exportedWalletsCount = exportedWallets.length;

        logger.info(
          `WalletMigrationStore: Exported ${exportedWalletsCount} wallets`,
          {
            exportedWalletsData,
            exportErrors,
          }
        );

        let restoredWalletsCount = 0;
        const restorationErrors = [];
        try {
          // Trigger wallet restoration
          if (exportedWalletsCount) {
            logger.info(
              `WalletMigrationStore: Restoring ${exportedWalletsCount} exported wallets...`
            );

            exportedWallets.forEach((wallet, index) => {
              const {
                // encrypted_root_private_key, // eslint-disable-line
                name,
                passphrase_hash, // eslint-disable-line
              } = wallet;
              logger.info(
                `WalletMigrationStore: Restoring wallet ${index + 1}...`,
                {
                  name,
                  hasPassword: passphrase_hash !== null, // eslint-disable-line
                }
              );
              restoredWalletsCount++;
            });

            logger.info(
              `WalletMigrationStore: Restored ${restoredWalletsCount} of ${exportedWalletsCount} exported wallets`
            );
          }

          await this.setWalletMigrationStatusRequest.execute(
            WalletMigrationStatuses.COMPLETED
          );
        } catch (error) {
          logger.error('WalletMigrationStore: Wallet export FAILURE', {
            error,
          });
          await this.setWalletMigrationStatusRequest.execute(
            WalletMigrationStatuses.ERRORED
          );
        }

        // Generate and store migration report
        const finalMigrationStatus = await this.getWalletMigrationStatusRequest.execute()
          .promise;
        const walletMigrationReportData: WalletMigrationReportData = {
          exportedWalletsData,
          exportedWalletsCount,
          exportErrors,
          restoredWalletsCount,
          restorationErrors,
          finalMigrationStatus,
        };
        logger.info(
          'WalletMigrationStore: Generating wallet migration report...',
          {
            walletMigrationReportData,
          }
        );
        try {
          await generateWalletMigrationReportChannel.send(
            walletMigrationReportData
          );
          logger.info(
            'WalletMigrationStore: Generated wallet migration report'
          );
        } catch (error) {
          logger.error(
            'WalletMigrationStore: Wallet migration report generation failed',
            {
              error,
            }
          );
        }
      } else {
        logger.info('WalletMigrationStore: Skipping wallet migration...', {
          walletMigrationStatus,
        });
      }
    }
  };
}
