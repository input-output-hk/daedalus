// @flow
import { action, observable } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { exportWalletsChannel } from '../ipc/cardano.ipc';
import { logger } from '../utils/logging';
import type { ExportWalletsMainResponse } from '../../../common/ipc/api';

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
    const { isMainnet, isDev, isTest } = this.environment;
    if (isMainnet || isDev || isTest) {
      const walletMigrationStatus = await this.getWalletMigrationStatusRequest.execute()
        .promise;
      if (walletMigrationStatus === WalletMigrationStatuses.UNSTARTED) {
        logger.info('WalletMigrationStore: Starting wallet migration...');
        await this.setWalletMigrationStatusRequest.execute(
          WalletMigrationStatuses.RUNNING
        );

        // Trigger wallet export
        logger.info('WalletMigrationStore: Starting wallet export...');
        try {
          const {
            data: exportedWallets,
            errors,
          }: ExportWalletsMainResponse = await exportWalletsChannel.request();
          logger.info('WalletMigrationStore: Wallet export SUCCESS', {
            exportedWalletsCount: exportedWallets.length,
            errors,
          });

          // Trigger wallet restoration
          if (exportedWallets.length) {
            logger.info(
              `WalletMigrationStore: Restoring ${exportedWallets.length} exported wallets`
            );

            exportedWallets.forEach((wallet, index) => {
              const {
                encrypted_root_private_key, // eslint-disable-line
                name,
                passphrase_hash, // eslint-disable-line
              } = wallet;
              logger.info(
                `WalletMigrationStore: Restoring wallet ${index + 1}`,
                {
                  encrypted_root_private_key,
                  name,
                  passphrase_hash,
                }
              );
            });

            logger.info('WalletMigrationStore: Wallet restoration SUCCESS');
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
        } finally {
          // Generate and store migration report
          logger.info('WalletMigrationStore: Generating migration report...');
        }
      } else {
        logger.info('WalletMigrationStore: Skipping wallet migration', {
          walletMigrationStatus,
        });
      }
    }
  };
}
