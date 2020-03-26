// @flow
import { action, computed, observable, runInAction } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Wallet from '../domains/Wallet';
import LocalizableError from '../i18n/LocalizableError';
import { exportWalletsChannel } from '../ipc/cardano.ipc';
import { generateWalletMigrationReportChannel } from '../ipc/generateWalletMigrationReportChannel';
import { logger } from '../utils/logging';
import { getRawWalletId } from '../api/utils';
import type { ExportWalletsMainResponse } from '../../../common/ipc/api';
import type { WalletMigrationReportData } from '../../../common/types/logging.types';
import type { ExportedByronWallet } from '../types/walletExportTypes';

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
  _restoredWallets: Array<Wallet> = [];
  _restorationErrors: Array<{
    error: LocalizableError,
    wallet: { name: string, hasPassword: boolean },
  }> = [];

  @observable
  getWalletMigrationStatusRequest: Request<WalletMigrationStatus> = new Request(
    this.api.localStorage.getWalletMigrationStatus
  );

  @observable
  setWalletMigrationStatusRequest: Request<void> = new Request(
    this.api.localStorage.setWalletMigrationStatus
  );

  @observable restoreExportedWalletRequest: Request<Wallet> = new Request(
    this.api.ada.restoreExportedByronWallet
  );

  _restoreExportedWallet = async (exportedWallet: ExportedByronWallet) => {
    // Reset restore requests to clear previous errors
    this.restoreExportedWalletRequest.reset();

    try {
      const restoredWallet = await this.restoreExportedWalletRequest.execute(
        exportedWallet
      ).promise;
      if (!restoredWallet)
        throw new Error('Restored wallet was not received correctly');

      runInAction('update restoredWallets', () => {
        this._restoredWallets.push(restoredWallet);
      });
    } catch (error) {
      const { name, passphrase_hash: passphraseHash } = exportedWallet;
      const hasPassword = passphraseHash !== null;
      this._restorationErrors.push({ error, wallet: { name, hasPassword } });
    }
  };

  @action startMigration = async () => {
    const { isMainnet, isTestnet } = this.environment;
    if (isMainnet || isTestnet) {
      // Reset store values
      this._restoredWallets = [];
      this._restorationErrors = [];

      const walletMigrationStatus = await this.getWalletMigrationStatusRequest.execute()
        .promise;
      if (walletMigrationStatus === WalletMigrationStatuses.UNSTARTED) {
        logger.debug('WalletMigrationStore: Starting wallet migration...');
        await this.setWalletMigrationStatusRequest.execute(
          WalletMigrationStatuses.RUNNING
        );

        // Trigger wallet export
        logger.debug('WalletMigrationStore: Starting wallet export...');
        const {
          wallets: exportedWallets,
          errors: exportErrors,
        }: ExportWalletsMainResponse = await exportWalletsChannel.request();
        const exportedWalletsData = exportedWallets.map(w => ({
          name: w.name,
          hasPassword: w.passphrase_hash !== null,
        }));
        const exportedWalletsCount = exportedWallets.length;

        logger.debug(
          `WalletMigrationStore: Exported ${exportedWalletsCount} wallets`,
          {
            exportedWalletsData,
            exportErrors,
          }
        );

        // Trigger wallet restoration
        if (exportedWalletsCount) {
          logger.debug(
            `WalletMigrationStore: Restoring ${exportedWalletsCount} exported wallets...`
          );

          await Promise.all(
            exportedWallets.map((wallet, index) => {
              logger.debug(
                `WalletMigrationStore: Restoring ${index + 1}. wallet...`,
                {
                  name: wallet.name,
                  hasPassword: wallet.passphrase_hash !== null, // eslint-disable-line
                }
              );
              return this._restoreExportedWallet(wallet);
            })
          );

          logger.debug(
            `WalletMigrationStore: Restored ${this._restoredWalletsCount} of ${exportedWalletsCount} exported wallets`
          );
        }

        if (
          exportedWalletsCount
            ? exportedWalletsCount === this._restoredWalletsCount
            : exportErrors === ''
        ) {
          await this.setWalletMigrationStatusRequest.execute(
            WalletMigrationStatuses.COMPLETED
          );
        } else {
          logger.debug('WalletMigrationStore: Wallet migration failed', {
            exportErrors,
            restorationErrors: this._restorationErrors,
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
          restoredWalletsData: this._restoredWallets.map(wallet => {
            const { id, name, hasPassword } = wallet;
            return { id: getRawWalletId(id), name, hasPassword };
          }),
          restoredWalletsCount: this._restoredWalletsCount,
          restorationErrors: this._restorationErrors,
          finalMigrationStatus,
        };
        logger.debug(
          'WalletMigrationStore: Generating wallet migration report...',
          {
            walletMigrationReportData,
          }
        );
        try {
          await generateWalletMigrationReportChannel.send(
            walletMigrationReportData
          );
          logger.debug(
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
        logger.debug('WalletMigrationStore: Skipping wallet migration...', {
          walletMigrationStatus,
        });
      }
    }
  };

  @computed get _restoredWalletsCount(): number {
    return this._restoredWallets.length;
  }
}
