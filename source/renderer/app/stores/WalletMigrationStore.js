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
import type {
  WalletMigrationReportData,
  ExportedWalletData,
  RestoredWalletData,
} from '../../../common/types/logging.types';
import type {
  ExportedByronWallet,
  WalletImportStatus,
} from '../types/walletExportTypes';
import { WalletImportStatuses } from '../types/walletExportTypes';

export type WalletMigrationStatus =
  | 'unstarted'
  | 'running'
  | 'completed'
  | 'skipped'
  | 'errored';

export const WalletMigrationStatuses: {
  UNSTARTED: WalletMigrationStatus,
  RUNNING: WalletMigrationStatus,
  COMPLETED: WalletMigrationStatus,
  SKIPPED: WalletMigrationStatus,
  ERRORED: WalletMigrationStatus,
} = {
  UNSTARTED: 'unstarted',
  RUNNING: 'running',
  COMPLETED: 'completed',
  SKIPPED: 'skipped',
  ERRORED: 'errored',
};

export default class WalletMigrationStore extends Store {
  @observable exportedWallets: Array<ExportedByronWallet> = [];
  @observable exportErrors: string = '';
  @observable restoredWallets: Array<Wallet> = [];
  @observable restorationErrors: Array<{
    error: LocalizableError,
    wallet: ExportedWalletData,
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

  getExportedWalletById = (id: string): ?ExportedByronWallet =>
    this.exportedWallets.find(w => w.id === id);

  @action toggleWalletImportPendingState = (id: string) => {
    const wallet = this.getExportedWalletById(id);
    if (wallet) {
      const { status } = wallet.import;
      const isPending = status === WalletImportStatuses.PENDING;
      this.updateWalletImportStatus(
        id,
        isPending
          ? WalletImportStatuses.UNSTARTED
          : WalletImportStatuses.PENDING
      );
    }
  };

  @action updateWalletImportStatus = (
    id: string,
    status: WalletImportStatus,
    error?: LocalizableError
  ) => {
    const wallet = this.getExportedWalletById(id);
    if (wallet) {
      wallet.import.status = status;
      wallet.import.error = error || null;
    }
  };

  @action exportWallets = async () => {
    logger.debug('WalletMigrationStore: Starting wallet export...');
    const {
      wallets,
      errors,
    }: ExportWalletsMainResponse = await exportWalletsChannel.request();
    runInAction('update exportedWallets and exportErrors', () => {
      this.exportedWallets = wallets.map(wallet => {
        const isImported =
          typeof this.stores.wallets.getWalletById(`legacy_${wallet.id}`) !==
          'undefined';
        const status = isImported
          ? WalletImportStatuses.COMPLETED
          : WalletImportStatuses.UNSTARTED;
        return { ...wallet, import: { status, error: null } };
      });
      this.exportErrors = errors;
    });
    logger.debug(
      `WalletMigrationStore: Exported ${this.exportedWalletsCount} wallets`,
      {
        exportedWalletsData: this.exportedWalletsData,
        exportErrors: this.exportErrors,
      }
    );
  };

  @action restoreWallets = async () => {
    logger.debug(
      `WalletMigrationStore: Restoring ${this.exportedWalletsCount} exported wallets...`
    );
    await Promise.all(
      this.exportedWallets.map((wallet, index) => {
        logger.debug(
          `WalletMigrationStore: Restoring ${index + 1}. wallet...`,
          {
            id: wallet.id,
            name: wallet.name,
            hasPassword: wallet.is_passphrase_empty,
          }
        );
        return this._restoreWallet(wallet);
      })
    );
    logger.debug(
      `WalletMigrationStore: Restored ${this.restoredWalletsCount} of ${this.exportedWalletsCount} exported wallets`
    );
  };

  _restoreWallet = async (exportedWallet: ExportedByronWallet) => {
    // Reset restore requests to clear previous errors
    this.restoreExportedWalletRequest.reset();

    const { id } = exportedWallet;
    this.updateWalletImportStatus(id, WalletImportStatuses.RUNNING);
    try {
      const restoredWallet = await this.restoreExportedWalletRequest.execute(
        exportedWallet
      ).promise;
      if (!restoredWallet)
        throw new Error('Restored wallet was not received correctly');

      runInAction('update restoredWallets', () => {
        this.updateWalletImportStatus(id, WalletImportStatuses.COMPLETED);
        this.restoredWallets.push(restoredWallet);
      });
    } catch (error) {
      runInAction('update restorationErrors', () => {
        const { name, is_passphrase_empty: hasPassword } = exportedWallet;
        this.updateWalletImportStatus(id, WalletImportStatuses.ERRORED, error);
        this.restorationErrors.push({
          error,
          wallet: { id, name, hasPassword },
        });
      });
    }
  };

  @action generateMigrationReport = async () => {
    const finalMigrationStatus = await this.getWalletMigrationStatusRequest.execute()
      .promise;
    const walletMigrationReportData: WalletMigrationReportData = {
      exportedWalletsData: this.exportedWalletsData,
      exportedWalletsCount: this.exportedWalletsCount,
      exportErrors: this.exportErrors,
      restoredWalletsData: this.restoredWalletsData,
      restoredWalletsCount: this.restoredWalletsCount,
      restorationErrors: this.restorationErrors,
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
      logger.debug('WalletMigrationStore: Generated wallet migration report');
    } catch (error) {
      logger.error(
        'WalletMigrationStore: Wallet migration report generation failed',
        {
          error,
        }
      );
    }
  };

  @action startMigration = async () => {
    const { isMainnet, isTestnet, isDev } = this.environment;
    if (isMainnet || isTestnet) {
      // Reset store values
      this.resetMigration();

      const walletMigrationStatus = await this.getWalletMigrationStatusRequest.execute()
        .promise;
      if (
        walletMigrationStatus === WalletMigrationStatuses.UNSTARTED ||
        isDev
      ) {
        // TODO: remove "isDev"
        // Update migration status to "RUNNING"
        logger.debug('WalletMigrationStore: Starting wallet migration...');
        await this.setWalletMigrationStatusRequest.execute(
          WalletMigrationStatuses.RUNNING
        );

        // Trigger wallet export
        await this.exportWallets();

        // Trigger wallet restoration
        if (this.exportedWalletsCount) {
          await this.restoreWallets();
        }

        // Update migration status
        if (
          this.exportedWalletsCount
            ? this.exportedWalletsCount === this.restoredWalletsCount
            : this.exportErrors === ''
        ) {
          logger.debug('WalletMigrationStore: Wallet migration succeeded');
          await this.setWalletMigrationStatusRequest.execute(
            WalletMigrationStatuses.COMPLETED
          );
        } else {
          logger.debug('WalletMigrationStore: Wallet migration failed', {
            exportErrors: this.exportErrors,
            restorationErrors: this.restorationErrors,
          });
          await this.setWalletMigrationStatusRequest.execute(
            WalletMigrationStatuses.ERRORED
          );
        }

        // Generate and store migration report
        await this.generateMigrationReport();
      } else {
        logger.debug('WalletMigrationStore: Skipping wallet migration...', {
          walletMigrationStatus,
        });
      }
    } else {
      // Update migration status to "SKIPPED"
      await this.setWalletMigrationStatusRequest.execute(
        WalletMigrationStatuses.SKIPPED
      );
    }
  };

  @action resetMigration = () => {
    this.exportedWallets = [];
    this.exportErrors = '';
    this.restoredWallets = [];
    this.restorationErrors = [];
  };

  @computed get exportedWalletsData(): Array<ExportedWalletData> {
    return this.exportedWallets.map(wallet => ({
      id: wallet.id,
      name: wallet.name,
      hasPassword: wallet.is_passphrase_empty,
      import: wallet.import,
    }));
  }

  @computed get exportedWalletsCount(): number {
    return this.exportedWallets.length;
  }

  @computed get restoredWalletsData(): Array<RestoredWalletData> {
    return this.restoredWallets.map(wallet => ({
      id: getRawWalletId(wallet.id),
      name: wallet.name,
      hasPassword: wallet.hasPassword,
    }));
  }

  @computed get restoredWalletsCount(): number {
    return this.restoredWallets.length;
  }
}
