// @flow
import { action, computed, observable, runInAction } from 'mobx';
import path from 'path';
import { orderBy } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Wallet from '../domains/Wallet';
import LocalizableError from '../i18n/LocalizableError';
import { exportWalletsChannel } from '../ipc/cardano.ipc';
import { showOpenDialogChannel } from '../ipc/show-file-dialog-channels';
import { generateWalletMigrationReportChannel } from '../ipc/generateWalletMigrationReportChannel';
import { logger } from '../utils/logging';
import { getRawWalletId } from '../api/utils';
import WalletImportFileDialog from '../components/wallet/wallet-import/WalletImportFileDialog';
import type { ExportWalletsMainResponse } from '../../../common/ipc/api';
import type {
  WalletMigrationReportData,
  ExportedWalletData,
  RestoredWalletData,
} from '../../../common/types/logging.types';
import type {
  ExportedByronWallet,
  WalletImportStatus,
  ImportFromOption,
} from '../types/walletExportTypes';
import {
  WalletImportStatuses,
  ImportFromOptions,
} from '../types/walletExportTypes';
import { IMPORT_WALLET_STEPS } from '../config/walletRestoreConfig';
import type { ImportWalletStep } from '../types/walletRestoreTypes';

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
  @observable walletMigrationStep: ?ImportWalletStep = null;

  @observable isExportRunning = false;
  @observable exportedWallets: Array<ExportedByronWallet> = [];
  @observable exportErrors: string = '';
  @observable exportSourcePath: string = '';
  @observable defaultExportSourcePath: string = global.legacyStateDir;
  @observable isTestMigrationEnabled: boolean = false;

  @observable isRestorationRunning = false;
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

  setup() {
    const { walletMigration } = this.actions;
    walletMigration.initiateMigration.listen(this._initiateMigration);
    walletMigration.startMigration.listen(this._startMigration);
    walletMigration.finishMigration.listen(this._finishMigration);
    walletMigration.resetMigration.listen(this._resetMigration);
    walletMigration.toggleWalletImportSelection.listen(
      this._toggleWalletImportSelection
    );
    walletMigration.updateWalletName.listen(this._updateWalletName);
    walletMigration.nextStep.listen(this._nextStep);
    walletMigration.selectExportSourcePath.listen(this._selectExportSourcePath);
    walletMigration.resetExportSourcePath.listen(this._resetExportSourcePath);
  }

  getExportedWalletById = (id: string): ?ExportedByronWallet =>
    this.exportedWallets.find(w => w.id === id);

  getExportedWalletDuplicatesById = (
    id: string,
    index: number
  ): Array<ExportedByronWallet> =>
    this.exportedWallets.filter(w => w.id === id && w.index !== index);

  getExportedWalletByIndex = (index: number): ?ExportedByronWallet =>
    this.exportedWallets.find(w => w.index === index);

  @action _initiateMigration = () => {
    this.walletMigrationStep = IMPORT_WALLET_STEPS.WALLET_IMPORT_FILE;
  };

  @action _selectExportSourcePath = async ({
    importFrom,
  }: {
    importFrom: ImportFromOption,
  }) => {
    const params =
      importFrom === ImportFromOptions.STATE_DIR
        ? {
            defaultPath: this.defaultExportSourcePath,
            properties: ['openDirectory'],
          }
        : {
            defaultPath: path.join(
              this.stores.profile.desktopDirectoryPath,
              'secret.key'
            ),
            properties: ['openFile'],
            filters: [
              {
                name: 'secret',
                extensions: ['key'],
              },
            ],
          };
    const { filePaths } = await showOpenDialogChannel.send(params);
    if (!filePaths || filePaths.length === 0) {
      return;
    }
    const filePath = filePaths[0];
    runInAction('update exportSourcePath', () => {
      this.exportSourcePath = filePath;
      this.exportErrors = '';
    });
  };

  @action _resetExportSourcePath = () => {
    this.exportSourcePath = '';
    this.exportErrors = '';
  };

  @action _nextStep = async () => {
    if (this.walletMigrationStep === IMPORT_WALLET_STEPS.WALLET_IMPORT_FILE) {
      await this._exportWallets();
      if (this.exportedWalletsCount) {
        runInAction('update walletMigrationStep', () => {
          this.walletMigrationStep = IMPORT_WALLET_STEPS.WALLET_SELECT_IMPORT;
        });
      }
    } else {
      this._restoreWallets();
    }
  };

  @action _toggleWalletImportSelection = ({ index }: { index: number }) => {
    const wallet = this.getExportedWalletByIndex(index);
    if (wallet) {
      const { status } = wallet.import;
      const isPending = status === WalletImportStatuses.PENDING;
      this._updateWalletImportStatus(
        index,
        isPending
          ? WalletImportStatuses.UNSTARTED
          : WalletImportStatuses.PENDING
      );

      const walletDuplicates = this.getExportedWalletDuplicatesById(
        wallet.id,
        index
      );
      if (walletDuplicates.length) {
        walletDuplicates.forEach(w => {
          if (w.import.status === WalletImportStatuses.PENDING) {
            w.import.status = WalletImportStatuses.UNSTARTED;
          }
        });
      }
    }
  };

  @action _updateWalletImportStatus = (
    index: number,
    status: WalletImportStatus,
    error?: LocalizableError
  ) => {
    const wallet = this.getExportedWalletByIndex(index);
    if (wallet) {
      wallet.import.status = status;
      wallet.import.error = error || null;
    }
  };

  @action _updateWalletName = ({
    index,
    name,
  }: {
    index: number,
    name: string,
  }) => {
    const wallet = this.getExportedWalletByIndex(index);
    if (wallet) {
      wallet.name = name;
    }
  };

  @action _exportWallets = async () => {
    // Reset export data
    this._resetExportData();

    logger.debug('WalletMigrationStore: Starting wallet export...');
    this.isExportRunning = true;

    const {
      wallets,
      errors,
    }: ExportWalletsMainResponse = await exportWalletsChannel.request({
      exportSourcePath: this.exportSourcePath || this.defaultExportSourcePath,
      locale: this.stores.profile.currentLocale,
    });
    runInAction('update exportedWallets and exportErrors', () => {
      this.exportedWallets = orderBy(
        wallets.map(wallet => {
          const hasName = wallet.name !== null;
          const importedWallet = this.stores.wallets.getWalletById(
            `legacy_${wallet.id}`
          );
          const isImported = typeof importedWallet !== 'undefined';
          if (isImported && importedWallet) wallet.name = importedWallet.name;
          const status = isImported
            ? WalletImportStatuses.EXISTS
            : WalletImportStatuses.UNSTARTED;
          return { ...wallet, hasName, import: { status, error: null } };
        }),
        ['hasName', 'id', 'name', 'is_passphrase_empty'],
        ['desc', 'asc', 'asc', 'asc']
      );

      // Guard against duplicated wallet ids
      this.exportedWallets.forEach((wallet, index) => {
        wallet.index = index + 1;
      });

      this.exportErrors =
        errors || !this.exportedWalletsCount ? 'No wallets found' : '';
    });

    logger.debug(
      `WalletMigrationStore: Exported ${this.exportedWalletsCount} wallets`,
      {
        exportedWalletsData: this.exportedWalletsData,
        exportErrors: this.exportErrors,
      }
    );
    runInAction('update isExportRunning', () => {
      this.isExportRunning = false;
    });
  };

  @action _restoreWallets = async () => {
    // Reset restoration data
    this._resetRestorationData();

    logger.debug(
      `WalletMigrationStore: Restoring ${this.pendingImportWalletsCount} selected wallets...`
    );
    this.isRestorationRunning = true;

    await Promise.all(
      this.pendingImportWallets.map((wallet, index) => {
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
      `WalletMigrationStore: Restored ${this.restoredWalletsCount} of ${this.pendingImportWalletsCount} selected wallets`
    );
    runInAction('update isRestorationRunning', () => {
      this.isRestorationRunning = false;
    });
  };

  @action _restoreWallet = async (exportedWallet: ExportedByronWallet) => {
    // Reset restore requests to clear previous errors
    this.restoreExportedWalletRequest.reset();

    const { id, index } = exportedWallet;
    this._updateWalletImportStatus(index, WalletImportStatuses.RUNNING);
    try {
      const restoredWallet = await this.restoreExportedWalletRequest.execute(
        exportedWallet
      ).promise;
      if (!restoredWallet)
        throw new Error('Restored wallet was not received correctly');

      runInAction('update restoredWallets', () => {
        this._updateWalletImportStatus(index, WalletImportStatuses.COMPLETED);

        const walletDuplicates = this.getExportedWalletDuplicatesById(
          id,
          index
        );
        if (walletDuplicates.length) {
          walletDuplicates.forEach(w => {
            if (w.import.status !== WalletImportStatuses.COMPLETED) {
              w.import.status = WalletImportStatuses.COMPLETED;
            }
          });
        }

        this.restoredWallets.push(restoredWallet);
      });
    } catch (error) {
      runInAction('update restorationErrors', () => {
        const { name, is_passphrase_empty: hasPassword } = exportedWallet;
        this._updateWalletImportStatus(
          index,
          WalletImportStatuses.ERRORED,
          error
        );
        this.restorationErrors.push({
          error,
          wallet: { id, name, hasPassword },
        });
      });
    }
  };

  @action _generateMigrationReport = async () => {
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

  @action _startMigration = async () => {
    const { isMainnet, isTestnet, isTest } = this.environment;
    if (isMainnet || isTestnet || (isTest && this.isTestMigrationEnabled)) {
      // Reset migration data
      this._resetMigration();

      const walletMigrationStatus = await this.getWalletMigrationStatusRequest.execute()
        .promise;
      if (walletMigrationStatus === WalletMigrationStatuses.UNSTARTED) {
        // Wait for wallets to load as we need to match existing and exported wallets
        await this.stores.wallets.refreshWalletsData();

        // Update migration status to "RUNNING"
        logger.debug('WalletMigrationStore: Starting wallet migration...');
        await this.setWalletMigrationStatusRequest.execute(
          WalletMigrationStatuses.RUNNING
        );

        // Trigger wallet export
        await this._exportWallets();

        if (this.exportedWalletsCount) {
          // Wallets successfully exported - ask the user to select the ones to import
          runInAction('update walletMigrationStep', () => {
            this.walletMigrationStep = IMPORT_WALLET_STEPS.WALLET_SELECT_IMPORT;
          });
          this.actions.dialogs.open.trigger({
            dialog: WalletImportFileDialog,
          });
        } else {
          // No wallets have been exported - finish migration
          this._finishMigration();
        }
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

  @action _resetExportData = () => {
    this.isExportRunning = false;
    this.exportedWallets = [];
    this.exportErrors = '';
  };

  @action _resetRestorationData = () => {
    this.isRestorationRunning = false;
    this.restoredWallets = [];
    this.restorationErrors = [];
  };

  @action _resetMigration = () => {
    this._resetExportData();
    this._resetRestorationData();
    this.exportSourcePath = '';
    this.walletMigrationStep = null;
  };

  // For E2E test purpose
  @action _setFakedImportPath = (sourcePath: string) => {
    if (this.environment.isTest) {
      this.exportSourcePath = sourcePath;
      this.defaultExportSourcePath = sourcePath;
    }
  };

  // For E2E test purpose
  @action _enableTestWalletMigration = async () => {
    if (this.environment.isTest) {
      this.isTestMigrationEnabled = true;
      await this.setWalletMigrationStatusRequest.execute(
        WalletMigrationStatuses.UNSTARTED
      );
      this._startMigration();
    }
  };

  @action _finishMigration = async () => {
    if (this.stores.uiDialogs.isOpen(WalletImportFileDialog)) {
      this.actions.dialogs.closeActiveDialog.trigger();
    }

    const walletMigrationStatus = await this.getWalletMigrationStatusRequest.execute()
      .promise;
    if (walletMigrationStatus === WalletMigrationStatuses.RUNNING) {
      // Update migration status
      if (this.exportErrors === '' && !this.restorationErrors.length) {
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
      await this._generateMigrationReport();
    }

    this._resetMigration();
    this.stores.wallets.refreshWalletsData();
  };

  @computed get pendingImportWallets(): Array<ExportedByronWallet> {
    return this.exportedWallets.filter(
      ({ import: { status } }: ExportedByronWallet) =>
        status === WalletImportStatuses.PENDING
    );
  }

  @computed get pendingImportWalletsCount(): number {
    return this.pendingImportWallets.length;
  }

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
