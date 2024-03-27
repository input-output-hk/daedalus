'use strict';
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.WalletMigrationStatuses = void 0;
const mobx_1 = require('mobx');
const path_1 = __importDefault(require('path'));
const lodash_1 = require('lodash');
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const cardano_ipc_1 = require('../ipc/cardano.ipc');
const show_file_dialog_channels_1 = require('../ipc/show-file-dialog-channels');
const generateWalletMigrationReportChannel_1 = require('../ipc/generateWalletMigrationReportChannel');
const logging_1 = require('../utils/logging');
const utils_1 = require('../api/utils');
const WalletImportFileDialog_1 = __importDefault(
  require('../components/wallet/wallet-import/WalletImportFileDialog')
);
const helper_1 = require('../../../common/utils/helper');
const walletExportTypes_1 = require('../types/walletExportTypes');
const walletRestoreConfig_1 = require('../config/walletRestoreConfig');
const walletsConfig_1 = require('../config/walletsConfig');
const analytics_1 = require('../analytics');
exports.WalletMigrationStatuses = {
  UNSTARTED: 'unstarted',
  RUNNING: 'running',
  COMPLETED: 'completed',
  SKIPPED: 'skipped',
  ERRORED: 'errored',
};
class WalletMigrationStore extends Store_1.default {
  walletMigrationStep = null;
  isExportRunning = false;
  exportedWallets = [];
  exportErrors = '';
  exportSourcePath = '';
  defaultExportSourcePath = global.legacyStateDir;
  isTestMigrationEnabled = false;
  isRestorationRunning = false;
  restoredWallets = [];
  restorationErrors = [];
  getWalletMigrationStatusRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getWalletMigrationStatus
  );
  setWalletMigrationStatusRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setWalletMigrationStatus
  );
  restoreExportedWalletRequest = new LocalizedRequest_1.default(
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
  getExportedWalletById = (id) => this.exportedWallets.find((w) => w.id === id);
  getExportedWalletDuplicatesById = (id, index) =>
    this.exportedWallets.filter((w) => w.id === id && w.index !== index);
  getExportedWalletByIndex = (index) =>
    this.exportedWallets.find((w) => w.index === index);
  _initiateMigration = () => {
    this.walletMigrationStep =
      walletRestoreConfig_1.IMPORT_WALLET_STEPS.WALLET_IMPORT_FILE;
  };
  _selectExportSourcePath = async ({ importFrom }) => {
    const params =
      importFrom === walletExportTypes_1.ImportFromOptions.STATE_DIR
        ? {
            defaultPath: this.defaultExportSourcePath,
            properties: ['openDirectory'],
          }
        : {
            defaultPath: path_1.default.join(
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
    const {
      filePaths,
    } = await show_file_dialog_channels_1.showOpenDialogChannel.send(params);
    if (!filePaths || filePaths.length === 0) {
      return;
    }
    const filePath = filePaths[0];
    (0, mobx_1.runInAction)('update exportSourcePath', () => {
      this.exportSourcePath = filePath;
      this.exportErrors = '';
    });
  };
  _resetExportSourcePath = () => {
    this.exportSourcePath = '';
    this.exportErrors = '';
  };
  _nextStep = async () => {
    if (
      this.walletMigrationStep ===
      walletRestoreConfig_1.IMPORT_WALLET_STEPS.WALLET_IMPORT_FILE
    ) {
      await this._exportWallets();
      if (this.exportedWalletsCount) {
        (0, mobx_1.runInAction)('update walletMigrationStep', () => {
          this.walletMigrationStep =
            walletRestoreConfig_1.IMPORT_WALLET_STEPS.WALLET_SELECT_IMPORT;
        });
      }
    } else {
      this._restoreWallets();
    }
  };
  _toggleWalletImportSelection = ({ index }) => {
    const wallet = this.getExportedWalletByIndex(index);
    if (wallet) {
      const { status } = wallet.import;
      const isPending =
        status === walletExportTypes_1.WalletImportStatuses.PENDING;
      this._updateWalletImportStatus(
        index,
        isPending
          ? walletExportTypes_1.WalletImportStatuses.UNSTARTED
          : walletExportTypes_1.WalletImportStatuses.PENDING
      );
      const walletDuplicates = this.getExportedWalletDuplicatesById(
        wallet.id,
        index
      );
      if (walletDuplicates.length) {
        walletDuplicates.forEach((w) => {
          if (
            w.import.status === walletExportTypes_1.WalletImportStatuses.PENDING
          ) {
            w.import.status =
              walletExportTypes_1.WalletImportStatuses.UNSTARTED;
          }
        });
      }
    }
  };
  _updateWalletImportStatus = (index, status, error) => {
    const wallet = this.getExportedWalletByIndex(index);
    if (wallet) {
      wallet.import.status = status;
      if (error) wallet.import.error = error;
    }
  };
  _updateWalletName = ({ index, name }) => {
    const wallet = this.getExportedWalletByIndex(index);
    if (wallet) {
      wallet.name = name;
    }
  };
  _exportWallets = async () => {
    // Reset export data
    this._resetExportData();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.debug('WalletMigrationStore: Starting wallet export...');
    this.isExportRunning = true;
    const {
      wallets,
      errors,
    } = await cardano_ipc_1.exportWalletsChannel.request({
      exportSourcePath: this.exportSourcePath || this.defaultExportSourcePath,
      locale: this.stores.profile.currentLocale,
    });
    (0, mobx_1.runInAction)('update exportedWallets and exportErrors', () => {
      this.exportedWallets = (0, lodash_1.orderBy)(
        wallets.map((wallet) => {
          const hasName = wallet.name !== null;
          const importedWallet = this.stores.wallets.getWalletById(
            `legacy_${wallet.id}`
          );
          const isImported = typeof importedWallet !== 'undefined';
          if (isImported && importedWallet) wallet.name = importedWallet.name;
          const status = isImported
            ? walletExportTypes_1.WalletImportStatuses.EXISTS
            : walletExportTypes_1.WalletImportStatuses.UNSTARTED;
          return {
            ...wallet,
            hasName,
            import: {
              status,
              error: null,
            },
          };
        }),
        ['hasName', 'id', 'name', 'isEmptyPassphrase'],
        ['desc', 'asc', 'asc', 'asc']
      );
      // Guard against duplicated wallet ids
      this.exportedWallets.forEach((wallet, index) => {
        wallet.index = index + 1;
      });
      this.exportErrors =
        errors || !this.exportedWalletsCount ? 'No wallets found' : '';
    });
    logging_1.logger.debug(
      `WalletMigrationStore: Exported ${this.exportedWalletsCount} wallets`,
      {
        exportedWalletsData: (0, helper_1.toJS)(this.exportedWalletsData),
        exportErrors: this.exportErrors,
      }
    );
    (0, mobx_1.runInAction)('update isExportRunning', () => {
      this.isExportRunning = false;
    });
  };
  _restoreWallets = async () => {
    // Reset restoration data
    this._resetRestorationData();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.debug(
      `WalletMigrationStore: Restoring ${this.pendingImportWalletsCount} selected wallets...`
    );
    this.isRestorationRunning = true;
    await Promise.all(
      this.pendingImportWallets.map((wallet, index) => {
        logging_1.logger.debug(
          `WalletMigrationStore: Restoring ${index + 1}. wallet...`,
          {
            id: wallet.id,
            name: wallet.name,
            hasPassword: !wallet.isEmptyPassphrase,
          }
        );
        return this._restoreWallet(wallet);
      })
    );
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.debug(
      `WalletMigrationStore: Restored ${this.restoredWalletsCount} of ${this.pendingImportWalletsCount} selected wallets`
    );
    (0, mobx_1.runInAction)('update isRestorationRunning', () => {
      this.isRestorationRunning = false;
    });
    this.analytics.sendEvent(
      analytics_1.EventCategories.WALLETS,
      'Restored legacy wallet(s)'
    );
  };
  _restoreWallet = async (exportedWallet) => {
    // Reset restore requests to clear previous errors
    this.restoreExportedWalletRequest.reset();
    const { id, index } = exportedWallet;
    this._updateWalletImportStatus(
      index,
      walletExportTypes_1.WalletImportStatuses.RUNNING
    );
    try {
      const restoredWallet = await this.restoreExportedWalletRequest.execute(
        exportedWallet
      ).promise;
      if (!restoredWallet)
        throw new Error('Restored wallet was not received correctly');
      (0, mobx_1.runInAction)('update restoredWallets', () => {
        this._updateWalletImportStatus(
          index,
          walletExportTypes_1.WalletImportStatuses.COMPLETED
        );
        const walletDuplicates = this.getExportedWalletDuplicatesById(
          id,
          index
        );
        if (walletDuplicates.length) {
          walletDuplicates.forEach((w) => {
            if (
              w.import.status !==
              walletExportTypes_1.WalletImportStatuses.COMPLETED
            ) {
              w.import.status =
                walletExportTypes_1.WalletImportStatuses.COMPLETED;
            }
          });
        }
        this.restoredWallets.push(restoredWallet);
      });
    } catch (error) {
      const errorStr =
        error.defaultMessage || error.message || error.toString();
      (0, mobx_1.runInAction)('update restorationErrors', () => {
        const { name, isEmptyPassphrase } = exportedWallet;
        this._updateWalletImportStatus(
          index,
          walletExportTypes_1.WalletImportStatuses.ERRORED,
          errorStr
        );
        this.restorationErrors.push({
          error: errorStr,
          wallet: {
            id,
            name,
            hasPassword: !isEmptyPassphrase,
          },
        });
      });
    }
  };
  _generateMigrationReport = async () => {
    const finalMigrationStatus = await this.getWalletMigrationStatusRequest.execute()
      .promise;
    const walletMigrationReportData = {
      exportedWalletsData: this.exportedWalletsData,
      exportedWalletsCount: this.exportedWalletsCount,
      exportErrors: this.exportErrors,
      restoredWalletsData: this.restoredWalletsData,
      restoredWalletsCount: this.restoredWalletsCount,
      restorationErrors: this.restorationErrors,
      finalMigrationStatus,
    };
    logging_1.logger.debug(
      'WalletMigrationStore: Generating wallet migration report...',
      {
        walletMigrationReportData,
      }
    );
    try {
      await generateWalletMigrationReportChannel_1.generateWalletMigrationReportChannel.send(
        (0, helper_1.toJS)(walletMigrationReportData)
      );
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.debug(
        'WalletMigrationStore: Generated wallet migration report'
      );
    } catch (error) {
      logging_1.logger.error(
        'WalletMigrationStore: Wallet migration report generation failed',
        {
          error,
        }
      );
    }
  };
  _startMigration = async () => {
    if (!walletsConfig_1.IS_AUTOMATIC_WALLET_MIGRATION_ENABLED) return;
    const { isMainnet, isTestnet, isTest } = this.environment;
    if (isMainnet || isTestnet || (isTest && this.isTestMigrationEnabled)) {
      // Reset migration data
      this._resetMigration();
      const walletMigrationStatus = await this.getWalletMigrationStatusRequest.execute()
        .promise;
      if (walletMigrationStatus === exports.WalletMigrationStatuses.UNSTARTED) {
        // Wait for wallets to load as we need to match existing and exported wallets
        await this.stores.wallets.refreshWalletsData();
        // Update migration status to "RUNNING"
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.debug(
          'WalletMigrationStore: Starting wallet migration...'
        );
        // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
        await this.setWalletMigrationStatusRequest.execute(
          exports.WalletMigrationStatuses.RUNNING
        );
        // Trigger wallet export
        await this._exportWallets();
        if (this.exportedWalletsCount) {
          // Wallets successfully exported - ask the user to select the ones to import
          (0, mobx_1.runInAction)('update walletMigrationStep', () => {
            this.walletMigrationStep =
              walletRestoreConfig_1.IMPORT_WALLET_STEPS.WALLET_SELECT_IMPORT;
          });
          this.actions.dialogs.open.trigger({
            // @ts-ignore ts-migrate(2322) FIXME: Type 'typeof WalletImportFileDialog' is not assign... Remove this comment to see the full error message
            dialog: WalletImportFileDialog_1.default,
          });
        } else {
          // No wallets have been exported - finish migration
          this._finishMigration();
        }
      } else {
        logging_1.logger.debug(
          'WalletMigrationStore: Skipping wallet migration...',
          {
            walletMigrationStatus,
          }
        );
      }
    } else {
      // Update migration status to "SKIPPED"
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.setWalletMigrationStatusRequest.execute(
        exports.WalletMigrationStatuses.SKIPPED
      );
    }
  };
  _resetExportData = () => {
    this.isExportRunning = false;
    this.exportedWallets = [];
    this.exportErrors = '';
  };
  _resetRestorationData = () => {
    this.isRestorationRunning = false;
    this.restoredWallets = [];
    this.restorationErrors = [];
  };
  _resetMigration = () => {
    this._resetExportData();
    this._resetRestorationData();
    this.exportSourcePath = '';
    this.walletMigrationStep = null;
  };
  // For E2E test purpose
  _setFakedImportPath = (sourcePath) => {
    if (this.environment.isTest) {
      this.exportSourcePath = sourcePath;
      this.defaultExportSourcePath = sourcePath;
    }
  };
  // For E2E test purpose
  _enableTestWalletMigration = async () => {
    if (this.environment.isTest) {
      this.isTestMigrationEnabled = true;
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.setWalletMigrationStatusRequest.execute(
        exports.WalletMigrationStatuses.UNSTARTED
      );
      this._startMigration();
    }
  };
  _finishMigration = async () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'typeof WalletImportFileDialog' i... Remove this comment to see the full error message
    if (this.stores.uiDialogs.isOpen(WalletImportFileDialog_1.default)) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
      this.actions.dialogs.closeActiveDialog.trigger();
    }
    const walletMigrationStatus = await this.getWalletMigrationStatusRequest.execute()
      .promise;
    if (walletMigrationStatus === exports.WalletMigrationStatuses.RUNNING) {
      // Update migration status
      if (this.exportErrors === '' && !this.restorationErrors.length) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.debug(
          'WalletMigrationStore: Wallet migration succeeded'
        );
        // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
        await this.setWalletMigrationStatusRequest.execute(
          exports.WalletMigrationStatuses.COMPLETED
        );
      } else {
        logging_1.logger.debug(
          'WalletMigrationStore: Wallet migration failed',
          {
            exportErrors: this.exportErrors,
            restorationErrors: this.restorationErrors,
          }
        );
        // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
        await this.setWalletMigrationStatusRequest.execute(
          exports.WalletMigrationStatuses.ERRORED
        );
      }
      // Generate and store migration report
      await this._generateMigrationReport();
    }
    this._resetMigration();
    this.stores.wallets.refreshWalletsData();
  };
  get pendingImportWallets() {
    return this.exportedWallets.filter(
      ({ import: { status } }) =>
        status === walletExportTypes_1.WalletImportStatuses.PENDING
    );
  }
  get pendingImportWalletsCount() {
    return this.pendingImportWallets.length;
  }
  get exportedWalletsData() {
    return this.exportedWallets.map((wallet) => ({
      id: wallet.id,
      name: wallet.name,
      hasPassword: !wallet.isEmptyPassphrase,
      import: (0, helper_1.toJS)(wallet.import),
    }));
  }
  get exportedWalletsCount() {
    return this.exportedWallets.length;
  }
  get restoredWalletsData() {
    return this.restoredWallets.map((wallet) => ({
      id: (0, utils_1.getRawWalletId)(wallet.id),
      name: wallet.name,
      hasPassword: wallet.hasPassword,
    }));
  }
  get restoredWalletsCount() {
    return this.restoredWallets.length;
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletMigrationStore.prototype,
  'walletMigrationStep',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  'isExportRunning',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  WalletMigrationStore.prototype,
  'exportedWallets',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  'exportErrors',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  'exportSourcePath',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletMigrationStore.prototype,
  'defaultExportSourcePath',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  'isTestMigrationEnabled',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  'isRestorationRunning',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  WalletMigrationStore.prototype,
  'restoredWallets',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  WalletMigrationStore.prototype,
  'restorationErrors',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletMigrationStore.prototype,
  'getWalletMigrationStatusRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletMigrationStore.prototype,
  'setWalletMigrationStatusRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletMigrationStore.prototype,
  'restoreExportedWalletRequest',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_initiateMigration',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_selectExportSourcePath',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_resetExportSourcePath',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_nextStep',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_toggleWalletImportSelection',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_updateWalletImportStatus',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_updateWalletName',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_exportWallets',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_restoreWallets',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_restoreWallet',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_generateMigrationReport',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_startMigration',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_resetExportData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_resetRestorationData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_resetMigration',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_setFakedImportPath',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_enableTestWalletMigration',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletMigrationStore.prototype,
  '_finishMigration',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  WalletMigrationStore.prototype,
  'pendingImportWallets',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  WalletMigrationStore.prototype,
  'pendingImportWalletsCount',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  WalletMigrationStore.prototype,
  'exportedWalletsData',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  WalletMigrationStore.prototype,
  'exportedWalletsCount',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  WalletMigrationStore.prototype,
  'restoredWalletsData',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  WalletMigrationStore.prototype,
  'restoredWalletsCount',
  null
);
exports.default = WalletMigrationStore;
//# sourceMappingURL=WalletMigrationStore.js.map
