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
exports.emptyTransactionFilterOptions = exports.DateRangeTypes = void 0;
const mobx_1 = require('mobx');
const lodash_1 = require('lodash');
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const validations_1 = require('../utils/validations');
const transactionsCsvGenerator_1 = __importDefault(
  require('../utils/transactionsCsvGenerator')
);
const i18nContext_1 = require('../utils/i18nContext');
const transaction_1 = require('../utils/transaction');
const analytics_1 = require('../analytics');
const INITIAL_SEARCH_LIMIT = null; // 'null' value stands for 'load all'
const SEARCH_LIMIT_INCREASE = 500; // eslint-disable-line
const SEARCH_SKIP = 0;
const RECENT_TRANSACTIONS_LIMIT = 50; // eslint-disable-line
exports.DateRangeTypes = {
  LAST_7_DAYS: 'last7Days',
  LAST_30_DAYS: 'last30Days',
  LAST_90_DAYS: 'last90Days',
  THIS_YEAR: 'thisYear',
  CUSTOM: 'custom',
};
exports.emptyTransactionFilterOptions = {
  searchTerm: '',
  searchLimit: INITIAL_SEARCH_LIMIT,
  searchSkip: SEARCH_SKIP,
  dateRange: '',
  fromDate: '',
  toDate: '',
  fromAmount: '',
  toAmount: '',
  incomingChecked: true,
  outgoingChecked: true,
};
class TransactionsStore extends Store_1.default {
  transactionsRequests = [];
  deleteTransactionRequest = new LocalizedRequest_1.default(
    this.api.ada.deleteTransaction
  );
  createExternalTransactionRequest = new LocalizedRequest_1.default(
    this.api.ada.createExternalTransaction
  );
  _filterOptionsForWallets = {};
  calculateTransactionFeeRequest = new LocalizedRequest_1.default(
    this.api.ada.calculateTransactionFee
  );
  setup() {
    const {
      transactions: transactionActions,
      networkStatus: networkStatusActions,
    } = this.actions;
    transactionActions.filterTransactions.listen(this._updateFilterOptions);
    // transactionActions.loadMoreTransactions.listen(this._increaseSearchLimit);
    transactionActions.requestCSVFile.listen(this._requestCSVFile);
    networkStatusActions.restartNode.listen(this._clearFilterOptions);
    this.registerReactions([this._ensureFilterOptionsForActiveWallet]);
  }
  get recentTransactionsRequest() {
    const wallet = this.stores.wallets.active;
    // TODO: Do not return new request here
    if (!wallet)
      return new LocalizedRequest_1.default(this.api.ada.getTransactions);
    return this._getTransactionsRecentRequest(wallet.id);
  }
  get searchRequest() {
    const wallet = this.stores.wallets.active;
    // TODO: Do not return new request here
    if (!wallet)
      return new LocalizedRequest_1.default(this.api.ada.getTransactions);
    return this._getTransactionsAllRequest(wallet.id);
  }
  get filterOptions() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return null;
    return this._filterOptionsForWallets[wallet.id];
  }
  get withdrawals() {
    const withdrawals = {};
    const { allWallets: wallets } = this.stores.wallets;
    for (const wallet of wallets) {
      const { id: walletId } = wallet;
      const request = this._getWithdrawalsRequest(walletId);
      withdrawals[walletId] =
        (0, lodash_1.get)(request, 'result.withdrawals') ||
        new bignumber_js_1.default(0);
    }
    return withdrawals;
  }
  get all() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const request = this._getTransactionsAllRequest(wallet.id);
    if (!request.result) {
      return [];
    }
    return request.result.transactions || [];
  }
  get allFiltered() {
    const { recentFiltered } = this;
    const allFiltered = this.all.filter((transaction) =>
      (0, transaction_1.isTransactionInFilterRange)(
        this.filterOptions,
        transaction
      )
    );
    // Straight away show recent filtered transactions if all filtered ones are not loaded yet
    return !allFiltered.length && recentFiltered.length
      ? recentFiltered
      : allFiltered;
  }
  get defaultFilterOptions() {
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ dateRange: string; fromDate: string; toDat... Remove this comment to see the full error message
    return (0, transaction_1.generateFilterOptions)(this.all);
  }
  get populatedFilterOptions() {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'TransactionFilterOptionsType | { searchTerm:... Remove this comment to see the full error message
    return this.filterOptions || exports.emptyTransactionFilterOptions;
  }
  get recent() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const results = this._getTransactionsRecentRequest(wallet.id).result;
    return results ? results.transactions : [];
  }
  get recentFiltered() {
    return this.recent.filter((transaction) =>
      (0, transaction_1.isTransactionInFilterRange)(
        this.filterOptions,
        transaction
      )
    );
  }
  get hasAnyFiltered() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const results = this._getTransactionsAllRequest(wallet.id).result;
    return results ? results.transactions.length > 0 : false;
  }
  get hasAny() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const results = this._getTransactionsRecentRequest(wallet.id).result;
    return results ? results.total > 0 : false;
  }
  get totalAvailable() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const results = this._getTransactionsAllRequest(wallet.id).result;
    return results ? results.total : 0;
  }
  get totalFilteredAvailable() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const results = this._getTransactionsAllRequest(wallet.id).result;
    return results ? results.transactions.length : 0;
  }
  get pendingTransactionsCount() {
    return this.recent.filter(({ state }) => state === 'pending').length;
  }
  _refreshTransactionData = async () => {
    if (this.stores.networkStatus.isConnected) {
      const { all: wallets } = this.stores.wallets;
      for (const wallet of wallets) {
        const recentRequest = this._getTransactionsRecentRequest(wallet.id);
        recentRequest.execute({
          walletId: wallet.id,
          order: 'descending',
          fromDate: null,
          toDate: null,
          isLegacy: wallet.isLegacy, // @API TODO - Params "pending" for V2
          // limit: this.RECENT_TRANSACTIONS_LIMIT,
          // skip: 0,
          // searchTerm: '',
          // isFirstLoad: !recentRequest.wasExecuted,
          // isRestoreActive,
          // isRestoreCompleted,
          // cachedTransactions: get(recentRequest, 'result.transactions', []),
        });
        const allRequest = this._getTransactionsAllRequest(wallet.id);
        allRequest.execute({
          walletId: wallet.id,
          order: 'descending',
          fromDate: null,
          toDate: null,
          isLegacy: wallet.isLegacy, // @API TODO - Params "pending" for V2
          // limit: this.INITIAL_SEARCH_LIMIT,
          // skip: 0,
          // searchTerm: '',
          // isFirstLoad: !allRequest.wasExecuted,
          // isRestoreActive,
          // isRestoreCompleted,
          // cachedTransactions: get(allRequest, 'result.transactions', []),
        });
        if (!wallet.isLegacy) {
          const withdrawalsRequest = this._getWithdrawalsRequest(wallet.id);
          withdrawalsRequest.execute({
            walletId: wallet.id,
          });
        }
      }
    }
  };
  // @ts-ignore ts-migrate(1058) FIXME: The return type of an async function must either b... Remove this comment to see the full error message
  calculateTransactionFee = async (transactionFeeRequest) => {
    const { walletId } = transactionFeeRequest;
    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet) {
      throw new Error(
        'Active wallet required before calculating transaction fees.'
      );
    }
    const { amount, availableAmount, reward, isLegacy } = wallet;
    this.calculateTransactionFeeRequest.reset();
    return this.calculateTransactionFeeRequest.execute({
      ...transactionFeeRequest,
      walletBalance: amount,
      availableBalance: availableAmount.plus(reward),
      rewardsBalance: reward,
      isLegacy,
    });
  };
  deletePendingTransaction = async ({ walletId, transactionId }) => {
    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet) {
      throw new Error(
        'Active wallet required before deleting a pending transaction.'
      );
    }
    const { isLegacy } = wallet;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.deleteTransactionRequest.execute({
      walletId,
      transactionId,
      isLegacy,
    });
    this.stores.wallets.refreshWalletsData();
  };
  validateAmount = (amountInLovelaces) =>
    Promise.resolve(
      (0, validations_1.isValidAmountInLovelaces)(amountInLovelaces)
    );
  validateAssetAmount = (amountInNaturalUnits) =>
    Promise.resolve(
      (0, validations_1.isValidAssetAmountInNaturalUnits)(amountInNaturalUnits)
    );
  // ======================= PRIVATE ========================== //
  _updateFilterOptions = (filterOptions) => {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const currentFilterOptions = this._filterOptionsForWallets[wallet.id];
    this._filterOptionsForWallets[wallet.id] = {
      ...currentFilterOptions,
      ...filterOptions,
    };
    this.analytics.sendEvent(
      analytics_1.EventCategories.WALLETS,
      'Set transaction filters'
    );
    return true;
  };
  _clearFilterOptions = () => {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    this._filterOptionsForWallets[wallet.id] = {
      ...exports.emptyTransactionFilterOptions,
    };
    return true;
  };
  _requestCSVFile = async () => {
    const {
      stores: { profile },
      allFiltered,
      actions,
      stores,
    } = this;
    const { isInternalAddress } = stores.addresses;
    const { active } = this.stores.wallets;
    const { desktopDirectoryPath } = profile;
    const locale = profile.currentLocale;
    const intl = (0, i18nContext_1.i18nContext)(locale);
    const transactions = allFiltered;
    const walletName = active ? active.name : '';
    const { getAsset } = this.stores.assets;
    const success = await (0, transactionsCsvGenerator_1.default)({
      desktopDirectoryPath,
      intl,
      transactions,
      walletName,
      getAsset,
      isInternalAddress,
    });
    if (success) {
      actions.transactions.requestCSVFileSuccess.trigger();
      this.analytics.sendEvent(
        analytics_1.EventCategories.WALLETS,
        'Exported transactions as CSV'
      );
    }
  };
  _createExternalTransaction = async (signedTransactionBlob) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.createExternalTransactionRequest.execute({
      signedTransactionBlob,
    });
    this.stores.wallets.refreshWalletsData();
  };
  _getTransactionsRecentRequest = (walletId) => {
    const foundRequest = (0, lodash_1.find)(this.transactionsRequests, {
      walletId,
    });
    if (foundRequest && foundRequest.recentRequest)
      return foundRequest.recentRequest;
    return new LocalizedRequest_1.default(this.api.ada.getTransactions);
  };
  _getTransactionsAllRequest = (walletId) => {
    const foundRequest = (0, lodash_1.find)(this.transactionsRequests, {
      walletId,
    });
    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new LocalizedRequest_1.default(this.api.ada.getTransactions);
  };
  _getWithdrawalsRequest = (walletId) => {
    const foundRequest = (0, lodash_1.find)(this.transactionsRequests, {
      walletId,
    });
    if (foundRequest && foundRequest.withdrawalsRequest)
      return foundRequest.withdrawalsRequest;
    return new LocalizedRequest_1.default(this.api.ada.getWithdrawals);
  };
  // ======================= REACTIONS ========================== //
  /**
   * Reaction that makes sure that we have some default (empty)
   * search options for the active wallet.
   * @private
   */
  _ensureFilterOptionsForActiveWallet = () => {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const options = this._filterOptionsForWallets[wallet.id];
    if (!options) {
      // Setup options for active wallet
      (0, mobx_1.runInAction)('setFilterOptionsForActiveWallet', () => {
        (0, mobx_1.extendObservable)(this._filterOptionsForWallets, {
          [wallet.id]: exports.emptyTransactionFilterOptions,
        });
      });
    }
    return true;
  };
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  TransactionsStore.prototype,
  'transactionsRequests',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  TransactionsStore.prototype,
  'deleteTransactionRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  TransactionsStore.prototype,
  'createExternalTransactionRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  TransactionsStore.prototype,
  '_filterOptionsForWallets',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  TransactionsStore.prototype,
  'calculateTransactionFeeRequest',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', LocalizedRequest_1.default),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'recentTransactionsRequest',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', LocalizedRequest_1.default),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'searchRequest',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'filterOptions',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'withdrawals',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'all',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'allFiltered',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'defaultFilterOptions',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'populatedFilterOptions',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'recent',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'recentFiltered',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'hasAnyFiltered',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'hasAny',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'totalAvailable',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'totalFilteredAvailable',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  TransactionsStore.prototype,
  'pendingTransactionsCount',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  TransactionsStore.prototype,
  '_refreshTransactionData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  TransactionsStore.prototype,
  '_updateFilterOptions',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  TransactionsStore.prototype,
  '_clearFilterOptions',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  TransactionsStore.prototype,
  '_requestCSVFile',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  TransactionsStore.prototype,
  '_createExternalTransaction',
  void 0
);
exports.default = TransactionsStore;
//# sourceMappingURL=TransactionsStore.js.map
