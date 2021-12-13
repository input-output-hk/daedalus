import {
  observable,
  computed,
  action,
  extendObservable,
  runInAction,
} from 'mobx';
import { find, get } from 'lodash';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { WalletTransaction } from '../domains/WalletTransaction';
import type {
  GetTransactionFeeRequest,
  DeleteTransactionRequest,
  GetTransactionsResponse,
  CreateExternalTransactionRequest,
  GetWithdrawalsResponse,
} from '../api/transactions/types';
import {
  isValidAmountInLovelaces,
  isValidAssetAmountInNaturalUnits,
} from '../utils/validations';
import transactionsCsvGenerator from '../utils/transactionsCsvGenerator';
import { i18nContext } from '../utils/i18nContext';
import {
  generateFilterOptions,
  isTransactionInFilterRange,
} from '../utils/transaction';
import type { ApiTokens } from '../api/assets/types';

const INITIAL_SEARCH_LIMIT = null; // 'null' value stands for 'load all'

const SEARCH_LIMIT_INCREASE = 500; // eslint-disable-line

const SEARCH_SKIP = 0;
const RECENT_TRANSACTIONS_LIMIT = 50; // eslint-disable-line

export type DateRangeType =
  | ''
  | 'last7Days'
  | 'last30Days'
  | 'last90Days'
  | 'thisYear'
  | 'custom';
export const DateRangeTypes = {
  LAST_7_DAYS: 'last7Days',
  LAST_30_DAYS: 'last30Days',
  LAST_90_DAYS: 'last90Days',
  THIS_YEAR: 'thisYear',
  CUSTOM: 'custom',
};
export type TransactionFilterOptionsType = {
  searchTerm?: string;
  searchLimit?: number | null | undefined;
  searchSkip?: number | null | undefined;
  dateRange?: DateRangeType;
  fromDate?: string;
  toDate?: string;
  fromAmount?: string;
  toAmount?: string;
  incomingChecked?: boolean;
  outgoingChecked?: boolean;
};
export const emptyTransactionFilterOptions = {
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
type TransactionFeeRequest = {
  walletId: string;
  address: string;
  amount: number;
  assets?: ApiTokens;
};
export default class TransactionsStore extends Store {
  @observable
  transactionsRequests: Array<{
    walletId: string;
    isLegacy: boolean;
    recentRequest: Request<GetTransactionsResponse>;
    allRequest: Request<GetTransactionsResponse>;
    withdrawalsRequest: Request<GetWithdrawalsResponse>;
  }> = [];
  @observable
  deleteTransactionRequest: Request<DeleteTransactionRequest> = new Request(
    this.api.ada.deleteTransaction
  );
  @observable
  createExternalTransactionRequest: Request<
    CreateExternalTransactionRequest
  > = new Request(this.api.ada.createExternalTransaction);
  @observable
  _filterOptionsForWallets = {};
  @observable
  calculateTransactionFeeRequest: Request<
    GetTransactionFeeRequest
  > = new Request(this.api.ada.calculateTransactionFee);

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

  @computed
  get recentTransactionsRequest(): Request<GetTransactionsResponse> {
    const wallet = this.stores.wallets.active;
    // TODO: Do not return new request here
    if (!wallet) return new Request(this.api.ada.getTransactions);
    return this._getTransactionsRecentRequest(wallet.id);
  }

  @computed
  get searchRequest(): Request<GetTransactionsResponse> {
    const wallet = this.stores.wallets.active;
    // TODO: Do not return new request here
    if (!wallet) return new Request(this.api.ada.getTransactions);
    return this._getTransactionsAllRequest(wallet.id);
  }

  @computed
  get filterOptions(): TransactionFilterOptionsType | null | undefined {
    const wallet = this.stores.wallets.active;
    if (!wallet) return null;
    return this._filterOptionsForWallets[wallet.id];
  }

  @computed
  get withdrawals(): Record<string, BigNumber> {
    const withdrawals = {};
    const { allWallets: wallets } = this.stores.wallets;

    for (const wallet of wallets) {
      const { id: walletId } = wallet;

      const request = this._getWithdrawalsRequest(walletId);

      withdrawals[walletId] =
        get(request, 'result.withdrawals') || new BigNumber(0);
    }

    return withdrawals;
  }

  @computed
  get all(): Array<WalletTransaction> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];

    const request = this._getTransactionsAllRequest(wallet.id);

    if (!request.result) {
      return [];
    }

    return request.result.transactions || [];
  }

  @computed
  get allFiltered(): Array<WalletTransaction> {
    const { recentFiltered } = this;
    const allFiltered = this.all.filter((transaction) =>
      isTransactionInFilterRange(this.filterOptions, transaction)
    );
    // Straight away show recent filtered transactions if all filtered ones are not loaded yet
    return !allFiltered.length && recentFiltered.length
      ? recentFiltered
      : allFiltered;
  }

  @computed
  get defaultFilterOptions(): TransactionFilterOptionsType {
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ dateRange: string; fromDate: string; toDat... Remove this comment to see the full error message
    return generateFilterOptions(this.all);
  }

  @computed
  get populatedFilterOptions(): TransactionFilterOptionsType {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'TransactionFilterOptionsType | { searchTerm:... Remove this comment to see the full error message
    return this.filterOptions || emptyTransactionFilterOptions;
  }

  @computed
  get recent(): Array<WalletTransaction> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];

    const results = this._getTransactionsRecentRequest(wallet.id).result;

    return results ? results.transactions : [];
  }

  @computed
  get recentFiltered(): Array<WalletTransaction> {
    return this.recent.filter((transaction) =>
      isTransactionInFilterRange(this.filterOptions, transaction)
    );
  }

  @computed
  get hasAnyFiltered(): boolean {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;

    const results = this._getTransactionsAllRequest(wallet.id).result;

    return results ? results.transactions.length > 0 : false;
  }

  @computed
  get hasAny(): boolean {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;

    const results = this._getTransactionsRecentRequest(wallet.id).result;

    return results ? results.total > 0 : false;
  }

  @computed
  get totalAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;

    const results = this._getTransactionsAllRequest(wallet.id).result;

    return results ? results.total : 0;
  }

  @computed
  get totalFilteredAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;

    const results = this._getTransactionsAllRequest(wallet.id).result;

    return results ? results.transactions.length : 0;
  }

  @computed
  get pendingTransactionsCount(): number {
    return this.recent.filter(({ state }) => state === 'pending').length;
  }

  @action
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
  calculateTransactionFee = async (
    transactionFeeRequest: TransactionFeeRequest
  ) => {
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
  deletePendingTransaction = async ({
    walletId,
    transactionId,
  }: {
    walletId: string;
    transactionId: string;
  }) => {
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
  validateAmount = (amountInLovelaces: string): Promise<boolean> =>
    Promise.resolve(isValidAmountInLovelaces(amountInLovelaces));
  validateAssetAmount = (amountInNaturalUnits: string): Promise<boolean> =>
    Promise.resolve(isValidAssetAmountInNaturalUnits(amountInNaturalUnits));
  // ======================= PRIVATE ========================== //
  @action
  _updateFilterOptions = (filterOptions: TransactionFilterOptionsType) => {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const currentFilterOptions = this._filterOptionsForWallets[wallet.id];
    this._filterOptionsForWallets[wallet.id] = {
      ...currentFilterOptions,
      ...filterOptions,
    };
    return true;
  };
  @action
  _clearFilterOptions = () => {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    this._filterOptionsForWallets[wallet.id] = {
      ...emptyTransactionFilterOptions,
    };
    return true;
  };
  @action
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
    const intl = i18nContext(locale);
    const transactions = allFiltered;
    const walletName = active ? active.name : '';
    const { getAsset } = this.stores.assets;
    const success = await transactionsCsvGenerator({
      desktopDirectoryPath,
      intl,
      transactions,
      walletName,
      getAsset,
      isInternalAddress,
    });
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    if (success) actions.transactions.requestCSVFileSuccess.trigger();
  };
  @action
  _createExternalTransaction = async (signedTransactionBlob: Buffer) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.createExternalTransactionRequest.execute({
      signedTransactionBlob,
    });
    this.stores.wallets.refreshWalletsData();
  };
  _getTransactionsRecentRequest = (
    walletId: string
  ): Request<GetTransactionsResponse> => {
    const foundRequest = find(this.transactionsRequests, {
      walletId,
    });
    if (foundRequest && foundRequest.recentRequest)
      return foundRequest.recentRequest;
    return new Request(this.api.ada.getTransactions);
  };
  _getTransactionsAllRequest = (
    walletId: string
  ): Request<GetTransactionsResponse> => {
    const foundRequest = find(this.transactionsRequests, {
      walletId,
    });
    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new Request(this.api.ada.getTransactions);
  };
  _getWithdrawalsRequest = (
    walletId: string
  ): Request<GetWithdrawalsResponse> => {
    const foundRequest = find(this.transactionsRequests, {
      walletId,
    });
    if (foundRequest && foundRequest.withdrawalsRequest)
      return foundRequest.withdrawalsRequest;
    return new Request(this.api.ada.getWithdrawals);
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
      runInAction('setFilterOptionsForActiveWallet', () => {
        extendObservable(this._filterOptionsForWallets, {
          [wallet.id]: emptyTransactionFilterOptions,
        });
      });
    }

    return true;
  };
}
