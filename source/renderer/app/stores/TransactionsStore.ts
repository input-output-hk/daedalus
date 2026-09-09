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
import {
  TransactionStates,
  TransactionTypes,
  WalletTransaction,
} from '../domains/WalletTransaction';
import type {
  GetTransactionFeeRequest,
  DeleteTransactionRequest,
  GetTransactionsResponse,
  CreateExternalTransactionRequest,
  GetWithdrawalsResponse,
  TransactionState,
} from '../api/transactions/types';
import type { TransactionReviewDisplay } from '../../../common/transactions/reviewDisplay';
import type {
  SubmissionTransactionRecord,
  SubmissionTransactionsData,
} from '../../../common/types/electron-store.types';
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
import { EventCategories } from '../analytics';
import { logger } from '../utils/logging';

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
export type WalletSendLease = Readonly<{
  walletId: string;
  release: () => void;
}>;

export type TrackSubmissionRequest = Readonly<{
  walletId: string;
  transactionId: string;
  state: TransactionState;
  display?: TransactionReviewDisplay;
  payment?: Readonly<{ address: string; amount: string }>;
}>;
export type SelectedTransaction = Readonly<{
  walletId: string;
  transactionId: string;
}>;

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
  private readonly walletSendTails = new Map<string, Promise<void>>();
  @observable
  selectedTransaction: SelectedTransaction | undefined;
  @observable
  private submissionsByWallet: Record<
    string,
    SubmissionTransactionRecord[]
  > = {};
  private readonly submissionLoads = new Map<string, Promise<void>>();
  private readonly submissionWrites = new Map<string, Promise<void>>();
  private readonly currentSessionSubmissions = new Set<string>();

  acquireWalletSendLock = async (
    walletId: string
  ): Promise<WalletSendLease> => {
    const previous = this.walletSendTails.get(walletId) || Promise.resolve();
    let unlock = (): void => undefined;
    const gate = new Promise<void>((resolve) => {
      unlock = resolve;
    });
    const tail = previous.catch(() => undefined).then(() => gate);
    this.walletSendTails.set(walletId, tail);
    await previous.catch(() => undefined);
    let released = false;
    return Object.freeze({
      walletId,
      release: () => {
        if (released) return;
        released = true;
        unlock();
        tail.finally(() => {
          if (this.walletSendTails.get(walletId) === tail)
            this.walletSendTails.delete(walletId);
        });
      },
    });
  };

  withWalletSendLock = async <T>(
    walletId: string,
    work: () => Promise<T>
  ): Promise<T> => {
    const lease = await this.acquireWalletSendLock(walletId);
    try {
      return await work();
    } finally {
      lease.release();
    }
  };

  setup() {
    const {
      transactions: transactionActions,
      networkStatus: networkStatusActions,
    } = this.actions;
    transactionActions.filterTransactions.listen(this._updateFilterOptions);
    // transactionActions.loadMoreTransactions.listen(this._increaseSearchLimit);
    transactionActions.requestCSVFile.listen(this._requestCSVFile);
    this._loadKnownWalletSubmissions();
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

  get all(): Array<WalletTransaction> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const request = this._getTransactionsAllRequest(wallet.id);
    return this._mergeTransactions(
      wallet.id,
      request.result ? request.result.transactions : []
    );
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
    return this._mergeTransactions(
      wallet.id,
      results ? results.transactions : []
    );
  }

  @computed
  get recentFiltered(): Array<WalletTransaction> {
    return this.recent.filter((transaction) =>
      isTransactionInFilterRange(this.filterOptions, transaction)
    );
  }

  @computed
  get hasAnyFiltered(): boolean {
    return this.all.length > 0;
  }

  @computed
  get hasAny(): boolean {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;

    return this.recent.length > 0;
  }

  @computed
  get totalAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;

    return this.all.length;
  }

  @computed
  get totalFilteredAvailable(): number {
    return this.allFiltered.length;
  }

  @computed
  get pendingTransactionsCount(): number {
    return this.recent.filter(({ state }) => state === 'pending').length;
  }
  getTransaction = (
    walletId: string,
    transactionId: string
  ): WalletTransaction | undefined => {
    const recent = this._getTransactionsRecentRequest(walletId).result;
    const all = this._getTransactionsAllRequest(walletId).result;
    const candidates = [
      recent?.transactions.find(({ id }) => id === transactionId),
      all?.transactions.find(({ id }) => id === transactionId),
    ].filter((transaction): transaction is WalletTransaction => !!transaction);
    return this._mergeTransactions(walletId, candidates).find(
      ({ id }) => id === transactionId
    );
  };

  @action
  trackSubmission = async ({
    walletId,
    transactionId,
    state,
    display,
    payment,
  }: TrackSubmissionRequest): Promise<void> => {
    if (
      !/^[A-Za-z0-9_-]{1,128}$/u.test(walletId) ||
      !/^[0-9a-f]{64}$/u.test(transactionId)
    )
      throw new Error('Invalid submitted transaction identity');
    if (
      ![
        TransactionStates.PENDING,
        TransactionStates.OK,
        TransactionStates.EXPIRED,
        TransactionStates.FAILED,
        TransactionStates.SUBMISSION_UNKNOWN,
      ].includes(state)
    )
      throw new Error('Invalid submitted transaction state');
    if (
      payment &&
      (payment.address.length > 256 ||
        !/^(?:0|[1-9]\d*)$/u.test(payment.amount))
    )
      throw new Error('Invalid submitted transaction payment');
    this.currentSessionSubmissions.add(`${walletId}:${transactionId}`);

    const previous = this._submissionRecord(walletId, transactionId);
    const walletChange = display ? display.walletChange : null;
    const amount = walletChange
      ? new BigNumber(walletChange.coin).dividedBy(1000000)
      : new BigNumber(0);
    const normalInputs =
      display?.entries.filter(({ role }) => role === 'input') || [];
    const normalOutputs =
      display?.entries.filter(({ role }) => role === 'output') || [];
    const matchedPayment =
      payment &&
      normalOutputs.find(
        (entry) =>
          entry.address === payment.address &&
          entry.value?.coin === payment.amount
      );
    const isKnownPureAdaSelfTransfer =
      !!display &&
      normalInputs.length > 0 &&
      normalOutputs.length > 0 &&
      [...normalInputs, ...normalOutputs].every(
        ({ ownership, value }) =>
          ownership === 'wallet' && value !== null && value.assets.length === 0
      ) &&
      display.walletChange !== null &&
      display.walletChange.assets.length === 0 &&
      display.mint.length === 0 &&
      display.certificates.length === 0 &&
      display.withdrawals.every(({ ownership }) => ownership === 'wallet');
    const presentationAddress =
      payment?.address ||
      display?.entries.find(
        (entry) =>
          entry.role === 'output' &&
          entry.address &&
          entry.ownership === (amount.isGreaterThan(0) ? 'wallet' : 'other')
      )?.address ||
      previous?.toAddress;
    let type = previous?.type || TransactionTypes.EXPEND;
    let title = previous?.title || 'Ada sent';
    if (walletChange) {
      type = amount.isGreaterThan(0)
        ? TransactionTypes.INCOME
        : TransactionTypes.EXPEND;
      title = amount.isGreaterThan(0) ? 'Ada received' : 'Ada sent';
    }
    if (isKnownPureAdaSelfTransfer) title = 'Transfer within this wallet';
    let transferAmount = previous?.transferAmount;
    let isSelfTransfer = previous?.isSelfTransfer;
    if (matchedPayment && payment) {
      transferAmount = new BigNumber(payment.amount)
        .dividedBy(1000000)
        .toString();
      isSelfTransfer = isKnownPureAdaSelfTransfer || undefined;
    }
    const record: SubmissionTransactionRecord = {
      transactionId,
      state,
      createdAt: previous?.createdAt || new Date().toISOString(),
      amount: walletChange ? amount.toString() : previous?.amount || '0',
      fee: display
        ? new BigNumber(display.fee).dividedBy(1000000).toString()
        : previous?.fee || '0',
      ...(transferAmount ? { transferAmount } : {}),
      ...(isSelfTransfer === undefined ? {} : { isSelfTransfer }),
      amountIsKnown: walletChange !== null || previous?.amountIsKnown === true,
      hasCertificates:
        (display ? display.certificates.length > 0 : undefined) ??
        previous?.hasCertificates ??
        false,
      type,
      title,
      ...(presentationAddress ? { toAddress: presentationAddress } : {}),
      assets: walletChange
        ? walletChange.assets.map(({ policyId, assetName, quantity }) => ({
            policyId,
            assetName,
            quantity: new BigNumber(quantity).absoluteValue().toString(),
          }))
        : previous?.assets || [],
      dismissed: previous?.dismissed || false,
      notified: previous?.notified || false,
    };
    this._setSubmissionRecord(walletId, record);
    await this._ensureSubmissionsLoaded(walletId);
    await this._persistSubmissions(walletId);
  };

  @action
  dismissReceipt = (
    walletId: string,
    transactionIds: readonly string[]
  ): void => {
    const ids = new Set(transactionIds);
    const confirmedIds = new Set(
      transactionIds.filter(
        (transactionId) =>
          this.getTransaction(walletId, transactionId)?.state ===
          TransactionStates.OK
      )
    );
    const records = this.submissionsByWallet[walletId] || [];
    let changed = false;
    const next = records.map((record) => {
      if (!ids.has(record.transactionId)) return record;
      const notified =
        record.notified || confirmedIds.has(record.transactionId);
      if (record.dismissed && notified === record.notified) return record;
      changed = true;
      return { ...record, dismissed: true, notified };
    });
    if (!changed) return;
    this.submissionsByWallet = {
      ...this.submissionsByWallet,
      [walletId]: next,
    };
    this._persistSubmissions(walletId).catch((error) => {
      logger.warn('Transaction receipt dismissal could not be persisted', {
        error,
      });
    });
  };

  @action
  openTransaction = (walletId: string, transactionId: string): void => {
    this._filterOptionsForWallets[walletId] = {
      ...emptyTransactionFilterOptions,
    };
    this.selectedTransaction = { walletId, transactionId };
    this.actions.router.goToRoute.trigger({
      route: this.stores.wallets.getWalletRoute(walletId, 'transactions'),
    });
  };

  @action
  _refreshTransactionData = async () => {
    if (this.stores.networkStatus.isConnected) {
      const { all: wallets } = this.stores.wallets;

      for (const wallet of wallets) {
        this._ensureSubmissionsLoaded(wallet.id);
        const reconcile = (request: Request<GetTransactionsResponse>) => {
          request.promise
            ?.then(() =>
              this._reconcileSubmissions(wallet.id, [
                ...(recentRequest.result?.transactions || []),
                ...(allRequest.result?.transactions || []),
              ])
            )
            .catch(() => undefined);
        };

        const recentRequest = this._getTransactionsRecentRequest(wallet.id);
        recentRequest.execute({
          walletId: wallet.id,
          order: 'descending',
          fromDate: null,
          toDate: null,
          isLegacy: wallet.isLegacy,
        });
        reconcile(recentRequest);

        const allRequest = this._getTransactionsAllRequest(wallet.id);
        allRequest.execute({
          walletId: wallet.id,
          order: 'descending',
          fromDate: null,
          toDate: null,
          isLegacy: wallet.isLegacy,
        });
        reconcile(allRequest);

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
    await this._ensureSubmissionsLoaded(walletId);
    const records = this.submissionsByWallet[walletId] || [];
    const next = records.filter(
      (record) => record.transactionId !== transactionId
    );
    if (next.length !== records.length) {
      runInAction('TransactionsStore::removeSubmission', () => {
        this.submissionsByWallet = {
          ...this.submissionsByWallet,
          [walletId]: next,
        };
      });
      await this._persistSubmissions(walletId);
    }
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

    this.analytics.sendEvent(
      EventCategories.WALLETS,
      'Set transaction filters'
    );
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
    if (success) {
      actions.transactions.requestCSVFileSuccess.trigger();
      this.analytics.sendEvent(
        EventCategories.WALLETS,
        'Exported transactions as CSV'
      );
    }
  };
  @action
  _createExternalTransaction = async (signedTransactionBlob: Buffer) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.createExternalTransactionRequest.execute({
      signedTransactionBlob,
    });
    this.stores.wallets.refreshWalletsData();
  };
  private _submissionRecord = (
    walletId: string,
    transactionId: string
  ): SubmissionTransactionRecord | undefined =>
    (this.submissionsByWallet[walletId] || []).find(
      (record) => record.transactionId === transactionId
    );

  private _limitSubmissionRecords = (
    records: SubmissionTransactionRecord[]
  ): SubmissionTransactionRecord[] => {
    if (records.length <= 100) return records;
    const unresolved = records.filter(
      ({ state }) =>
        state === TransactionStates.PENDING ||
        state === TransactionStates.SUBMISSION_UNKNOWN
    );
    return [
      ...unresolved.slice(0, 100),
      ...records
        .filter(
          ({ state }) =>
            state !== TransactionStates.PENDING &&
            state !== TransactionStates.SUBMISSION_UNKNOWN
        )
        .slice(0, Math.max(0, 100 - unresolved.length)),
    ];
  };

  @action
  private _setSubmissionRecord = (
    walletId: string,
    record: SubmissionTransactionRecord
  ): void => {
    const previous = this._submissionRecord(walletId, record.transactionId);
    let nextRecord = record;
    if (
      this.currentSessionSubmissions.has(
        `${walletId}:${record.transactionId}`
      ) &&
      previous &&
      previous.state !== TransactionStates.OK &&
      record.state === TransactionStates.OK &&
      previous.dismissed &&
      !previous.notified
    ) {
      this.actions.transactions.transactionConfirmed.trigger({
        walletId,
        transactionId: record.transactionId,
      });
      nextRecord = { ...record, notified: true };
    }
    this.submissionsByWallet = {
      ...this.submissionsByWallet,
      [walletId]: this._limitSubmissionRecords([
        nextRecord,
        ...(this.submissionsByWallet[walletId] || []).filter(
          ({ transactionId }) => transactionId !== record.transactionId
        ),
      ]),
    };
  };

  private _createSubmissionTransaction = (
    record: SubmissionTransactionRecord
  ): WalletTransaction =>
    new WalletTransaction({
      id: record.transactionId,
      type: record.type,
      title: record.title,
      amount: new BigNumber(record.amount),
      fee: new BigNumber(record.fee),
      deposit: new BigNumber(0),
      date: new Date(record.createdAt),
      assets: record.assets.map(({ policyId, assetName, quantity }) => ({
        policyId,
        assetName,
        uniqueId: `${policyId}${assetName}`,
        quantity: new BigNumber(quantity),
        ...(record.toAddress ? { address: record.toAddress } : {}),
      })),
      description: '',
      addresses: {
        from: [],
        to: record.toAddress ? [record.toAddress] : [],
        withdrawals: [],
      },
      state: record.state,
      confirmations: 0,
      slotNumber: null,
      epochNumber: null,
      metadata: null,
      ...(record.transferAmount
        ? { transferAmount: new BigNumber(record.transferAmount) }
        : {}),
      ...(record.isSelfTransfer === undefined
        ? {}
        : { isSelfTransfer: record.isSelfTransfer }),
      amountIsKnown: record.amountIsKnown,
      hasCertificates: record.hasCertificates,
      localSubmission: true,
    });

  private _mergeTransactions = (
    walletId: string,
    backendTransactions: WalletTransaction[]
  ): WalletTransaction[] => {
    const walletAddresses = this.stores.addresses._getAddressesAllRequest(
      walletId
    ).result;
    const ownedAddresses = new Set(
      walletAddresses ? walletAddresses.map(({ id }) => id) : []
    );
    const backendById = new Map<string, WalletTransaction>();
    backendTransactions.forEach((transaction) => {
      let displayed = transaction;
      const inputAddresses = transaction.addresses.from;
      const outputAddresses = transaction.addresses.to;
      if (
        transaction.hasOnlyAda === true &&
        transaction.hasCertificates === false &&
        transaction.addresses.withdrawals.length === 0 &&
        inputAddresses.length > 0 &&
        outputAddresses.length > 0 &&
        [...inputAddresses, ...outputAddresses].every(
          (address): address is string =>
            typeof address === 'string' &&
            address.length > 0 &&
            ownedAddresses.has(address)
        )
      )
        displayed = new WalletTransaction({
          ...transaction,
          isSelfTransfer: true,
          title: 'Transfer within this wallet',
        });
      const previous = backendById.get(transaction.id);
      const previousIsUnresolved =
        previous?.state === TransactionStates.PENDING ||
        previous?.state === TransactionStates.SUBMISSION_UNKNOWN;
      const transactionIsResolved =
        transaction.state !== TransactionStates.PENDING &&
        transaction.state !== TransactionStates.SUBMISSION_UNKNOWN;
      if (!previous || (previousIsUnresolved && transactionIsResolved))
        backendById.set(transaction.id, displayed);
    });
    const records = this.submissionsByWallet[walletId] || [];
    records.forEach((record) => {
      const backend = backendById.get(record.transactionId);
      if (backend) {
        const backendIsUnresolved =
          backend.state === TransactionStates.PENDING ||
          backend.state === TransactionStates.SUBMISSION_UNKNOWN;
        const recordIsResolved =
          record.state !== TransactionStates.PENDING &&
          record.state !== TransactionStates.SUBMISSION_UNKNOWN;
        backendById.set(
          record.transactionId,
          new WalletTransaction({
            ...backend,
            ...(backendIsUnresolved && recordIsResolved
              ? { state: record.state }
              : {}),
            localSubmission: true,
            ...(record.transferAmount
              ? { transferAmount: new BigNumber(record.transferAmount) }
              : {}),
            ...(record.isSelfTransfer === undefined
              ? {}
              : { isSelfTransfer: record.isSelfTransfer }),
          })
        );
      } else {
        backendById.set(
          record.transactionId,
          this._createSubmissionTransaction(record)
        );
      }
    });
    return Array.from(backendById.values()).sort(
      (left, right) =>
        (right.date ? right.date.getTime() : 0) -
        (left.date ? left.date.getTime() : 0)
    );
  };

  @action
  private _reconcileSubmissions = async (
    walletId: string,
    transactions: WalletTransaction[]
  ): Promise<void> => {
    await this._ensureSubmissionsLoaded(walletId);
    const backendById = new Map<string, WalletTransaction>();
    transactions.forEach((transaction) => {
      const previous = backendById.get(transaction.id);
      const previousIsUnresolved =
        previous?.state === TransactionStates.PENDING ||
        previous?.state === TransactionStates.SUBMISSION_UNKNOWN;
      const transactionIsResolved =
        transaction.state !== TransactionStates.PENDING &&
        transaction.state !== TransactionStates.SUBMISSION_UNKNOWN;
      if (!previous || (previousIsUnresolved && transactionIsResolved))
        backendById.set(transaction.id, transaction);
    });
    const records = this.submissionsByWallet[walletId] || [];
    let changed = false;
    records.forEach((record) => {
      const backend = backendById.get(record.transactionId);
      if (!backend) return;
      if (
        backend.state === record.state &&
        (backend.hasCertificates === true) === record.hasCertificates
      )
        return;
      changed = true;
      this._setSubmissionRecord(walletId, {
        ...record,
        state: backend.state,
        hasCertificates: backend.hasCertificates === true,
      });
    });
    if (changed) await this._persistSubmissions(walletId);
  };

  @action
  private _ensureSubmissionsLoaded = (walletId: string): Promise<void> => {
    const existing = this.submissionLoads.get(walletId);
    if (existing) return existing;
    const load = this.api.localStorage
      .getSubmissionTransactions(walletId)
      .then(
        action(
          'TransactionsStore::loadSubmissions',
          (data: SubmissionTransactionsData) => {
            const current = this.submissionsByWallet[walletId] || [];
            const currentIds = new Set(
              current.map(({ transactionId }) => transactionId)
            );
            this.submissionsByWallet = {
              ...this.submissionsByWallet,
              [walletId]: this._limitSubmissionRecords([
                ...current,
                ...data.records.filter(
                  ({ transactionId }) => !currentIds.has(transactionId)
                ),
              ]),
            };
          }
        )
      )
      .catch(() => undefined);
    this.submissionLoads.set(walletId, load);
    return load;
  };

  private _loadKnownWalletSubmissions = async (): Promise<void> => {
    await Promise.all(
      this.stores.wallets.all.map(({ id }) => this._ensureSubmissionsLoaded(id))
    );
  };

  private _persistSubmissions = (walletId: string): Promise<void> => {
    const previous = this.submissionWrites.get(walletId) || Promise.resolve();
    const next = previous
      .catch(() => undefined)
      .then(() =>
        this.api.localStorage.setSubmissionTransactions(walletId, {
          version: 1,
          records: this.submissionsByWallet[walletId] || [],
        })
      );
    this.submissionWrites.set(walletId, next);
    return next;
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
