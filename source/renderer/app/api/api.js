// @flow
import { split, get, includes, map } from 'lodash';
import { action } from 'mobx';
import BigNumber from 'bignumber.js';
import moment from 'moment';

// domains
import Wallet, {
  WalletDelegationStatuses,
  WalletUnits,
} from '../domains/Wallet';
import {
  WalletTransaction,
  TransactionTypes,
  TransactionStates,
} from '../domains/WalletTransaction';
import WalletAddress from '../domains/WalletAddress';

// Addresses requests
import { getAddresses } from './addresses/requests/getAddresses';

// Network requests
import { getNetworkInfo } from './network/requests/getNetworkInfo';

// Nodes requests
import { applyNodeUpdate } from './nodes/requests/applyNodeUpdate';
// import { getNextNodeUpdate } from './nodes/requests/getNextNodeUpdate';
import { postponeNodeUpdate } from './nodes/requests/postponeNodeUpdate';
import { getLatestAppVersion } from './nodes/requests/getLatestAppVersion';

// Transactions requests
import { getTransactionFee } from './transactions/requests/getTransactionFee';
import { getTransactionHistory } from './transactions/requests/getTransactionHistory';
import { getLegacyWalletTransactionHistory } from './transactions/requests/getLegacyWalletTransactionHistory';
import { createTransaction } from './transactions/requests/createTransaction';
import { deleteLegacyTransaction } from './transactions/requests/deleteLegacyTransaction';

// Wallets requests
import { changeSpendingPassword } from './wallets/requests/changeSpendingPassword';
import { deleteWallet } from './wallets/requests/deleteWallet';
import { deleteLegacyWallet } from './wallets/requests/deleteLegacyWallet';
import { exportWalletAsJSON } from './wallets/requests/exportWalletAsJSON';
import { importWalletAsJSON } from './wallets/requests/importWalletAsJSON';
import { getWallets } from './wallets/requests/getWallets';
import { getLegacyWallets } from './wallets/requests/getLegacyWallets';
import { importWalletAsKey } from './wallets/requests/importWalletAsKey';
import { createWallet } from './wallets/requests/createWallet';
import { restoreWallet } from './wallets/requests/restoreWallet';
import { restoreLegacyWallet } from './wallets/requests/restoreLegacyWallet';
import { updateWallet } from './wallets/requests/updateWallet';
import { getWalletUtxos } from './wallets/requests/getWalletUtxos';
import { getWallet } from './wallets/requests/getWallet';
import { getWalletIdAndBalance } from './wallets/requests/getWalletIdAndBalance';
import { transferFundsCalculateFee } from './wallets/requests/transferFundsCalculateFee';
import { transferFunds } from './wallets/requests/transferFunds';

// Staking
import StakePool from '../domains/StakePool';
import stakingStakePoolsMissingApiData from '../config/stakingStakePoolsMissingApiData.dummy.json';
import { EPOCH_LENGTH_ITN } from '../config/epochsConfig';

// News requests
import { getNews } from './news/requests/getNews';

// Stake Pools request
import { getStakePools } from './staking/requests/getStakePools';
import { getDelegationFee } from './staking/requests/getDelegationFee';
import { joinStakePool } from './staking/requests/joinStakePool';
import { quitStakePool } from './staking/requests/quitStakePool';

// Utility functions
import {
  awaitUpdateChannel,
  cardanoFaultInjectionChannel,
} from '../ipc/cardano.ipc';
import patchAdaApi from './utils/patchAdaApi';
import { utcStringToDate } from './utils';
import { Logger } from '../utils/logging';
import {
  unscrambleMnemonics,
  scrambleMnemonics,
  generateAccountMnemonics,
  generateAdditionalMnemonics,
} from './utils/mnemonics';
import { filterLogData } from '../../../common/utils/logging';

// Config constants
import { LOVELACES_PER_ADA } from '../config/numbersConfig';
import { ADA_CERTIFICATE_MNEMONIC_LENGTH } from '../config/cryptoConfig';

// Addresses Types
import type { Address, GetAddressesRequest } from './addresses/types';

// Common Types
import type { RequestConfig } from './common/types';

// Network Types
import type {
  GetNetworkInfoResponse,
  NetworkInfoResponse,
} from './network/types';

// Nodes Types
import type {
  LatestAppVersionInfoResponse,
  NodeSoftware,
  GetLatestAppVersionResponse,
} from './nodes/types';

// Transactions Types
import type {
  Transaction,
  TransactionFee,
  GetTransactionFeeRequest,
  CreateTransactionRequest,
  DeleteTransactionRequest,
  GetTransactionsRequest,
  GetTransactionsResponse,
} from './transactions/types';

// Wallets Types
import type {
  AdaWallet,
  AdaWallets,
  LegacyAdaWallet,
  LegacyAdaWallets,
  WalletUtxos,
  WalletIdAndBalance,
  CreateWalletRequest,
  DeleteWalletRequest,
  RestoreWalletRequest,
  RestoreLegacyWalletRequest,
  UpdateSpendingPasswordRequest,
  ExportWalletToFileRequest,
  GetWalletCertificateRecoveryPhraseRequest,
  GetWalletRecoveryPhraseFromCertificateRequest,
  ImportWalletFromKeyRequest,
  ImportWalletFromFileRequest,
  UpdateWalletRequest,
  GetWalletUtxosRequest,
  GetWalletRequest,
  GetWalletIdAndBalanceRequest,
  GetWalletIdAndBalanceResponse,
  TransferFundsCalculateFeeRequest,
  TransferFundsCalculateFeeResponse,
  TransferFundsRequest,
  TransferFundsResponse,
} from './wallets/types';

// News Types
import type { GetNewsResponse } from './news/types';

// Staking Types
import type {
  JoinStakePoolRequest,
  GetDelegationFeeRequest,
  DelegationFee,
  AdaApiStakePools,
  AdaApiStakePool,
  QuitStakePoolRequest,
} from './staking/types';

// Common errors
import {
  GenericApiError,
  IncorrectSpendingPasswordError,
  InvalidMnemonicError,
  ForbiddenMnemonicError,
} from './common/errors';

// Wallets errors
import {
  WalletAlreadyRestoredError,
  WalletAlreadyImportedError,
  WalletFileImportError,
} from './wallets/errors';

// Transactions errors
import {
  NotAllowedToSendMoneyToRedeemAddressError,
  NotEnoughFundsForTransactionError,
  CanNotCalculateTransactionFeesError,
  NotEnoughFundsForTransactionFeesError,
  NotEnoughMoneyToSendError,
  TooBigTransactionError,
  InvalidAddressError,
} from './transactions/errors';
import type { FaultInjectionIpcRequest } from '../../../common/types/cardano-node.types';
import { TlsCertificateNotValidError } from './nodes/errors';
import { getSHA256HexForString } from './utils/hashing';
import { getNewsHash } from './news/requests/getNewsHash';
import { deleteTransaction } from './transactions/requests/deleteTransaction';

export default class AdaApi {
  config: RequestConfig;

  constructor(isTest: boolean, config: RequestConfig) {
    this.setRequestConfig(config);
    if (isTest) patchAdaApi(this);
  }

  setRequestConfig(config: RequestConfig) {
    this.config = config;
  }

  getWallets = async (): Promise<Array<Wallet>> => {
    Logger.debug('AdaApi::getWallets called');
    try {
      const wallets: AdaWallets = await getWallets(this.config);
      const legacyWallets: LegacyAdaWallets = await getLegacyWallets(
        this.config
      );
      Logger.debug('AdaApi::getWallets success', { wallets, legacyWallets });

      map(legacyWallets, legacyAdaWallet => {
        const extraLegacyWalletProps = {
          address_pool_gap: 0, // Not needed for legacy wallets
          delegation: WalletDelegationStatuses.NOT_DELEGATING,
          isLegacy: true,
        };
        wallets.push({
          ...legacyAdaWallet,
          ...extraLegacyWalletProps,
        });
      });

      return wallets.map(_createWalletFromServerData);
    } catch (error) {
      Logger.error('AdaApi::getWallets error', { error });
      throw new GenericApiError();
    }
  };

  getWallet = async (request: GetWalletRequest): Promise<Wallet> => {
    Logger.debug('AdaApi::getWallet called', {
      parameters: filterLogData(request),
    });
    try {
      const { walletId } = request;
      const wallet: AdaWallet = await getWallet(this.config, { walletId });
      Logger.debug('AdaApi::getWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      Logger.error('AdaApi::getWallet error', { error });
      throw new GenericApiError();
    }
  };

  getAddresses = async (
    request: GetAddressesRequest
  ): Promise<Array<WalletAddress>> => {
    Logger.debug('AdaApi::getAddresses called', {
      parameters: filterLogData(request),
    });

    const { walletId, queryParams, isLegacy } = request;
    try {
      let response = [];
      if (!isLegacy) {
        response = await getAddresses(this.config, walletId, queryParams);
      }
      Logger.debug('AdaApi::getAddresses success', { addresses: response });
      return response.map(_createAddressFromServerData);
    } catch (error) {
      Logger.error('AdaApi::getAddresses error', { error });
      throw new GenericApiError();
    }
  };

  getTransactions = async (
    request: GetTransactionsRequest
  ): Promise<GetTransactionsResponse> => {
    Logger.debug('AdaApi::searchHistory called', { parameters: request });
    const { walletId, order, fromDate, toDate, isLegacy } = request;

    const params = Object.assign(
      {},
      {
        order: order || 'descending',
      }
    );
    if (fromDate)
      params.start = `${moment.utc(fromDate).format('YYYY-MM-DDTHH:mm:ss')}Z`;
    if (toDate)
      params.end = `${moment.utc(toDate).format('YYYY-MM-DDTHH:mm:ss')}Z`;

    try {
      let response;
      if (isLegacy) {
        response = await getLegacyWalletTransactionHistory(
          this.config,
          walletId,
          params
        );
      } else {
        response = await getTransactionHistory(this.config, walletId, params);
      }
      const transactions = response.map(tx =>
        _createTransactionFromServerData(tx)
      );
      return new Promise(resolve =>
        resolve({ transactions, total: response.length })
      );
    } catch (error) {
      Logger.error('AdaApi::searchHistory error', { error });
      throw new GenericApiError();
    }

    // @API TODO - Filter / Search fine tunning "pending" for V2

    // const requestStats = Object.assign({}, request, {
    //   cachedTransactions: request.cachedTransactions.length,
    // });
    //  Logger.debug('AdaApi::searchHistory called', { parameters: requestStats });
    // const requestTimestamp = moment();
    // const params = {
    //   wallet_id: walletId,
    //   page: skip === 0 ? 1 : skip + 1,
    //   per_page: perPage,
    //   sort_by: 'DES[created_at]',
    //   created_at: `LTE[${moment.utc().format('YYYY-MM-DDTHH:mm:ss')}]`,
    //   // ^^ By setting created_at filter to current time we make sure
    //   // all subsequent multi-pages requests load the same set of transactions
    // };
    //
    //
    // const {
    //   walletId,
    //   skip,
    //   limit,
    //   isFirstLoad, // during first load we fetch all wallet's transactions
    //   isRestoreActive, // during restoration we fetch only missing transactions
    //   isRestoreCompleted, // once restoration is done we fetch potentially missing transactions
    //   cachedTransactions,
    // } , unionBy= request;
    //
    //
    // let perPage = limit;
    // const shouldLoadAll = limit === null;
    // if (shouldLoadAll || limit > MAX_TRANSACTIONS_PER_PAGE) {
    //   perPage = MAX_TRANSACTIONS_PER_PAGE;
    // }
    //
    // const params = {
    //   wallet_id: walletId,
    //   page: skip === 0 ? 1 : skip + 1,
    //   per_page: perPage,
    //   sort_by: 'DES[created_at]',
    //   created_at: `LTE[${moment.utc().format('YYYY-MM-DDTHH:mm:ss')}]`,
    //   // ^^ By setting created_at filter to current time we make sure
    //   // all subsequent multi-pages requests load the same set of transactions
    // };
    //
    // const shouldLoadOnlyFresh =
    //   !isFirstLoad && !isRestoreActive && !isRestoreCompleted;
    // if (shouldLoadOnlyFresh) {
    //   const tenMinutesAgo = moment
    //     .utc(Date.now() - TX_AGE_POLLING_THRESHOLD)
    //     .format('YYYY-MM-DDTHH:mm:ss');
    //   // Since we load all transactions in a first load, later on we only care about fresh ones
    //   Object.assign(params, { created_at: `GTE[${tenMinutesAgo}]` });
    // }
    //
    // const pagesToBeLoaded = Math.ceil(limit / params.per_page);
    //
    // try {
    //   // Load first page of transactions
    //   const response: Transactions = await getTransactionHistory(
    //     this.config,
    //     params
    //   );
    //   const { meta, data: txHistory } = response;
    //   const { totalPages, totalEntries: totalTransactions } = meta.pagination;
    //
    //   let transactions = txHistory.map(tx =>
    //     _createTransactionFromServerData(tx)
    //   );
    //
    //   // Load additional pages of transactions
    //   const hasMultiplePages =
    //     totalPages > 1 && (shouldLoadAll || limit > perPage);
    //   if (hasMultiplePages) {
    //     let page = 2;
    //     const hasNextPage = () => {
    //       const hasMorePages = page < totalPages + 1;
    //       if ((isRestoreActive || isRestoreCompleted) && hasMorePages) {
    //         const loadedTransactions = unionBy(
    //           transactions,
    //           cachedTransactions,
    //           'id'
    //         );
    //         const hasMoreTransactions =
    //           totalTransactions - loadedTransactions.length > 0;
    //         return hasMoreTransactions;
    //       }
    //       return hasMorePages;
    //     };
    //     const shouldLoadNextPage = () =>
    //       shouldLoadAll || page <= pagesToBeLoaded;
    //
    //     if (isRestoreActive || isRestoreCompleted) {
    //       const latestLoadedTransactionDate = transactions[0].date;
    //       const latestLoadedTransactionDateString = moment
    //         .utc(latestLoadedTransactionDate)
    //         .format('YYYY-MM-DDTHH:mm:ss');
    //       // During restoration we need to fetch only transactions older than the latest loaded one
    //       // as this ensures that both totalPages and totalEntries remain unchanged throught out
    //       // subsequent page loads (as in the meantime new transactions can be discovered)
    //       Object.assign(params, {
    //         created_at: `LTE[${latestLoadedTransactionDateString}]`,
    //       });
    //     }
    //
    //     for (page; hasNextPage() && shouldLoadNextPage(); page++) {
    //       const { data: pageHistory } = await getTransactionHistory(
    //         this.config,
    //         Object.assign(params, { page })
    //       );
    //       transactions.push(
    //         ...pageHistory.map(tx => _createTransactionFromServerData(tx))
    //       );
    //     }
    //   }
    //
    //   // Merge newly loaded and previously loaded transactions
    //   // - unionBy also serves the purpose of removing transaction duplicates
    //   //   which may occur as a side-effect of transaction request pagination
    //   //   as multi-page requests are not executed at the exact same time!
    //   transactions = unionBy(transactions, cachedTransactions, 'id');
    //
    //   // Enforce the limit in case we are not loading all transactions
    //   if (!shouldLoadAll) transactions.splice(limit);
    //
    //   const total = transactions.length;
    //
    //   const responseStats = {
    //     apiRequested: limit || 'all',
    //     apiFiltered: shouldLoadOnlyFresh ? 'fresh' : '',
    //     apiReturned: totalTransactions,
    //     apiPagesTotal: totalPages,
    //     apiPagesRequested: params.page,
    //     daedalusCached: cachedTransactions.length,
    //     daedalusLoaded: total - cachedTransactions.length,
    //     daedalusTotal: total,
    //     requestDurationInMs: moment
    //       .duration(moment().diff(requestTimestamp))
    //       .as('milliseconds'),
    //   };
    //   Logger.debug(
    //     `AdaApi::searchHistory success: ${total} transactions loaded`,
    //     { responseStats }
    //   );
    //   return new Promise(resolve => resolve({ transactions, total }));
    // } catch (error) {
    //   Logger.error('AdaApi::searchHistory error', { error });
    //   throw new GenericApiError();
    // }
  };

  createWallet = async (request: CreateWalletRequest): Promise<Wallet> => {
    Logger.debug('AdaApi::createWallet called', {
      parameters: filterLogData(request),
    });
    const { name, mnemonic, spendingPassword } = request;
    try {
      const walletInitData = {
        name,
        mnemonic_sentence: split(mnemonic, ' '),
        passphrase: spendingPassword,
      };
      const wallet: AdaWallet = await createWallet(this.config, {
        walletInitData,
      });
      Logger.debug('AdaApi::createWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      Logger.error('AdaApi::createWallet error', { error });
      throw new GenericApiError();
    }
  };

  deleteWallet = async (request: DeleteWalletRequest): Promise<boolean> => {
    Logger.debug('AdaApi::deleteWallet called', {
      parameters: filterLogData(request),
    });
    try {
      const { walletId, isLegacy } = request;
      let response;
      if (isLegacy) {
        response = await deleteLegacyWallet(this.config, { walletId });
      } else {
        response = await deleteWallet(this.config, { walletId });
      }
      Logger.debug('AdaApi::deleteWallet success', { response });
      return true;
    } catch (error) {
      Logger.error('AdaApi::deleteWallet error', { error });
      throw new GenericApiError();
    }
  };

  createTransaction = async (
    request: CreateTransactionRequest
  ): Promise<WalletTransaction> => {
    Logger.debug('AdaApi::createTransaction called', {
      parameters: filterLogData(request),
    });
    const { walletId, address, amount, passphrase } = request;

    try {
      const data = {
        payments: [
          {
            address,
            amount: {
              quantity: amount,
              unit: WalletUnits.LOVELACE,
            },
          },
        ],
        passphrase,
      };

      const response: Transaction = await createTransaction(this.config, {
        walletId,
        data,
      });

      Logger.debug('AdaApi::createTransaction success', {
        transaction: response,
      });

      return _createTransactionFromServerData(response);
    } catch (error) {
      Logger.error('AdaApi::createTransaction error', { error });
      if (error.code === 'output_is_redeem') {
        throw new NotAllowedToSendMoneyToRedeemAddressError();
      }
      if (error.code === 'cannot_cover_fee') {
        throw new NotEnoughFundsForTransactionFeesError();
      }
      if (error.code === 'not_enough_money') {
        throw new NotEnoughMoneyToSendError();
      }
      if (error.code === 'wrong_encryption_passphrase') {
        throw new IncorrectSpendingPasswordError();
      }
      if (error.code === 'too_big_transaction') {
        throw new TooBigTransactionError();
      }
      throw new GenericApiError();
    }
  };

  calculateTransactionFee = async (
    request: GetTransactionFeeRequest
  ): Promise<BigNumber> => {
    Logger.debug('AdaApi::calculateTransactionFee called', {
      parameters: filterLogData(request),
    });
    const {
      walletId,
      address,
      amount,
      walletBalance,
      availableBalance,
    } = request;

    try {
      const data = {
        payments: [
          {
            address,
            amount: {
              quantity: amount,
              unit: WalletUnits.LOVELACE,
            },
          },
        ],
      };

      const response: TransactionFee = await getTransactionFee(this.config, {
        walletId,
        data,
      });

      const formattedTxAmount = new BigNumber(amount).dividedBy(
        LOVELACES_PER_ADA
      );
      const fee = _createTransactionFeeFromServerData(response);
      const amountWithFee = formattedTxAmount.plus(fee);
      if (amountWithFee.gt(walletBalance)) {
        // Amount + fees exceeds walletBalance:
        // = show "Not enough Ada for fees. Try sending a smaller amount."
        throw new NotEnoughFundsForTransactionFeesError();
      }

      Logger.debug('AdaApi::calculateTransactionFee success', {
        transactionFee: response,
      });
      return fee;
    } catch (error) {
      Logger.error('AdaApi::calculateTransactionFee error', { error });
      if (error.name === 'NotEnoughFundsForTransactionFeesError') {
        throw error;
      } else if (error.code === 'not_enough_money') {
        if (walletBalance.gt(availableBalance)) {
          // Amount exceeds availableBalance due to pending transactions:
          // - error.diagnostic.details.msg === 'Not enough available coins to proceed.'
          // - total walletBalance > error.diagnostic.details.availableBalance
          // = show "Cannot calculate fees while there are pending transactions."
          throw new CanNotCalculateTransactionFeesError();
        } else {
          // Amount exceeds walletBalance:
          // - error.diagnostic.details.msg === 'Not enough available coins to proceed.'
          // - total walletBalance === error.diagnostic.details.availableBalance
          // = show "Not enough Ada. Try sending a smaller amount."
          throw new NotEnoughFundsForTransactionError();
        }
      } else if (
        error.code === 'bad_request' &&
        includes(error.message, 'Unable to decode Address')
      ) {
        throw new InvalidAddressError();
      } else {
        throw new GenericApiError();
      }
    }
  };

  deleteTransaction = async (
    request: DeleteTransactionRequest
  ): Promise<void> => {
    Logger.debug('AdaApi::deleteTransaction called', { parameters: request });
    const { walletId, transactionId, isLegacy } = request;
    try {
      let response;
      if (isLegacy) {
        response = await deleteLegacyTransaction(this.config, {
          walletId,
          transactionId,
        });
      } else {
        response = await deleteTransaction(this.config, {
          walletId,
          transactionId,
        });
      }
      Logger.debug('AdaApi::deleteTransaction success', response);
    } catch (error) {
      Logger.error('AdaApi::deleteTransaction error', { error });
      // In this particular call we don't need to handle the error in the UI
      // The only reason transaction canceling would fail is if the transaction
      // is no longer pending - in which case there is nothign we can do.
    }
  };

  isValidCertificateMnemonic = (mnemonic: string): boolean =>
    mnemonic.split(' ').length === ADA_CERTIFICATE_MNEMONIC_LENGTH;

  getWalletRecoveryPhrase(): Promise<Array<string>> {
    Logger.debug('AdaApi::getWalletRecoveryPhrase called');
    try {
      const response: Promise<Array<string>> = new Promise(resolve =>
        resolve(generateAccountMnemonics())
      );
      Logger.debug('AdaApi::getWalletRecoveryPhrase success');
      return response;
    } catch (error) {
      Logger.error('AdaApi::getWalletRecoveryPhrase error', { error });
      throw new GenericApiError();
    }
  }

  getWalletCertificateAdditionalMnemonics(): Promise<Array<string>> {
    Logger.debug('AdaApi::getWalletCertificateAdditionalMnemonics called');
    try {
      const response: Promise<Array<string>> = new Promise(resolve =>
        resolve(generateAdditionalMnemonics())
      );
      Logger.debug('AdaApi::getWalletCertificateAdditionalMnemonics success');
      return response;
    } catch (error) {
      Logger.error('AdaApi::getWalletCertificateAdditionalMnemonics error', {
        error,
      });
      throw new GenericApiError();
    }
  }

  getWalletCertificateRecoveryPhrase(
    request: GetWalletCertificateRecoveryPhraseRequest
  ): Promise<Array<string>> {
    Logger.debug('AdaApi::getWalletCertificateRecoveryPhrase called');
    const { passphrase, input: scrambledInput } = request;
    try {
      const response: Promise<Array<string>> = new Promise(resolve =>
        resolve(scrambleMnemonics({ passphrase, scrambledInput }))
      );
      Logger.debug('AdaApi::getWalletCertificateRecoveryPhrase success');
      return response;
    } catch (error) {
      Logger.error('AdaApi::getWalletCertificateRecoveryPhrase error', {
        error,
      });
      throw new GenericApiError();
    }
  }

  getWalletRecoveryPhraseFromCertificate(
    request: GetWalletRecoveryPhraseFromCertificateRequest
  ): Promise<Array<string>> {
    Logger.debug('AdaApi::getWalletRecoveryPhraseFromCertificate called');
    const { passphrase, scrambledInput } = request;
    try {
      const response = unscrambleMnemonics({ passphrase, scrambledInput });
      Logger.debug('AdaApi::getWalletRecoveryPhraseFromCertificate success');
      return Promise.resolve(response);
    } catch (error) {
      Logger.error('AdaApi::getWalletRecoveryPhraseFromCertificate error', {
        error,
      });
      return Promise.reject(new InvalidMnemonicError());
    }
  }

  restoreWallet = async (request: RestoreWalletRequest): Promise<Wallet> => {
    Logger.debug('AdaApi::restoreWallet called', {
      parameters: filterLogData(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: split(recoveryPhrase, ' '),
      passphrase: spendingPassword,
    };
    try {
      const wallet: AdaWallet = await restoreWallet(this.config, {
        walletInitData,
      });
      Logger.debug('AdaApi::restoreWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      Logger.error('AdaApi::restoreWallet error', { error });
      if (error.code === 'wallet_already_exists') {
        throw new WalletAlreadyRestoredError();
      }
      // @API TOOD - improve once error is handled by v2 API (REPORT to BE team)
      if (error.message === 'JSONValidationFailed') {
        const validationError = get(error, 'diagnostic.validationError', '');
        if (
          validationError.includes(
            'Forbidden Mnemonic: an example Mnemonic has been submitted'
          )
        ) {
          throw new ForbiddenMnemonicError();
        }
      }
      throw new GenericApiError();
    }
  };

  restoreLegacyWallet = async (
    request: RestoreLegacyWalletRequest
  ): Promise<Wallet> => {
    Logger.debug('AdaApi::restoreLegacyWallet called', {
      parameters: filterLogData(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: split(recoveryPhrase, ' '),
      passphrase: spendingPassword,
    };
    try {
      const legacyWallet: LegacyAdaWallet = await restoreLegacyWallet(
        this.config,
        {
          walletInitData,
        }
      );
      const extraLegacyWalletProps = {
        address_pool_gap: 0, // Not needed for legacy wallets
        delegation: WalletDelegationStatuses.NOT_DELEGATING,
        isLegacy: true,
      };
      const wallet = {
        ...legacyWallet,
        ...extraLegacyWalletProps,
      };
      Logger.debug('AdaApi::restoreLegacyWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      Logger.error('AdaApi::restoreLegacyWallet error', { error });
      if (error.code === 'wallet_already_exists') {
        throw new WalletAlreadyRestoredError();
      }
      // @API TOOD - improve once error is handled by v2 API (REPORT to BE team)
      if (error.message === 'JSONValidationFailed') {
        const validationError = get(error, 'diagnostic.validationError', '');
        if (
          validationError.includes(
            'Forbidden Mnemonic: an example Mnemonic has been submitted'
          )
        ) {
          throw new ForbiddenMnemonicError();
        }
      }
      throw new GenericApiError();
    }
  };

  importWalletFromKey = async (
    request: ImportWalletFromKeyRequest
  ): Promise<Wallet> => {
    Logger.debug('AdaApi::importWalletFromKey called', {
      parameters: filterLogData(request),
    });
    const { filePath, spendingPassword } = request;
    try {
      const importedWallet: AdaWallet = await importWalletAsKey(this.config, {
        filePath,
        spendingPassword: spendingPassword || '',
      });
      Logger.debug('AdaApi::importWalletFromKey success', { importedWallet });
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      Logger.error('AdaApi::importWalletFromKey error', { error });
      if (error.message === 'WalletAlreadyExists') {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  };

  importWalletFromFile = async (
    request: ImportWalletFromFileRequest
  ): Promise<Wallet> => {
    Logger.debug('AdaApi::importWalletFromFile called', {
      parameters: filterLogData(request),
    });
    const { filePath, spendingPassword } = request;
    const isKeyFile =
      filePath
        .split('.')
        .pop()
        .toLowerCase() === 'key';
    try {
      const importedWallet: AdaWallet = isKeyFile
        ? await importWalletAsKey(this.config, {
            filePath,
            spendingPassword,
          })
        : await importWalletAsJSON(this.config, filePath);
      Logger.debug('AdaApi::importWalletFromFile success', { importedWallet });
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      Logger.error('AdaApi::importWalletFromFile error', { error });
      if (error.message === 'WalletAlreadyExists') {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  };

  nextUpdate = async (): Promise<NodeSoftware | null> => {
    Logger.debug('AdaApi::nextUpdate called');

    /* TODO: Re-enable when API is available
    try {
      const nodeUpdate = await getNextNodeUpdate(this.config);
      if (nodeUpdate && nodeUpdate.version) {
        Logger.debug('AdaApi::nextUpdate success', { nodeUpdate });
        return nodeUpdate;
      }
      Logger.debug('AdaApi::nextUpdate success: No Update Available');
    } catch (error) {
      Logger.error('AdaApi::nextUpdate error', { error });
      throw new GenericApiError();
    }
    */

    return null;
  };

  postponeUpdate = async (): Promise<void> => {
    Logger.debug('AdaApi::postponeUpdate called');
    try {
      const response: Promise<any> = await postponeNodeUpdate(this.config);
      Logger.debug('AdaApi::postponeUpdate success', { response });
    } catch (error) {
      Logger.error('AdaApi::postponeUpdate error', { error });
      throw new GenericApiError();
    }
  };

  applyUpdate = async (): Promise<void> => {
    Logger.debug('AdaApi::applyUpdate called');
    try {
      await awaitUpdateChannel.send();
      const response: Promise<any> = await applyNodeUpdate(this.config);
      Logger.debug('AdaApi::applyUpdate success', { response });
    } catch (error) {
      Logger.error('AdaApi::applyUpdate error', { error });
      throw new GenericApiError();
    }
  };

  updateWallet = async (request: UpdateWalletRequest): Promise<Wallet> => {
    Logger.debug('AdaApi::updateWallet called', {
      parameters: filterLogData(request),
    });
    const { walletId, name } = request;
    try {
      const wallet: AdaWallet = await updateWallet(this.config, {
        walletId,
        name,
      });
      Logger.debug('AdaApi::updateWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      Logger.error('AdaApi::updateWallet error', { error });
      throw new GenericApiError();
    }
  };

  updateSpendingPassword = async (
    request: UpdateSpendingPasswordRequest
  ): Promise<boolean> => {
    Logger.debug('AdaApi::updateSpendingPassword called', {
      parameters: filterLogData(request),
    });
    const { walletId, oldPassword, newPassword } = request;
    try {
      await changeSpendingPassword(this.config, {
        walletId,
        oldPassword,
        newPassword,
      });
      Logger.debug('AdaApi::updateSpendingPassword success');
      return true;
    } catch (error) {
      Logger.error('AdaApi::updateSpendingPassword error', { error });
      const errorCode = get(error, 'code', '');
      if (
        errorCode === 'wrong_encryption_passphrase' ||
        errorCode === 'bad_request'
      ) {
        throw new IncorrectSpendingPasswordError();
      }
      throw new GenericApiError();
    }
  };

  quitStakePool = async (
    request: QuitStakePoolRequest
  ): Promise<Transaction> => {
    Logger.debug('AdaApi::quitStakePool called', {
      parameters: filterLogData(request),
    });
    const { stakePoolId, walletId, passphrase } = request;
    try {
      const result = await quitStakePool(this.config, {
        stakePoolId,
        walletId,
        passphrase,
      });
      Logger.debug('AdaApi::quitStakePool success');
      return result;
    } catch (error) {
      Logger.error('AdaApi::quitStakePool error', { error });
      if (error.code === 'wrong_encryption_passphrase') {
        throw new IncorrectSpendingPasswordError();
      }
      throw new GenericApiError();
    }
  };

  exportWalletToFile = async (
    request: ExportWalletToFileRequest
  ): Promise<[]> => {
    const { walletId, filePath } = request;
    Logger.debug('AdaApi::exportWalletToFile called', {
      parameters: filterLogData(request),
    });
    try {
      const response: Promise<[]> = await exportWalletAsJSON(this.config, {
        walletId,
        filePath,
      });
      Logger.debug('AdaApi::exportWalletToFile success', { response });
      return response;
    } catch (error) {
      Logger.error('AdaApi::exportWalletToFile error', { error });
      throw new GenericApiError();
    }
  };

  getWalletUtxos = async (
    request: GetWalletUtxosRequest
  ): Promise<WalletUtxos> => {
    const { walletId } = request;
    Logger.debug('AdaApi::getWalletUtxos called', {
      parameters: filterLogData(request),
    });
    try {
      const response: Promise<WalletUtxos> = await getWalletUtxos(this.config, {
        walletId,
      });
      Logger.debug('AdaApi::getWalletUtxos success', { response });
      return response;
    } catch (error) {
      Logger.error('AdaApi::getWalletUtxos error', { error });
      throw new GenericApiError();
    }
  };

  getWalletIdAndBalance = async (
    request: GetWalletIdAndBalanceRequest
  ): Promise<WalletIdAndBalance> => {
    const { recoveryPhrase, getBalance } = request;
    Logger.debug('AdaApi::getWalletIdAndBalance called', {
      parameters: { getBalance },
    });
    try {
      const response: GetWalletIdAndBalanceResponse = await getWalletIdAndBalance(
        this.config,
        {
          recoveryPhrase,
          getBalance,
        }
      );
      Logger.debug('AdaApi::getWalletIdAndBalance success', { response });
      const { walletId, balance } = response;
      return {
        walletId,
        balance:
          balance !== null // If balance is "null" it means we didn't fetch it - getBalance was false
            ? new BigNumber(balance).dividedBy(LOVELACES_PER_ADA)
            : null,
      };
    } catch (error) {
      Logger.error('AdaApi::getWalletIdAndBalance error', { error });
      throw new GenericApiError();
    }
  };

  transferFundsCalculateFee = async (
    request: TransferFundsCalculateFeeRequest
  ): Promise<BigNumber> => {
    const { sourceWalletId } = request;
    Logger.debug('AdaApi::transferFundsCalculateFee called', {
      parameters: { sourceWalletId },
    });
    try {
      const response: TransferFundsCalculateFeeResponse = await transferFundsCalculateFee(
        this.config,
        {
          sourceWalletId,
        }
      );
      Logger.debug('AdaApi::transferFundsCalculateFee success', { response });
      return _createMigrationFeeFromServerData(response);
    } catch (error) {
      Logger.error('AdaApi::transferFundsCalculateFee error', { error });
      throw new GenericApiError();
    }
  };

  transferFunds = async (
    request: TransferFundsRequest
  ): Promise<TransferFundsResponse> => {
    const { sourceWalletId, targetWalletId, passphrase } = request;
    Logger.debug('AdaApi::transferFunds called', {
      parameters: { sourceWalletId, targetWalletId },
    });

    try {
      const response: TransferFundsResponse = await transferFunds(this.config, {
        sourceWalletId,
        targetWalletId,
        passphrase,
      });
      Logger.debug('AdaApi::transferFunds success', { response });
      return response;
    } catch (error) {
      Logger.error('AdaApi::transferFunds error', { error });
      if (error.code === 'wrong_encryption_passphrase') {
        throw new IncorrectSpendingPasswordError();
      }
      throw new GenericApiError();
    }
  };

  getStakePools = async (): Promise<Array<StakePool>> => {
    Logger.debug('AdaApi::getStakePools called');
    try {
      const stakePools: AdaApiStakePools = await getStakePools(this.config);
      Logger.debug('AdaApi::getStakePools success');
      return (
        stakePools
          // @API TODO: Filter Stake Pools without metadata, once metadata is present in the API response
          // .filter(({ metadata }: AdaApiStakePool) => metadata !== undefined)
          .map(_createStakePoolFromServerData)
      );
    } catch (error) {
      Logger.error('AdaApi::getStakePools error', { error });
      throw new GenericApiError();
    }
  };

  testReset = async (): Promise<void> => {
    Logger.debug('AdaApi::testReset called');
    try {
      const wallets: AdaWallets = await getWallets(this.config);
      wallets.map(wallet => deleteWallet(this.config, { walletId: wallet.id }));
      const legacyWallets: LegacyAdaWallets = await getLegacyWallets(
        this.config
      );
      legacyWallets.map(wallet =>
        deleteLegacyWallet(this.config, { walletId: wallet.id })
      );
      Logger.debug('AdaApi::testReset success');
    } catch (error) {
      Logger.error('AdaApi::testReset error', { error });
      throw new GenericApiError();
    }
  };

  getNetworkInfo = async (): Promise<GetNetworkInfoResponse> => {
    Logger.debug('AdaApi::getNetworkInfo called');
    try {
      const networkInfo: NetworkInfoResponse = await getNetworkInfo(
        this.config
      );
      Logger.debug('AdaApi::getNetworkInfo success', { networkInfo });

      /* eslint-disable-next-line camelcase */
      const { sync_progress, node_tip, network_tip, next_epoch } = networkInfo;

      const syncProgress =
        get(sync_progress, 'status') === 'ready'
          ? 100
          : get(sync_progress, 'progress.quantity', 0);

      // extract relevant data before sending to NetworkStatusStore
      return {
        syncProgress,
        localTip: {
          epoch: get(node_tip, 'epoch_number', 0),
          slot: get(node_tip, 'slot_number', 0),
        },
        networkTip: {
          epoch: get(network_tip, 'epoch_number', 0),
          slot: get(network_tip, 'slot_number', 0),
        },
        nextEpoch: {
          epochNumber: get(next_epoch, 'epoch_number', 0),
          epochStart: get(next_epoch, 'epoch_start_time', ''),
        },
        futureEpoch: {
          epochNumber: get(next_epoch, 'epoch_number', 0) + 1,
          epochStart: moment(get(next_epoch, 'epoch_start_time', 0))
            .add(EPOCH_LENGTH_ITN, 'seconds')
            .toISOString(),
        },
      };
    } catch (error) {
      Logger.error('AdaApi::getNetworkInfo error', { error });
      // @API TODO - Inspect this implementation once TLS support is implemented on the BE
      if (error.code === TlsCertificateNotValidError.API_ERROR) {
        throw new TlsCertificateNotValidError();
      }
      throw new GenericApiError(error);
    }
  };

  getLatestAppVersion = async (): Promise<GetLatestAppVersionResponse> => {
    Logger.debug('AdaApi::getLatestAppVersion called');
    try {
      const { isWindows, platform } = global.environment;
      const latestAppVersionInfo: LatestAppVersionInfoResponse = await getLatestAppVersion();

      const latestAppVersionPath = `platforms.${
        isWindows ? 'windows' : platform
      }.version`;

      const applicationVersionPath = `platforms.${
        isWindows ? 'windows' : platform
      }.applicationVersion`;

      const latestAppVersion = get(
        latestAppVersionInfo,
        latestAppVersionPath,
        null
      );

      const applicationVersion = get(
        latestAppVersionInfo,
        applicationVersionPath,
        null
      );
      Logger.debug('AdaApi::getLatestAppVersion success', {
        latestAppVersion,
        latestAppVersionInfo,
        applicationVersion,
      });
      return { latestAppVersion, applicationVersion };
    } catch (error) {
      Logger.error('AdaApi::getLatestAppVersion error', { error });
      throw new GenericApiError();
    }
  };

  getNews = async (): Promise<GetNewsResponse> => {
    Logger.debug('AdaApi::getNews called');

    // Fetch news json
    let rawNews: string;
    let news: GetNewsResponse;
    try {
      rawNews = await getNews();
      news = JSON.parse(rawNews);
    } catch (error) {
      Logger.error('AdaApi::getNews error', { error });
      throw new Error('Unable to fetch news');
    }

    // Fetch news verification hash
    let newsHash: string;
    let expectedNewsHash: string;
    try {
      newsHash = await getSHA256HexForString(rawNews);
      expectedNewsHash = await getNewsHash(news.updatedAt);
    } catch (error) {
      Logger.error('AdaApi::getNews (hash) error', { error });
      throw new Error('Unable to fetch news hash');
    }

    if (newsHash !== expectedNewsHash) {
      throw new Error('Newsfeed could not be verified');
    }

    Logger.debug('AdaApi::getNews success', {
      updatedAt: news.updatedAt,
      items: news.items.length,
    });
    return news;
  };

  calculateDelegationFee = async (
    request: GetDelegationFeeRequest
  ): Promise<BigNumber> => {
    Logger.debug('AdaApi::calculateDelegationFee called', {
      parameters: filterLogData(request),
    });

    try {
      const response: DelegationFee = await getDelegationFee(this.config, {
        walletId: request.walletId,
      });
      const delegationFee = _createDelegationFeeFromServerData(response);
      return delegationFee;
    } catch (error) {
      Logger.error('AdaApi::calculateDelegationFee error', { error });
      throw new GenericApiError();
    }
  };

  joinStakePool = async (
    request: JoinStakePoolRequest
  ): Promise<Transaction> => {
    Logger.debug('AdaApi::joinStakePool called', {
      parameters: filterLogData(request),
    });
    const { walletId, stakePoolId, passphrase } = request;

    try {
      const response = await joinStakePool(this.config, {
        walletId,
        stakePoolId,
        passphrase,
      });

      Logger.debug('AdaApi::joinStakePool success', {
        stakePool: response,
      });

      return response;
    } catch (error) {
      Logger.error('AdaApi::joinStakePool error', { error });
      if (error.code === 'wrong_encryption_passphrase') {
        throw new IncorrectSpendingPasswordError();
      }
      throw new GenericApiError();
    }
  };

  setCardanoNodeFault = async (fault: FaultInjectionIpcRequest) => {
    await cardanoFaultInjectionChannel.send(fault);
  };

  // No implementation here but can be overwritten
  setLocalTimeDifference: Function;
  setSyncProgress: Function;
  setNextUpdate: Function;
  setLatestAppVersion: Function;
  setApplicationVersion: Function;
  setFaultyNodeSettingsApi: boolean;
  resetTestOverrides: Function;

  // Newsfeed testing utility
  setFakeNewsFeedJsonForTesting: (fakeNewsfeedJson: GetNewsResponse) => void;
}

// ========== TRANSFORM SERVER DATA INTO FRONTEND MODELS =========

const _createWalletFromServerData = action(
  'AdaApi::_createWalletFromServerData',
  (data: AdaWallet) => {
    const {
      id,
      address_pool_gap: addressPoolGap,
      balance,
      name,
      state,
      passphrase,
      delegation,
      isLegacy = false,
    } = data;
    const passphraseLastUpdatedAt = get(passphrase, 'last_updated_at', null);
    const walletTotalAmount =
      balance.total.unit === WalletUnits.LOVELACE
        ? new BigNumber(balance.total.quantity).dividedBy(LOVELACES_PER_ADA)
        : new BigNumber(balance.total.quantity);
    const walletAvailableAmount =
      balance.available.unit === WalletUnits.LOVELACE
        ? new BigNumber(balance.available.quantity).dividedBy(LOVELACES_PER_ADA)
        : new BigNumber(balance.available.quantity);
    let walletRewardAmount = 0;
    if (!isLegacy) {
      walletRewardAmount =
        balance.reward.unit === WalletUnits.LOVELACE
          ? new BigNumber(balance.reward.quantity).dividedBy(LOVELACES_PER_ADA)
          : new BigNumber(balance.reward.quantity);
    }
    const delegatedStakePoolId = isLegacy ? null : delegation.target;

    return new Wallet({
      id,
      addressPoolGap,
      name,
      amount: walletTotalAmount,
      availableAmount: walletAvailableAmount,
      reward: walletRewardAmount,
      passwordUpdateDate:
        passphraseLastUpdatedAt && new Date(passphraseLastUpdatedAt),
      syncState: state,
      isLegacy,
      delegatedStakePoolId,
    });
  }
);

const _createAddressFromServerData = action(
  'AdaApi::_createAddressFromServerData',
  (address: Address) => {
    const { id, state } = address;
    return new WalletAddress({
      id,
      used: state === 'used',
    });
  }
);

const _conditionToTxState = (condition: string) =>
  TransactionStates[condition === 'pending' ? 'PENDING' : 'OK'];

const _createTransactionFromServerData = action(
  'AdaApi::_createTransactionFromServerData',
  (data: Transaction) => {
    const {
      id,
      amount,
      inserted_at, // eslint-disable-line camelcase
      pending_since, // eslint-disable-line camelcase
      depth,
      direction,
      inputs,
      outputs,
      status,
    } = data;
    const state = _conditionToTxState(status);
    const stateInfo =
      state === TransactionStates.PENDING ? pending_since : inserted_at; // eslint-disable-line
    const date = get(stateInfo, 'time');
    const slotNumber = get(stateInfo, ['block', 'slot_number'], null);
    const epochNumber = get(stateInfo, ['block', 'epoch_number'], null);
    return new WalletTransaction({
      id,
      depth,
      slotNumber,
      epochNumber,
      title: direction === 'outgoing' ? 'Ada sent' : 'Ada received',
      type:
        direction === 'outgoing'
          ? TransactionTypes.EXPEND
          : TransactionTypes.INCOME,
      amount: new BigNumber(
        direction === 'outgoing' ? amount.quantity * -1 : amount.quantity
      ).dividedBy(LOVELACES_PER_ADA),
      date: utcStringToDate(date),
      description: '',
      addresses: {
        from: inputs.map(({ address }) => address || null),
        to: outputs.map(({ address }) => address),
      },
      state,
    });
  }
);

const _createTransactionFeeFromServerData = action(
  'AdaApi::_createTransactionFeeFromServerData',
  (data: TransactionFee) => {
    const amount = get(data, ['amount', 'quantity'], 0);
    return new BigNumber(amount).dividedBy(LOVELACES_PER_ADA);
  }
);

const _createMigrationFeeFromServerData = action(
  'AdaApi::_createTransactionFeeFromServerData',
  (data: TransactionFee) => {
    const amount = get(data, ['migration_cost', 'quantity'], 0);
    return new BigNumber(amount).dividedBy(LOVELACES_PER_ADA);
  }
);

const _createStakePoolFromServerData = action(
  'AdaApi::_createStakePoolFromServerData',
  (stakePool: AdaApiStakePool, index: number) => {
    // DATA FROM THE API
    const { id, metrics, apparent_performance: performance } = stakePool;
    let {
      controlled_stake: controlledStake,
      produced_blocks: producedBlocks,
    } = metrics; // eslint-disable-line
    const {
      // MISSING DATA FROM THE API
      // IT IS CONTAINED IN THE DOCS:
      metadata,
      // MISSING DATA FROM THE API
      // NOT CONTAINED IN THE CURRENT API DOCS:
      // _cost: cost,
      _createdAt: createdAt,
      _isCharity: isCharity,
      // _pledge: pledge,
      _profitMargin: profitMargin,
      // _ranking: ranking,
      _retiring: retiring,
    } = stakingStakePoolsMissingApiData[index];
    const {
      name,
      description,
      ticker,
      homepage,
      pledge_address: pledgeAddress,
    } = metadata;
    controlledStake = controlledStake.quantity;
    producedBlocks = producedBlocks.quantity;
    return new StakePool({
      id,
      performance: performance * 100, // Percentage!
      controlledStake,
      producedBlocks,
      ticker,
      homepage,
      pledgeAddress,

      // cost: new BigNumber(cost).dividedBy(LOVELACES_PER_ADA),
      createdAt,
      description,
      isCharity,
      name,
      // pledge: new BigNumber(pledge).dividedBy(LOVELACES_PER_ADA),
      profitMargin,
      ranking: index + 1,
      retiring,
    });
  }
);

const _createDelegationFeeFromServerData = action(
  'AdaApi::_createDelegationFeeFromServerData',
  (data: DelegationFee) => {
    const amount = get(data, ['amount', 'quantity'], 0);
    return new BigNumber(amount).dividedBy(LOVELACES_PER_ADA);
  }
);
