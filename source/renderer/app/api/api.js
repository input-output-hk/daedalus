// @flow
import { split, get, includes, map, last } from 'lodash';
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
import { createByronWalletAddress } from './addresses/requests/createByronWalletAddress';

// Network requests
import { getNetworkInfo } from './network/requests/getNetworkInfo';

// Nodes requests
import { applyNodeUpdate } from './nodes/requests/applyNodeUpdate';
// import { getNextNodeUpdate } from './nodes/requests/getNextNodeUpdate';
import { postponeNodeUpdate } from './nodes/requests/postponeNodeUpdate';
import { getLatestAppVersion } from './nodes/requests/getLatestAppVersion';

// Transactions requests
import { getTransactionFee } from './transactions/requests/getTransactionFee';
import { getByronWalletTransactionFee } from './transactions/requests/getByronWalletTransactionFee';
import { getTransactionHistory } from './transactions/requests/getTransactionHistory';
import { getLegacyWalletTransactionHistory } from './transactions/requests/getLegacyWalletTransactionHistory';
import { createTransaction } from './transactions/requests/createTransaction';
import { createByronWalletTransaction } from './transactions/requests/createByronWalletTransaction';
import { deleteLegacyTransaction } from './transactions/requests/deleteLegacyTransaction';

// Wallets requests
import { updateSpendingPassword } from './wallets/requests/updateSpendingPassword';
import { updateByronSpendingPassword } from './wallets/requests/updateByronSpendingPassword';
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
import { restoreByronWallet } from './wallets/requests/restoreByronWallet';
import { restoreExportedByronWallet } from './wallets/requests/restoreExportedByronWallet';
import { updateWallet } from './wallets/requests/updateWallet';
import { updateByronWallet } from './wallets/requests/updateByronWallet';
import { forceWalletResync } from './wallets/requests/forceWalletResync';
import { forceLegacyWalletResync } from './wallets/requests/forceLegacyWalletResync';
import { getWalletUtxos } from './wallets/requests/getWalletUtxos';
import { getByronWalletUtxos } from './wallets/requests/getByronWalletUtxos';
import { getWallet } from './wallets/requests/getWallet';
import { getLegacyWallet } from './wallets/requests/getLegacyWallet';
import { getWalletIdAndBalance } from './wallets/requests/getWalletIdAndBalance';
import { transferFundsCalculateFee } from './wallets/requests/transferFundsCalculateFee';
import { transferFunds } from './wallets/requests/transferFunds';

// Staking
import StakePool from '../domains/StakePool';
import { EPOCH_LENGTH_ITN } from '../config/epochsConfig';

// News requests
import { getNews } from './news/requests/getNews';

// Stake Pools request
import { getStakePools } from './staking/requests/getStakePools';
import { getDelegationFee } from './staking/requests/getDelegationFee';
import { joinStakePool } from './staking/requests/joinStakePool';
import { quitStakePool } from './staking/requests/quitStakePool';

// Utility functions
import { wait } from './utils/apiHelpers';
import {
  awaitUpdateChannel,
  cardanoFaultInjectionChannel,
} from '../ipc/cardano.ipc';
import patchAdaApi from './utils/patchAdaApi';
import { getLegacyWalletId, utcStringToDate } from './utils';
import { logger } from '../utils/logging';
import {
  unscrambleMnemonics,
  scrambleMnemonics,
  generateAccountMnemonics,
  generateAdditionalMnemonics,
} from './utils/mnemonics';
import { filterLogData } from '../../../common/utils/logging';

// Config constants
import { LOVELACES_PER_ADA } from '../config/numbersConfig';
import {
  ADA_CERTIFICATE_MNEMONIC_LENGTH,
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
  LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from '../config/cryptoConfig';
import { FORCED_WALLET_RESYNC_WAIT } from '../config/timingConfig';

// Addresses Types
import type {
  Address,
  GetAddressesRequest,
  CreateByronWalletAddressRequest,
} from './addresses/types';

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
  RestoreExportedByronWalletRequest,
  UpdateSpendingPasswordRequest,
  ExportWalletToFileRequest,
  GetWalletCertificateRecoveryPhraseRequest,
  GetWalletRecoveryPhraseFromCertificateRequest,
  ImportWalletFromKeyRequest,
  ImportWalletFromFileRequest,
  ForceWalletResyncRequest,
  GetWalletUtxosRequest,
  GetWalletRequest,
  GetWalletIdAndBalanceRequest,
  GetWalletIdAndBalanceResponse,
  TransferFundsCalculateFeeRequest,
  TransferFundsCalculateFeeResponse,
  TransferFundsRequest,
  TransferFundsResponse,
  UpdateWalletRequest,
} from './wallets/types';
import type { WalletProps } from '../domains/Wallet';

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
import type { StakePoolProps } from '../domains/StakePool';

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
import { WALLET_BYRON_KINDS } from '../config/walletRestoreConfig';

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
    logger.debug('AdaApi::getWallets called');
    const { isIncentivizedTestnet } = global;
    try {
      const wallets: AdaWallets = isIncentivizedTestnet
        ? await getWallets(this.config)
        : [];
      const legacyWallets: LegacyAdaWallets = await getLegacyWallets(
        this.config
      );
      logger.debug('AdaApi::getWallets success', { wallets, legacyWallets });

      map(legacyWallets, legacyAdaWallet => {
        const extraLegacyWalletProps = {
          address_pool_gap: 0, // Not needed for legacy wallets
          delegation: {
            active: {
              status: WalletDelegationStatuses.NOT_DELEGATING,
            },
          },
          isLegacy: true,
        };
        wallets.push({
          ...legacyAdaWallet,
          ...extraLegacyWalletProps,
        });
      });

      return wallets.map(_createWalletFromServerData);
    } catch (error) {
      logger.error('AdaApi::getWallets error', { error });
      throw new GenericApiError(error);
    }
  };

  getWallet = async (request: GetWalletRequest): Promise<Wallet> => {
    logger.debug('AdaApi::getWallet called', {
      parameters: filterLogData(request),
    });
    try {
      const { walletId, isLegacy } = request;
      let wallet;
      if (isLegacy) {
        const legacyWallet: LegacyAdaWallet = await getLegacyWallet(
          this.config,
          { walletId }
        );
        const extraLegacyWalletProps = {
          address_pool_gap: 0, // Not needed for legacy wallets
          delegation: {
            active: {
              status: WalletDelegationStatuses.NOT_DELEGATING,
            },
          },
          isLegacy: true,
        };
        wallet = {
          ...legacyWallet,
          ...extraLegacyWalletProps,
        };
      } else {
        wallet = await getWallet(this.config, { walletId });
      }
      logger.debug('AdaApi::getWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logger.error('AdaApi::getWallet error', { error });
      throw new GenericApiError(error);
    }
  };

  getAddresses = async (
    request: GetAddressesRequest
  ): Promise<Array<WalletAddress>> => {
    logger.debug('AdaApi::getAddresses called', {
      parameters: filterLogData(request),
    });
    const { walletId, queryParams, isLegacy } = request;
    try {
      let response = [];
      if (isLegacy) {
        // @TODO - response is faked to enable UI. Comment out once endpoint is available
        // response = await getByronWalletAddresses(this.config, walletId, queryParams);
        return response;
      }
      response = await getAddresses(this.config, walletId, queryParams);

      logger.debug('AdaApi::getAddresses success', { addresses: response });
      return response.map(_createAddressFromServerData);
    } catch (error) {
      logger.error('AdaApi::getAddresses error', { error });
      throw new GenericApiError(error);
    }
  };

  getTransactions = async (
    request: GetTransactionsRequest
  ): Promise<GetTransactionsResponse> => {
    logger.debug('AdaApi::searchHistory called', { parameters: request });
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
      logger.error('AdaApi::searchHistory error', { error });
      throw new GenericApiError(error);
    }

    // @API TODO - Filter / Search fine tunning "pending" for V2

    // const requestStats = Object.assign({}, request, {
    //   cachedTransactions: request.cachedTransactions.length,
    // });
    //  logger.debug('AdaApi::searchHistory called', { parameters: requestStats });
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
    //   logger.debug(
    //     `AdaApi::searchHistory success: ${total} transactions loaded`,
    //     { responseStats }
    //   );
    //   return new Promise(resolve => resolve({ transactions, total }));
    // } catch (error) {
    //   logger.error('AdaApi::searchHistory error', { error });
    //   throw new GenericApiError(error);
    // }
  };

  createWallet = async (request: CreateWalletRequest): Promise<Wallet> => {
    const { isIncentivizedTestnet } = global;
    logger.debug('AdaApi::createWallet called', {
      parameters: filterLogData(request),
    });
    const { name, mnemonic, spendingPassword } = request;
    try {
      let wallet: AdaWallet;
      const walletInitData = {
        name,
        mnemonic_sentence: split(mnemonic, ' '),
        passphrase: spendingPassword,
      };

      if (isIncentivizedTestnet) {
        wallet = await createWallet(this.config, {
          walletInitData,
        });
        logger.debug('AdaApi::createWallet (Shelley) success', { wallet });
      } else {
        const legacyWallet: LegacyAdaWallet = await restoreByronWallet(
          this.config,
          { walletInitData },
          'random'
        );
        const extraLegacyWalletProps = {
          address_pool_gap: 0, // Not needed for legacy wallets
          delegation: {
            active: {
              status: WalletDelegationStatuses.NOT_DELEGATING,
            },
          },
          isLegacy: true,
        };
        wallet = {
          ...legacyWallet,
          ...extraLegacyWalletProps,
        };
        logger.debug('AdaApi::createWallet (Byron) success', { wallet });
      }
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logger.error('AdaApi::createWallet error', { error });
      throw new GenericApiError(error);
    }
  };

  deleteWallet = async (request: DeleteWalletRequest): Promise<boolean> => {
    logger.debug('AdaApi::deleteWallet called', {
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
      logger.debug('AdaApi::deleteWallet success', { response });
      return true;
    } catch (error) {
      logger.error('AdaApi::deleteWallet error', { error });
      throw new GenericApiError(error);
    }
  };

  createTransaction = async (
    request: CreateTransactionRequest
  ): Promise<WalletTransaction> => {
    logger.debug('AdaApi::createTransaction called', {
      parameters: filterLogData(request),
    });
    const { walletId, address, amount, passphrase, isLegacy } = request;

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

      let response: Transaction;
      if (isLegacy) {
        response = await createByronWalletTransaction(this.config, {
          walletId,
          data,
        });
      } else {
        response = await createTransaction(this.config, {
          walletId,
          data,
        });
      }

      logger.debug('AdaApi::createTransaction success', {
        transaction: response,
      });

      return _createTransactionFromServerData(response);
    } catch (error) {
      logger.error('AdaApi::createTransaction error', { error });
      if (error.code === 'output_is_redeem') {
        throw new NotAllowedToSendMoneyToRedeemAddressError();
      }
      if (error.code === 'cannot_cover_fee') {
        throw new NotEnoughFundsForTransactionFeesError();
      }
      if (error.code === 'not_enough_money') {
        throw new NotEnoughMoneyToSendError();
      }
      if (
        error.code === 'wrong_encryption_passphrase' ||
        (error.code === 'bad_request' &&
          error.message.includes('passphrase is too short'))
      ) {
        throw new IncorrectSpendingPasswordError();
      }
      if (error.code === 'too_big_transaction') {
        throw new TooBigTransactionError();
      }
      throw new GenericApiError(error);
    }
  };

  calculateTransactionFee = async (
    request: GetTransactionFeeRequest
  ): Promise<BigNumber> => {
    logger.debug('AdaApi::calculateTransactionFee called', {
      parameters: filterLogData(request),
    });
    const {
      walletId,
      address,
      amount,
      walletBalance,
      availableBalance,
      isLegacy,
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

      let response: TransactionFee;
      if (isLegacy) {
        response = await getByronWalletTransactionFee(this.config, {
          walletId,
          data,
        });
      } else {
        response = await getTransactionFee(this.config, {
          walletId,
          data,
        });
      }

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

      logger.debug('AdaApi::calculateTransactionFee success', {
        transactionFee: response,
      });
      return fee;
    } catch (error) {
      logger.error('AdaApi::calculateTransactionFee error', { error });
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
        throw new GenericApiError(error);
      }
    }
  };

  createAddress = async (
    request: CreateByronWalletAddressRequest
  ): Promise<WalletAddress> => {
    logger.debug('AdaApi::createAddress called', {
      parameters: filterLogData(request),
    });
    const { addressIndex, walletId, passphrase: passwordString } = request;
    const passphrase = passwordString || '';
    try {
      const address: Address = await createByronWalletAddress(this.config, {
        passphrase,
        walletId,
        addressIndex,
      });
      logger.debug('AdaApi::createAddress success', { address });
      return _createAddressFromServerData(address);
    } catch (error) {
      logger.error('AdaApi::createAddress error', { error });
      if (error.message === 'CannotCreateAddress') {
        throw new IncorrectSpendingPasswordError();
      }
      throw new GenericApiError(error);
    }
  };

  deleteTransaction = async (
    request: DeleteTransactionRequest
  ): Promise<void> => {
    logger.debug('AdaApi::deleteTransaction called', { parameters: request });
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
      logger.debug('AdaApi::deleteTransaction success', response);
    } catch (error) {
      logger.error('AdaApi::deleteTransaction error', { error });
      // In this particular call we don't need to handle the error in the UI
      // The only reason transaction canceling would fail is if the transaction
      // is no longer pending - in which case there is nothign we can do.
    }
  };

  isValidCertificateMnemonic = (mnemonic: string): boolean =>
    mnemonic.split(' ').length === ADA_CERTIFICATE_MNEMONIC_LENGTH;

  getWalletRecoveryPhrase(): Promise<Array<string>> {
    const { isIncentivizedTestnet } = global;
    logger.debug('AdaApi::getWalletRecoveryPhrase called');
    try {
      const response: Promise<Array<string>> = new Promise(resolve =>
        resolve(
          generateAccountMnemonics(
            isIncentivizedTestnet
              ? WALLET_RECOVERY_PHRASE_WORD_COUNT
              : LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT
          )
        )
      );
      logger.debug('AdaApi::getWalletRecoveryPhrase success');
      return response;
    } catch (error) {
      logger.error('AdaApi::getWalletRecoveryPhrase error', { error });
      throw new GenericApiError(error);
    }
  }

  getWalletCertificateAdditionalMnemonics(): Promise<Array<string>> {
    logger.debug('AdaApi::getWalletCertificateAdditionalMnemonics called');
    try {
      const response: Promise<Array<string>> = new Promise(resolve =>
        resolve(generateAdditionalMnemonics())
      );
      logger.debug('AdaApi::getWalletCertificateAdditionalMnemonics success');
      return response;
    } catch (error) {
      logger.error('AdaApi::getWalletCertificateAdditionalMnemonics error', {
        error,
      });
      throw new GenericApiError(error);
    }
  }

  getWalletCertificateRecoveryPhrase(
    request: GetWalletCertificateRecoveryPhraseRequest
  ): Promise<Array<string>> {
    logger.debug('AdaApi::getWalletCertificateRecoveryPhrase called');
    const { passphrase, input: scrambledInput } = request;
    try {
      const response: Promise<Array<string>> = new Promise(resolve =>
        resolve(scrambleMnemonics({ passphrase, scrambledInput }))
      );
      logger.debug('AdaApi::getWalletCertificateRecoveryPhrase success');
      return response;
    } catch (error) {
      logger.error('AdaApi::getWalletCertificateRecoveryPhrase error', {
        error,
      });
      throw new GenericApiError(error);
    }
  }

  getWalletRecoveryPhraseFromCertificate(
    request: GetWalletRecoveryPhraseFromCertificateRequest
  ): Promise<Array<string>> {
    logger.debug('AdaApi::getWalletRecoveryPhraseFromCertificate called');
    const { passphrase, scrambledInput } = request;
    try {
      const response = unscrambleMnemonics({ passphrase, scrambledInput });
      logger.debug('AdaApi::getWalletRecoveryPhraseFromCertificate success');
      return Promise.resolve(response);
    } catch (error) {
      logger.error('AdaApi::getWalletRecoveryPhraseFromCertificate error', {
        error,
      });
      return Promise.reject(new InvalidMnemonicError());
    }
  }

  restoreWallet = async (request: RestoreWalletRequest): Promise<Wallet> => {
    logger.debug('AdaApi::restoreWallet called', {
      parameters: filterLogData(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    try {
      const wallet: AdaWallet = await restoreWallet(this.config, {
        walletInitData,
      });
      logger.debug('AdaApi::restoreWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logger.error('AdaApi::restoreWallet error', { error });
      if (error.code === 'wallet_already_exists') {
        throw new WalletAlreadyRestoredError();
      }
      // @API TODO - improve once error is handled by v2 API (REPORT to BE team)
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
      throw new GenericApiError(error);
    }
  };

  restoreLegacyWallet = async (
    request: RestoreLegacyWalletRequest
  ): Promise<Wallet> => {
    logger.debug('AdaApi::restoreLegacyWallet called', {
      parameters: filterLogData(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      style: 'random',
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    try {
      const legacyWallet: LegacyAdaWallet = await restoreLegacyWallet(
        this.config,
        { walletInitData }
      );
      const extraLegacyWalletProps = {
        address_pool_gap: 0, // Not needed for legacy wallets
        delegation: {
          active: {
            status: WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = {
        ...legacyWallet,
        ...extraLegacyWalletProps,
      };
      logger.debug('AdaApi::restoreLegacyWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logger.error('AdaApi::restoreLegacyWallet error', { error });
      if (error.code === 'wallet_already_exists') {
        throw new WalletAlreadyRestoredError();
      }
      // @API TODO - improve once error is handled by v2 API (REPORT to BE team)
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
      throw new GenericApiError(error);
    }
  };

  restoreByronRandomWallet = async (
    request: RestoreLegacyWalletRequest
  ): Promise<Wallet> => {
    logger.debug('AdaApi::restoreByronRandomWallet called', {
      parameters: filterLogData(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    const type = WALLET_BYRON_KINDS.RANDOM;
    try {
      const legacyWallet: LegacyAdaWallet = await restoreByronWallet(
        this.config,
        { walletInitData },
        type
      );
      const extraLegacyWalletProps = {
        address_pool_gap: 0, // Not needed for legacy wallets
        delegation: {
          active: {
            status: WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = {
        ...legacyWallet,
        ...extraLegacyWalletProps,
      };
      logger.debug('AdaApi::restoreByronRandomWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logger.error('AdaApi::restoreByronRandomWallet error', { error });
      if (error.code === 'wallet_already_exists') {
        throw new WalletAlreadyRestoredError();
      }
      // @API TODO - improve once error is handled by v2 API (REPORT to BE team)
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
      throw new GenericApiError(error);
    }
  };

  restoreByronIcarusWallet = async (
    request: RestoreLegacyWalletRequest
  ): Promise<Wallet> => {
    logger.debug('AdaApi::restoreByronIcarusWallet called', {
      parameters: filterLogData(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    const type = WALLET_BYRON_KINDS.ICARUS;
    try {
      const legacyWallet: LegacyAdaWallet = await restoreByronWallet(
        this.config,
        { walletInitData },
        type
      );
      const extraLegacyWalletProps = {
        address_pool_gap: 0, // Not needed for legacy wallets
        delegation: {
          active: {
            status: WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = {
        ...legacyWallet,
        ...extraLegacyWalletProps,
      };
      logger.debug('AdaApi::restoreByronIcarusWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logger.error('AdaApi::restoreByronIcarusWallet error', { error });
      if (error.code === 'wallet_already_exists') {
        throw new WalletAlreadyRestoredError();
      }
      // @API TODO - improve once error is handled by v2 API (REPORT to BE team)
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
      throw new GenericApiError(error);
    }
  };

  restoreByronTrezorWallet = async (
    request: RestoreLegacyWalletRequest
  ): Promise<Wallet> => {
    logger.debug('AdaApi::restoreByronTrezorWallet called', {
      parameters: filterLogData(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    const type = WALLET_BYRON_KINDS.TREZOR;
    try {
      const legacyWallet: LegacyAdaWallet = await restoreByronWallet(
        this.config,
        { walletInitData },
        type
      );
      const extraLegacyWalletProps = {
        address_pool_gap: 0, // Not needed for legacy wallets
        delegation: {
          active: {
            status: WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = {
        ...legacyWallet,
        ...extraLegacyWalletProps,
      };
      logger.debug('AdaApi::restoreByronTrezorWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logger.error('AdaApi::restoreByronTrezorWallet error', { error });
      if (error.code === 'wallet_already_exists') {
        throw new WalletAlreadyRestoredError();
      }
      // @API TODO - improve once error is handled by v2 API (REPORT to BE team)
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
      throw new GenericApiError(error);
    }
  };

  restoreByronLedgerWallet = async (
    request: RestoreLegacyWalletRequest
  ): Promise<Wallet> => {
    logger.debug('AdaApi::restoreByronLedgerWallet called', {
      parameters: filterLogData(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    const type = WALLET_BYRON_KINDS.LEDGER;
    try {
      const legacyWallet: LegacyAdaWallet = await restoreByronWallet(
        this.config,
        { walletInitData },
        type
      );
      const extraLegacyWalletProps = {
        address_pool_gap: 0, // Not needed for legacy wallets
        delegation: {
          active: {
            status: WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = {
        ...legacyWallet,
        ...extraLegacyWalletProps,
      };
      logger.debug('AdaApi::restoreByronLedgerWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logger.error('AdaApi::restoreByronLedgerWallet error', { error });
      if (error.code === 'wallet_already_exists') {
        throw new WalletAlreadyRestoredError();
      }
      // @API TODO - improve once error is handled by v2 API (REPORT to BE team)
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
      throw new GenericApiError(error);
    }
  };

  restoreExportedByronWallet = async (
    request: RestoreExportedByronWalletRequest
  ): Promise<Wallet> => {
    logger.debug('AdaApi::restoreExportedByronWallet called', {
      name: request.name,
      hasPassword: request.passphrase_hash !== null,
    });
    try {
      const legacyWallet: LegacyAdaWallet = await restoreExportedByronWallet(
        this.config,
        { walletInitData: request }
      );
      const extraLegacyWalletProps = {
        address_pool_gap: 0, // Not needed for legacy wallets
        delegation: {
          active: {
            status: WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = {
        ...legacyWallet,
        ...extraLegacyWalletProps,
      };
      logger.debug('AdaApi::restoreExportedByronWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logger.error('AdaApi::restoreExportedByronWallet error', { error });
      if (error.code === 'wallet_already_exists') {
        throw new WalletAlreadyRestoredError();
      }
      throw new GenericApiError(error);
    }
  };

  importWalletFromKey = async (
    request: ImportWalletFromKeyRequest
  ): Promise<Wallet> => {
    logger.debug('AdaApi::importWalletFromKey called', {
      parameters: filterLogData(request),
    });
    const { filePath, spendingPassword } = request;
    try {
      const importedWallet: AdaWallet = await importWalletAsKey(this.config, {
        filePath,
        spendingPassword: spendingPassword || '',
      });
      logger.debug('AdaApi::importWalletFromKey success', { importedWallet });
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      logger.error('AdaApi::importWalletFromKey error', { error });
      if (error.message === 'WalletAlreadyExists') {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  };

  importWalletFromFile = async (
    request: ImportWalletFromFileRequest
  ): Promise<Wallet> => {
    logger.debug('AdaApi::importWalletFromFile called', {
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
      logger.debug('AdaApi::importWalletFromFile success', { importedWallet });
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      logger.error('AdaApi::importWalletFromFile error', { error });
      if (error.message === 'WalletAlreadyExists') {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  };

  nextUpdate = async (): Promise<NodeSoftware | null> => {
    logger.debug('AdaApi::nextUpdate called');

    /* TODO: Re-enable when API is available
    try {
      const nodeUpdate = await getNextNodeUpdate(this.config);
      if (nodeUpdate && nodeUpdate.version) {
        logger.debug('AdaApi::nextUpdate success', { nodeUpdate });
        return nodeUpdate;
      }
      logger.debug('AdaApi::nextUpdate success: No Update Available');
    } catch (error) {
      logger.error('AdaApi::nextUpdate error', { error });
      throw new GenericApiError(error);
    }
    */

    return null;
  };

  postponeUpdate = async (): Promise<void> => {
    logger.debug('AdaApi::postponeUpdate called');
    try {
      const response: Promise<any> = await postponeNodeUpdate(this.config);
      logger.debug('AdaApi::postponeUpdate success', { response });
    } catch (error) {
      logger.error('AdaApi::postponeUpdate error', { error });
      throw new GenericApiError(error);
    }
  };

  applyUpdate = async (): Promise<void> => {
    logger.debug('AdaApi::applyUpdate called');
    try {
      await awaitUpdateChannel.send();
      const response: Promise<any> = await applyNodeUpdate(this.config);
      logger.debug('AdaApi::applyUpdate success', { response });
    } catch (error) {
      logger.error('AdaApi::applyUpdate error', { error });
      throw new GenericApiError(error);
    }
  };

  updateWallet = async (request: UpdateWalletRequest): Promise<Wallet> => {
    logger.debug('AdaApi::updateWallet called', {
      parameters: filterLogData(request),
    });
    const { walletId, name, isLegacy } = request;
    try {
      let wallet: AdaWallet;
      if (isLegacy) {
        const response = await updateByronWallet(this.config, {
          walletId,
          name,
        });
        wallet = {
          ...response,
          address_pool_gap: 0, // Not needed for legacy wallets
          delegation: {
            active: {
              status: WalletDelegationStatuses.NOT_DELEGATING,
            },
          },
          isLegacy: true,
        };
      } else {
        wallet = await updateWallet(this.config, { walletId, name });
      }
      logger.debug('AdaApi::updateWallet success', { wallet });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logger.error('AdaApi::updateWallet error', { error });
      throw new GenericApiError(error);
    }
  };

  updateSpendingPassword = async (
    request: UpdateSpendingPasswordRequest
  ): Promise<boolean> => {
    logger.debug('AdaApi::updateSpendingPassword called', {
      parameters: filterLogData(request),
    });
    const { walletId, oldPassword, newPassword, isLegacy } = request;
    try {
      if (isLegacy) {
        await updateByronSpendingPassword(this.config, {
          walletId,
          oldPassword,
          newPassword,
        });
      } else {
        await updateSpendingPassword(this.config, {
          walletId,
          oldPassword,
          newPassword,
        });
      }
      logger.debug('AdaApi::updateSpendingPassword success');
      return true;
    } catch (error) {
      logger.error('AdaApi::updateSpendingPassword error', { error });
      const errorCode = get(error, 'code', '');
      if (
        errorCode === 'wrong_encryption_passphrase' ||
        (errorCode === 'bad_request' &&
          error.message.includes('passphrase is too short'))
      ) {
        throw new IncorrectSpendingPasswordError();
      }
      throw new GenericApiError(error);
    }
  };

  quitStakePool = async (
    request: QuitStakePoolRequest
  ): Promise<Transaction> => {
    logger.debug('AdaApi::quitStakePool called', {
      parameters: filterLogData(request),
    });
    const { walletId, passphrase } = request;
    try {
      const result = await quitStakePool(this.config, {
        walletId,
        passphrase,
      });
      logger.debug('AdaApi::quitStakePool success', { result });
      return result;
    } catch (error) {
      logger.error('AdaApi::quitStakePool error', { error });
      const errorCode = get(error, 'code', '');
      if (
        errorCode === 'wrong_encryption_passphrase' ||
        (errorCode === 'bad_request' &&
          error.message.includes('passphrase is too short'))
      ) {
        throw new IncorrectSpendingPasswordError();
      }
      throw new GenericApiError(error);
    }
  };

  exportWalletToFile = async (
    request: ExportWalletToFileRequest
  ): Promise<[]> => {
    const { walletId, filePath } = request;
    logger.debug('AdaApi::exportWalletToFile called', {
      parameters: filterLogData(request),
    });
    try {
      const response: Promise<[]> = await exportWalletAsJSON(this.config, {
        walletId,
        filePath,
      });
      logger.debug('AdaApi::exportWalletToFile success', { response });
      return response;
    } catch (error) {
      logger.error('AdaApi::exportWalletToFile error', { error });
      throw new GenericApiError(error);
    }
  };

  getWalletUtxos = async (
    request: GetWalletUtxosRequest
  ): Promise<WalletUtxos> => {
    const { walletId, isLegacy } = request;
    logger.debug('AdaApi::getWalletUtxos called', {
      parameters: filterLogData(request),
    });
    try {
      let response: WalletUtxos;
      if (isLegacy) {
        response = await getByronWalletUtxos(this.config, { walletId });
      } else {
        response = await getWalletUtxos(this.config, { walletId });
      }
      logger.debug('AdaApi::getWalletUtxos success', { response });
      return response;
    } catch (error) {
      logger.error('AdaApi::getWalletUtxos error', { error });
      throw new GenericApiError(error);
    }
  };

  getWalletIdAndBalance = async (
    request: GetWalletIdAndBalanceRequest
  ): Promise<WalletIdAndBalance> => {
    const { recoveryPhrase, getBalance } = request;
    logger.debug('AdaApi::getWalletIdAndBalance called', {
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
      logger.debug('AdaApi::getWalletIdAndBalance success', { response });
      const { walletId, balance } = response;
      return {
        walletId,
        balance:
          balance !== null // If balance is "null" it means we didn't fetch it - getBalance was false
            ? new BigNumber(balance).dividedBy(LOVELACES_PER_ADA)
            : null,
      };
    } catch (error) {
      logger.error('AdaApi::getWalletIdAndBalance error', { error });
      throw new GenericApiError(error);
    }
  };

  forceWalletResync = async (
    request: ForceWalletResyncRequest
  ): Promise<void> => {
    await wait(FORCED_WALLET_RESYNC_WAIT); // API request throttling
    logger.debug('AdaApi::forceWalletResync called', { parameters: request });
    try {
      const { walletId, isLegacy } = request;
      let response;
      if (isLegacy) {
        response = await forceLegacyWalletResync(this.config, { walletId });
      } else {
        response = await forceWalletResync(this.config, { walletId });
      }
      logger.debug('AdaApi::forceWalletResync success', { response });
    } catch (error) {
      logger.error('AdaApi::forceWalletResync error', { error });
      throw new GenericApiError(error);
    }
  };

  transferFundsCalculateFee = async (
    request: TransferFundsCalculateFeeRequest
  ): Promise<BigNumber> => {
    const { sourceWalletId } = request;
    logger.debug('AdaApi::transferFundsCalculateFee called', {
      parameters: { sourceWalletId },
    });
    try {
      const response: TransferFundsCalculateFeeResponse = await transferFundsCalculateFee(
        this.config,
        {
          sourceWalletId,
        }
      );
      logger.debug('AdaApi::transferFundsCalculateFee success', { response });
      return _createMigrationFeeFromServerData(response);
    } catch (error) {
      logger.error('AdaApi::transferFundsCalculateFee error', { error });
      throw new GenericApiError(error);
    }
  };

  transferFunds = async (
    request: TransferFundsRequest
  ): Promise<TransferFundsResponse> => {
    const { sourceWalletId, targetWalletId, passphrase } = request;
    logger.debug('AdaApi::transferFunds called', {
      parameters: { sourceWalletId, targetWalletId },
    });

    try {
      const response: TransferFundsResponse = await transferFunds(this.config, {
        sourceWalletId,
        targetWalletId,
        passphrase,
      });
      logger.debug('AdaApi::transferFunds success', { response });
      return response;
    } catch (error) {
      logger.error('AdaApi::transferFunds error', { error });
      if (
        error.code === 'wrong_encryption_passphrase' ||
        (error.code === 'bad_request' &&
          error.message.includes('passphrase is too short'))
      ) {
        throw new IncorrectSpendingPasswordError();
      }
      throw new GenericApiError(error);
    }
  };

  getStakePools = async (): Promise<Array<StakePool>> => {
    logger.debug('AdaApi::getStakePools called');
    try {
      const response: AdaApiStakePools = await getStakePools(this.config);
      const stakePools = response
        .filter(({ metadata }: AdaApiStakePool) => metadata !== undefined)
        .map(_createStakePoolFromServerData);
      logger.debug('AdaApi::getStakePools success', {
        stakePoolsTotal: response.length,
        stakePoolsWithMetadata: stakePools.length,
      });
      return stakePools;
    } catch (error) {
      logger.error('AdaApi::getStakePools error', { error });
      throw new GenericApiError(error);
    }
  };

  testReset = async (): Promise<void> => {
    const { isIncentivizedTestnet } = global;
    logger.debug('AdaApi::testReset called');
    try {
      if (isIncentivizedTestnet) {
        const wallets: AdaWallets = await getWallets(this.config);
        await Promise.all(
          wallets.map(wallet =>
            deleteWallet(this.config, { walletId: wallet.id })
          )
        );
      }
      const legacyWallets: LegacyAdaWallets = await getLegacyWallets(
        this.config
      );
      await Promise.all(
        legacyWallets.map(wallet =>
          deleteLegacyWallet(this.config, { walletId: wallet.id })
        )
      );
      logger.debug('AdaApi::testReset success');
    } catch (error) {
      logger.error('AdaApi::testReset error', { error });
      throw new GenericApiError(error);
    }
  };

  getNetworkInfo = async (): Promise<GetNetworkInfoResponse> => {
    logger.debug('AdaApi::getNetworkInfo called');
    try {
      const networkInfo: NetworkInfoResponse = await getNetworkInfo(
        this.config
      );
      logger.debug('AdaApi::getNetworkInfo success', { networkInfo });
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
          // N+1 epoch
          epochNumber: get(next_epoch, 'epoch_number', 0),
          epochStart: get(next_epoch, 'epoch_start_time', ''),
        },
        futureEpoch: {
          // N+2 epoch
          epochNumber: get(next_epoch, 'epoch_number', 0) + 1,
          epochStart: moment(get(next_epoch, 'epoch_start_time', 0))
            .add(EPOCH_LENGTH_ITN, 'seconds')
            .toISOString(),
        },
      };
    } catch (error) {
      logger.error('AdaApi::getNetworkInfo error', { error });
      // @API TODO - Inspect this implementation once TLS support is implemented on the BE
      if (error.code === TlsCertificateNotValidError.API_ERROR) {
        throw new TlsCertificateNotValidError();
      }
      throw new GenericApiError(error);
    }
  };

  getLatestAppVersion = async (): Promise<GetLatestAppVersionResponse> => {
    logger.debug('AdaApi::getLatestAppVersion called');
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
      logger.debug('AdaApi::getLatestAppVersion success', {
        latestAppVersion,
        latestAppVersionInfo,
        applicationVersion,
      });
      return { latestAppVersion, applicationVersion };
    } catch (error) {
      logger.error('AdaApi::getLatestAppVersion error', { error });
      throw new GenericApiError(error);
    }
  };

  getNews = async (): Promise<GetNewsResponse> => {
    logger.debug('AdaApi::getNews called');

    // Fetch news json
    let rawNews: string;
    let news: GetNewsResponse;
    try {
      rawNews = await getNews();
      news = JSON.parse(rawNews);
    } catch (error) {
      logger.error('AdaApi::getNews error', { error });
      throw new Error('Unable to fetch news');
    }

    // Fetch news verification hash
    let newsHash: string;
    let expectedNewsHash: string;
    try {
      newsHash = await getSHA256HexForString(rawNews);
      expectedNewsHash = await getNewsHash(news.updatedAt);
    } catch (error) {
      logger.error('AdaApi::getNews (hash) error', { error });
      throw new Error('Unable to fetch news hash');
    }

    if (newsHash !== expectedNewsHash) {
      throw new Error('Newsfeed could not be verified');
    }

    logger.debug('AdaApi::getNews success', {
      updatedAt: news.updatedAt,
      items: news.items.length,
    });
    return news;
  };

  calculateDelegationFee = async (
    request: GetDelegationFeeRequest
  ): Promise<BigNumber> => {
    logger.debug('AdaApi::calculateDelegationFee called', {
      parameters: filterLogData(request),
    });
    try {
      const response: DelegationFee = await getDelegationFee(this.config, {
        walletId: request.walletId,
      });
      logger.debug('AdaApi::calculateDelegationFee success', { response });
      const delegationFee = _createDelegationFeeFromServerData(response);
      return delegationFee;
    } catch (error) {
      logger.error('AdaApi::calculateDelegationFee error', { error });
      throw new GenericApiError(error);
    }
  };

  joinStakePool = async (
    request: JoinStakePoolRequest
  ): Promise<Transaction> => {
    logger.debug('AdaApi::joinStakePool called', {
      parameters: filterLogData(request),
    });
    const { walletId, stakePoolId, passphrase } = request;
    try {
      const response = await joinStakePool(this.config, {
        walletId,
        stakePoolId,
        passphrase,
      });
      logger.debug('AdaApi::joinStakePool success', {
        stakePool: response,
      });
      return response;
    } catch (error) {
      logger.error('AdaApi::joinStakePool error', { error });
      if (
        error.code === 'wrong_encryption_passphrase' ||
        (error.code === 'bad_request' &&
          error.message.includes('passphrase is too short'))
      ) {
        throw new IncorrectSpendingPasswordError();
      }
      throw new GenericApiError(error);
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
  setTestingNewsFeed: (testingNewsFeedData: GetNewsResponse) => void;
  setTestingStakePools: (testingStakePoolsData: Array<StakePoolProps>) => void;
  setTestingWallets: (testingWalletsData: Array<WalletProps>) => void;
  setTestingWallet: (testingWalletData: Object, walletIndex?: number) => void;

  // Stake pools testing utility
  setFakeStakePoolsJsonForTesting: (
    fakeStakePoolsJson: Array<StakePool>
  ) => void;
  setStakePoolsFetchingFailed: () => void;
}

// ========== TRANSFORM SERVER DATA INTO FRONTEND MODELS =========

const _createWalletFromServerData = action(
  'AdaApi::_createWalletFromServerData',
  (wallet: AdaWallet) => {
    const {
      id: rawWalletId,
      address_pool_gap: addressPoolGap,
      balance,
      name,
      passphrase,
      delegation,
      state: syncState,
      isLegacy = false,
    } = wallet;

    const id = isLegacy ? getLegacyWalletId(rawWalletId) : rawWalletId;
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

    // Current (Active)
    const active = get(delegation, 'active', null);
    const target = get(active, 'target', null);
    const status = get(active, 'status', null);
    const delegatedStakePoolId = isLegacy ? null : target;
    const delegationStakePoolStatus = isLegacy ? null : status;

    // Last
    const next = get(delegation, 'next', null);
    const lastPendingStakePool = next ? last(next) : null;
    const lastTarget = get(lastPendingStakePool, 'target', null);
    const lastDelegationStakePoolId = isLegacy ? null : lastTarget;

    return new Wallet({
      id,
      addressPoolGap,
      name,
      amount: walletTotalAmount,
      availableAmount: walletAvailableAmount,
      reward: walletRewardAmount,
      passwordUpdateDate:
        passphraseLastUpdatedAt && new Date(passphraseLastUpdatedAt),
      hasPassword: passphraseLastUpdatedAt !== null,
      syncState,
      isLegacy,
      delegatedStakePoolId,
      delegationStakePoolStatus,
      lastDelegationStakePoolId,
      pendingDelegations: next,
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
    const {
      id,
      metrics,
      apparent_performance: performance,
      cost,
      margin: profitMargin,
      metadata,
      saturation,
    } = stakePool;
    const {
      controlled_stake: controlledStake,
      produced_blocks: producedBlocks,
    } = metrics; // eslint-disable-line
    const {
      name,
      description = '',
      ticker,
      homepage,
      pledge_address: pledgeAddress,
    } = metadata;
    const controlledStakeQuantity = get(controlledStake, 'quantity', 0);
    const producedBlocksCount = get(producedBlocks, 'quantity', 0);
    const costQuantity = get(cost, 'quantity', 0);
    const profitMarginPercentage = get(profitMargin, 'quantity', 0);
    return new StakePool({
      id,
      performance: performance * 100,
      controlledStake: new BigNumber(controlledStakeQuantity).dividedBy(
        LOVELACES_PER_ADA
      ),
      producedBlocks: producedBlocksCount,
      ticker,
      homepage,
      pledgeAddress,
      cost: new BigNumber(costQuantity).dividedBy(LOVELACES_PER_ADA),
      description,
      isCharity: false,
      name,
      // pledge: new BigNumber(pledge).dividedBy(LOVELACES_PER_ADA),
      profitMargin: profitMarginPercentage,
      ranking: index + 1,
      retiring: null,
      saturation: saturation * 100,
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
