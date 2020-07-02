// @flow
import { split, get, map, last } from 'lodash';
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
import { getByronWalletAddresses } from './addresses/requests/getByronWalletAddresses';
import { createByronWalletAddress } from './addresses/requests/createByronWalletAddress';

// Network requests
import { getNetworkInfo } from './network/requests/getNetworkInfo';
import { getNetworkClock } from './network/requests/getNetworkClock';
import { getNetworkParameters } from './network/requests/getNetworkParameters';

// App update requests
import { applyAppUpdate } from './nodes/requests/applyAppUpdate';
// import { getNextAppUpdate } from './nodes/requests/getNextAppUpdate';
import { postponeAppUpdate } from './nodes/requests/postponeAppUpdate';
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
import { getWalletUtxos } from './wallets/requests/getWalletUtxos';
import { getByronWalletUtxos } from './wallets/requests/getByronWalletUtxos';
import { getWallet } from './wallets/requests/getWallet';
import { getLegacyWallet } from './wallets/requests/getLegacyWallet';
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
  GetNetworkClockResponse,
  NetworkClockResponse,
  GetNetworkParametersResponse,
  NetworkParametersResponse,
} from './network/types';

// Nodes Types
import type {
  LatestAppVersionInfoResponse,
  AppInfo,
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
  GetWalletUtxosRequest,
  GetWalletRequest,
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
  AdaApiStakePools,
  AdaApiStakePool,
  QuitStakePoolRequest,
} from './staking/types';
import type { StakePoolProps } from '../domains/StakePool';
import type { FaultInjectionIpcRequest } from '../../../common/types/cardano-node.types';

import { TlsCertificateNotValidError } from './nodes/errors';
import { getSHA256HexForString } from './utils/hashing';
import { getNewsHash } from './news/requests/getNewsHash';
import { deleteTransaction } from './transactions/requests/deleteTransaction';
import { WALLET_BYRON_KINDS } from '../config/walletRestoreConfig';
import ApiError from '../domains/ApiError';

const { isIncentivizedTestnet, isShelleyTestnet } = global;

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
    try {
      const wallets: AdaWallets = isIncentivizedTestnet
        ? await getWallets(this.config)
        : [];
      const legacyWallets: LegacyAdaWallets = !isShelleyTestnet
        ? await getLegacyWallets(this.config)
        : [];
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
      throw new ApiError(error);
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
      throw new ApiError(error);
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
      if (isLegacy && !isIncentivizedTestnet) {
        response = await getByronWalletAddresses(
          this.config,
          walletId,
          queryParams
        );
      } else if (!isLegacy) {
        response = await getAddresses(this.config, walletId, queryParams);
      }
      logger.debug('AdaApi::getAddresses success', { addresses: response });
      return response.map(_createAddressFromServerData);
    } catch (error) {
      logger.error('AdaApi::getAddresses error', { error });
      throw new ApiError(error);
    }
  };

  getTransactions = async (
    request: GetTransactionsRequest
  ): Promise<GetTransactionsResponse> => {
    logger.debug('AdaApi::getTransactions called', { parameters: request });
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
      logger.debug('AdaApi::getTransactions success', {
        transactions: response,
      });
      const transactions = response.map(tx =>
        _createTransactionFromServerData(tx)
      );
      return new Promise(resolve =>
        resolve({ transactions, total: response.length })
      );
    } catch (error) {
      logger.error('AdaApi::getTransactions error', { error });
      throw new ApiError(error);
    }

    // @API TODO - Filter / Search fine tuning "pending" for V2

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
    //       // as this ensures that both totalPages and totalEntries remain unchanged throughout
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

        // Generate address for the newly created Byron wallet
        const { id: walletId } = legacyWallet;
        const address: Address = await createByronWalletAddress(this.config, {
          passphrase: spendingPassword,
          walletId,
        });
        logger.debug('AdaApi::createAddress (Byron) success', { address });

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
      throw new ApiError(error);
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
      throw new ApiError(error);
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
      throw new ApiError(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .set('transactionIsTooBig', true, {
          linkLabel: 'tooBigTransactionErrorLinkLabel',
          linkURL: 'tooBigTransactionErrorLinkURL',
        })
        .where('code', 'transaction_is_too_big')
        .result();
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
        throw new ApiError().result('cannotCoverFee');
      }
      logger.debug('AdaApi::calculateTransactionFee success', {
        transactionFee: response,
      });
      return fee;
    } catch (error) {
      // 1. Amount exceeds availableBalance due to pending transactions:
      // - error.diagnostic.details.msg === 'Not enough available coins to proceed.'
      // - total walletBalance > error.diagnostic.details.availableBalance
      // = show "Cannot calculate fees while there are pending transactions."
      // 2. Amount exceeds walletBalance:
      // - error.diagnostic.details.msg === 'Not enough available coins to proceed.'
      // - total walletBalance === error.diagnostic.details.availableBalance
      // = show "Not enough Ada. Try sending a smaller amount."
      const notEnoughMoneyError = walletBalance.gt(availableBalance)
        ? 'canNotCalculateTransactionFees'
        : 'notEnoughFundsForTransaction';

      // ApiError with logging showcase
      throw new ApiError(error, {
        logError: true,
        msg: 'AdaApi::calculateTransactionFee error',
      })
        .set(notEnoughMoneyError, true)
        .where('code', 'not_enough_money')
        .set('invalidAddress')
        .where('code', 'bad_request')
        .inc('message', 'Unable to decode Address')
        .result();
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

      throw new ApiError(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
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
      // is no longer pending - in which case there is nothing we can do.
    }
  };

  isValidCertificateMnemonic = (mnemonic: string): boolean =>
    mnemonic.split(' ').length === ADA_CERTIFICATE_MNEMONIC_LENGTH;

  getWalletRecoveryPhrase(): Promise<Array<string>> {
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
      throw new ApiError(error);
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
      throw new ApiError(error);
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
      throw new ApiError(error);
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
      const errorRejection = new ApiError(error)
        .set('invalidMnemonic', true)
        .result();
      return Promise.reject(errorRejection);
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

      throw new ApiError(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
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
      throw new ApiError(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
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

      if (!isIncentivizedTestnet) {
        // Generate address for the newly restored Byron wallet
        const { id: walletId } = legacyWallet;
        const address: Address = await createByronWalletAddress(this.config, {
          passphrase: spendingPassword,
          walletId,
        });
        logger.debug('AdaApi::createAddress (Byron) success', { address });
      }

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
      throw new ApiError(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
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
      throw new ApiError(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
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
      throw new ApiError(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
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
      throw new ApiError(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
    }
  };

  restoreExportedByronWallet = async (
    request: RestoreExportedByronWalletRequest
  ): Promise<Wallet> => {
    logger.debug('AdaApi::restoreExportedByronWallet called', {
      name: request.name,
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
      throw new ApiError(error);
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
      throw new ApiError(error)
        .set('walletAlreadyImported', true)
        .where('code', 'wallet_already_exists')
        .result('walletFileImportError');
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
      throw new ApiError(error)
        .set('walletAlreadyImported', true)
        .where('code', 'wallet_already_exists')
        .result('walletFileImportError');
    }
  };

  nextUpdate = async (): Promise<AppInfo | null> => {
    logger.debug('AdaApi::nextUpdate called');

    /* TODO: Re-enable when API is available
    try {
      const appUpdate = await getNextAppUpdate(this.config);
      if (appUpdate && appUpdate.version) {
        logger.debug('AdaApi::nextUpdate success', { appUpdate });
        return appUpdate;
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
      const response: Promise<any> = await postponeAppUpdate(this.config);
      logger.debug('AdaApi::postponeUpdate success', { response });
    } catch (error) {
      logger.error('AdaApi::postponeUpdate error', { error });
      throw new ApiError(error);
    }
  };

  applyUpdate = async (): Promise<void> => {
    logger.debug('AdaApi::applyUpdate called');
    try {
      await awaitUpdateChannel.send();
      const response: Promise<any> = await applyAppUpdate(this.config);
      logger.debug('AdaApi::applyUpdate success', { response });
    } catch (error) {
      logger.error('AdaApi::applyUpdate error', { error });
      throw new ApiError(error);
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
      throw new ApiError(error);
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

        if (!isIncentivizedTestnet && !oldPassword) {
          // Generate address for the Byron wallet for which password was set for the 1st time
          const address: Address = await createByronWalletAddress(this.config, {
            passphrase: newPassword,
            walletId,
          });
          logger.debug('AdaApi::createAddress (Byron) success', { address });
        }
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
      throw new ApiError(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
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
      throw new ApiError(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
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
      throw new ApiError(error);
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
      throw new ApiError(error);
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
      throw new ApiError(error);
    }
  };

  transferFunds = async (
    request: TransferFundsRequest
  ): Promise<TransferFundsResponse> => {
    const { sourceWalletId, targetWalletAddresses, passphrase } = request;
    logger.debug('AdaApi::transferFunds called', {
      parameters: { sourceWalletId, targetWalletAddresses },
    });

    if (!targetWalletAddresses) {
      throw new ApiError({
        code: 'no_such_wallet',
        message: 'Target wallet does not exist',
      }).result();
    }

    try {
      const response: TransferFundsResponse = await transferFunds(this.config, {
        sourceWalletId,
        targetWalletAddresses,
        passphrase,
      });
      logger.debug('AdaApi::transferFunds success', { response });
      return response;
    } catch (error) {
      logger.error('AdaApi::transferFunds error', { error });
      throw new ApiError(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
    }
  };

  getStakePools = async (): Promise<Array<StakePool>> => {
    logger.debug('AdaApi::getStakePools called');
    try {
      const response: AdaApiStakePools = await getStakePools(this.config);
      const stakePools = response
        .filter(({ metadata }: AdaApiStakePool) => metadata !== undefined)
        .filter(
          ({ margin }: AdaApiStakePool) =>
            margin !== undefined && margin.quantity < 100
        )
        .map(_createStakePoolFromServerData);
      logger.debug('AdaApi::getStakePools success', {
        stakePoolsTotal: response.length,
        stakePoolsWithMetadata: stakePools.length,
        unfilteredStakePools: response,
      });
      return stakePools;
    } catch (error) {
      logger.error('AdaApi::getStakePools error', { error });
      throw new ApiError(error);
    }
  };

  testReset = async (): Promise<void> => {
    logger.debug('AdaApi::testReset called');
    try {
      const wallets = await this.getWallets();
      await Promise.all(
        wallets.map(wallet =>
          this.deleteWallet({
            walletId: wallet.id,
            isLegacy: wallet.isLegacy,
          })
        )
      );
      logger.debug('AdaApi::testReset success');
    } catch (error) {
      logger.error('AdaApi::testReset error', { error });
      throw new ApiError(error);
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
      // Special Error case
      if (
        error.code === TlsCertificateNotValidError.API_ERROR ||
        error.code === 'EPROTO'
      ) {
        throw new TlsCertificateNotValidError();
      }
      throw new ApiError(error);
    }
  };

  getNetworkClock = async (
    isForceCheck: boolean
  ): Promise<GetNetworkClockResponse> => {
    logger.debug('AdaApi::getNetworkClock called', { isForceCheck });
    try {
      const networkClock: NetworkClockResponse = await getNetworkClock(
        this.config,
        isForceCheck
      );
      logger.debug('AdaApi::getNetworkClock success', {
        networkClock,
        isForceCheck,
      });
      return {
        status: networkClock.status,
        offset: get(networkClock, 'offset.quantity', null),
      };
    } catch (error) {
      logger.error('AdaApi::getNetworkClock error', { error, isForceCheck });
      throw new ApiError(error);
    }
  };

  getNetworkParameters = async (
    epochId: number
  ): Promise<GetNetworkParametersResponse> => {
    logger.debug('AdaApi::getNetworkParameters called');
    try {
      const networkParameters: NetworkParametersResponse = await getNetworkParameters(
        epochId,
        this.config
      );
      logger.debug('AdaApi::getNetworkParameters success', {
        networkParameters,
      });

      const {
        genesis_block_hash: genesisBlockHash,
        blockchain_start_time, // eslint-disable-line
        slot_length: slotLength,
        epoch_length: epochLength,
        epoch_stability: epochStability,
        active_slot_coefficient: activeSlotCoefficient,
      } = networkParameters;
      const blockchainStartTime = moment(blockchain_start_time).valueOf();

      return {
        genesisBlockHash,
        blockchainStartTime,
        slotLength,
        epochLength,
        epochStability,
        activeSlotCoefficient,
      };
    } catch (error) {
      logger.error('AdaApi::getNetworkParameters error', { error });
      throw new ApiError(error);
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
      throw new ApiError(error);
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
      const response: TransactionFee = await getDelegationFee(this.config, {
        walletId: request.walletId,
      });
      logger.debug('AdaApi::calculateDelegationFee success', { response });
      const delegationFee = _createDelegationFeeFromServerData(response);
      return delegationFee;
    } catch (error) {
      logger.error('AdaApi::calculateDelegationFee error', { error });
      throw new ApiError(error);
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
      throw new ApiError(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
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
      discovery,
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
    let walletRewardAmount = new BigNumber(0);
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
      discovery,
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
    const amount = get(data, ['estimated_max', 'quantity'], 0);
    return new BigNumber(amount).dividedBy(LOVELACES_PER_ADA);
  }
);

const _createMigrationFeeFromServerData = action(
  'AdaApi::_createMigrationFeeFromServerData',
  (data: TransferFundsCalculateFeeResponse) => {
    const amount = get(data, ['migration_cost', 'quantity'], 0);
    return new BigNumber(amount).dividedBy(LOVELACES_PER_ADA);
  }
);

const _createDelegationFeeFromServerData = action(
  'AdaApi::_createDelegationFeeFromServerData',
  (data: TransactionFee) => {
    const amount = get(data, ['estimated_max', 'quantity'], 0);
    return new BigNumber(amount).dividedBy(LOVELACES_PER_ADA);
  }
);

const _createStakePoolFromServerData = action(
  'AdaApi::_createStakePoolFromServerData',
  (stakePool: AdaApiStakePool, index: number) => {
    const {
      id,
      metrics,
      cost,
      margin: profitMargin,
      metadata,
      pledge,
    } = stakePool;
    const {
      relative_stake: relativeStake,
      produced_blocks: producedBlocks,
      saturation,
    } = metrics; // eslint-disable-line
    const { name, description = '', ticker, homepage } = metadata;
    const relativeStakePercentage = get(relativeStake, 'quantity', 0);
    const producedBlocksCount = get(producedBlocks, 'quantity', 0);
    const costQuantity = get(cost, 'quantity', 0);
    const pledgeQuantity = get(pledge, 'quantity', 0);
    const profitMarginPercentage = get(profitMargin, 'quantity', 0);
    return new StakePool({
      id,
      relativeStake: relativeStakePercentage,
      producedBlocks: producedBlocksCount,
      ticker,
      homepage,
      cost: new BigNumber(costQuantity).dividedBy(LOVELACES_PER_ADA),
      description,
      isCharity: false,
      name,
      pledge: new BigNumber(pledgeQuantity).dividedBy(LOVELACES_PER_ADA),
      profitMargin: profitMarginPercentage,
      ranking: index + 1,
      retiring: null,
      saturation: saturation * 100,
    });
  }
);
