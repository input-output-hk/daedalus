// @flow
import { split, get, size } from 'lodash';
import { action } from 'mobx';
import BigNumber from 'bignumber.js';
// import moment from 'moment';

// domains
import Wallet, { WalletDelegationStatuses } from '../domains/Wallet';
import {
  WalletTransaction,
  transactionTypes,
} from '../domains/WalletTransaction';
import WalletAddress from '../domains/WalletAddress';

// Addresses requests
import { getAddress } from './addresses/requests/getAddress';
import { getAddresses } from './addresses/requests/getAddresses';
import { createAddress } from './addresses/requests/createAddress';

// Nodes requests
import { applyNodeUpdate } from './nodes/requests/applyNodeUpdate';
// import { getNodeInfo } from './nodes/requests/getNodeInfo';
import { getNodeSettings } from './nodes/requests/getNodeSettings';
import { getCurrentEpoch } from './nodes/requests/getCurrentEpoch';
// import { getNextNodeUpdate } from './nodes/requests/getNextNodeUpdate';
import { postponeNodeUpdate } from './nodes/requests/postponeNodeUpdate';
import { getLatestAppVersion } from './nodes/requests/getLatestAppVersion';

// Transactions requests
import { getTransactionFee } from './transactions/requests/getTransactionFee';
// import { getTransactionHistory } from './transactions/requests/getTransactionHistory';
import { createTransaction } from './transactions/requests/createTransaction';

// Wallets requests
import { changeSpendingPassword } from './wallets/requests/changeSpendingPassword';
import { deleteWallet } from './wallets/requests/deleteWallet';
import { exportWalletAsJSON } from './wallets/requests/exportWalletAsJSON';
import { importWalletAsJSON } from './wallets/requests/importWalletAsJSON';
import { getWallets } from './wallets/requests/getWallets';
import { importWalletAsKey } from './wallets/requests/importWalletAsKey';
import { createWallet } from './wallets/requests/createWallet';
import { restoreWallet } from './wallets/requests/restoreWallet';
import { updateWallet } from './wallets/requests/updateWallet';
import { getWalletUtxos } from './wallets/requests/getWalletUtxos';

// utility functions
import {
  awaitUpdateChannel,
  cardanoFaultInjectionChannel,
} from '../ipc/cardano.ipc';
import patchAdaApi from './utils/patchAdaApi';
import { isValidMnemonic } from '../../../common/crypto/decrypt';
import { utcStringToDate, encryptPassphrase } from './utils';
import { Logger } from '../utils/logging';
import {
  unscrambleMnemonics,
  scrambleMnemonics,
  generateAccountMnemonics,
  generateAdditionalMnemonics,
} from './utils/mnemonics';
import { filterLogData } from '../../../common/utils/logging';

// config constants
import {
  LOVELACES_PER_ADA,
  // MAX_TRANSACTIONS_PER_PAGE,
  MAX_TRANSACTION_CONFIRMATIONS,
  // TX_AGE_POLLING_THRESHOLD,
} from '../config/numbersConfig';
import {
  ADA_CERTIFICATE_MNEMONIC_LENGTH,
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from '../config/cryptoConfig';

// Accounts types
// import type { Accounts } from './accounts/types';

// Addresses Types
import type {
  Address,
  Addresses,
  GetAddressesRequest,
  GetAddressesResponse,
  CreateAddressRequest,
} from './addresses/types';

// Common Types
import type { RequestConfig } from './common/types';

// Nodes Types
import type {
  CardanoExplorerResponse,
  LatestAppVersionInfoResponse,
  // NodeInfoResponse,
  NodeSettingsResponse,
  NodeSoftware,
  GetNetworkStatusResponse,
  GetNodeSettingsResponse,
  GetCurrentEpochFallbackResponse,
  GetLatestAppVersionResponse,
} from './nodes/types';
import type { NodeInfoQueryParams } from './nodes/requests/getNodeInfo';

// Transactions Types
import type {
  Transaction,
  // Transactions,
  TransactionFee,
  TransactionRequest,
  GetTransactionsRequest,
  GetTransactionsResponse,
} from './transactions/types';

// Wallets Types
import type {
  AdaWallet,
  AdaWallets,
  WalletUtxos,
  CreateWalletRequest,
  DeleteWalletRequest,
  RestoreWalletRequest,
  UpdateSpendingPasswordRequest,
  ExportWalletToFileRequest,
  GetWalletCertificateRecoveryPhraseRequest,
  GetWalletRecoveryPhraseFromCertificateRequest,
  ImportWalletFromKeyRequest,
  ImportWalletFromFileRequest,
  UpdateWalletRequest,
  GetWalletUtxosRequest,
} from './wallets/types';

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
  CanNotCalculateTransactionFeesError,
  NotAllowedToSendMoneyToRedeemAddressError,
  NotEnoughFundsForTransactionFeesError,
  NotEnoughFundsForTransactionError,
  NotEnoughMoneyToSendError,
  TooBigTransactionError,
} from './transactions/errors';
import type { FaultInjectionIpcRequest } from '../../../common/types/cardano-node.types';
import { TlsCertificateNotValidError } from './nodes/errors';

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
      const response: AdaWallets = await getWallets(this.config);
      Logger.debug('AdaApi::getWallets success', { wallets: response });
      return response.map(_createWalletFromServerData);
    } catch (error) {
      Logger.error('AdaApi::getWallets error', { error });
      throw new GenericApiError();
    }
  };

  getAddresses = async (
    request: GetAddressesRequest
  ): Promise<GetAddressesResponse> => {
    Logger.debug('AdaApi::getAddresses called', {
      parameters: filterLogData(request),
    });
    const { walletId, queryParams } = request;
    try {
      const response: Addresses = await getAddresses(
        this.config,
        walletId,
        queryParams
      );

      Logger.debug('AdaApi::getAddresses success', { addresses: response });
      const addresses = response.map(_createAddressFromServerData);

      return new Promise(resolve => resolve({ accountIndex: 0, addresses }));
    } catch (error) {
      Logger.error('AdaApi::getAddresses error', { error });
      throw new GenericApiError();
    }
  };

  getTransactions = async (
    request: GetTransactionsRequest
  ): Promise<GetTransactionsResponse> => {
    // const requestTimestamp = moment();
    const requestStats = Object.assign({}, request, {
      cachedTransactions: request.cachedTransactions.length,
    });
    Logger.debug('AdaApi::searchHistory called', { parameters: requestStats });

    /*
    const {
      walletId,
      skip,
      limit,
      isFirstLoad, // during first load we fetch all wallet's transactions
      isRestoreActive, // during restoration we fetch only missing transactions
      isRestoreCompleted, // once restoration is done we fetch potentially missing transactions
      cachedTransactions,
    } , unionBy= request;
    */
    // NOTE: Not yet available in the API
    return new Promise(resolve => resolve({ transactions: [], total: 0 }));

    /* TODO: Uncomment once API available

    const accounts: Accounts = await getAccounts(this.config, { walletId });

    if (!accounts.length || !accounts[0].index) {
      return new Promise(resolve => resolve({ transactions: [], total: 0 }));
    }

    let perPage = limit;
    const shouldLoadAll = limit === null;
    if (shouldLoadAll || limit > MAX_TRANSACTIONS_PER_PAGE) {
      perPage = MAX_TRANSACTIONS_PER_PAGE;
    }

    const params = {
      wallet_id: walletId,
      account_index: accounts[0].index,
      page: skip === 0 ? 1 : skip + 1,
      per_page: perPage,
      sort_by: 'DES[created_at]',
      created_at: `LTE[${moment.utc().format('YYYY-MM-DDTHH:mm:ss')}]`,
      // ^^ By setting created_at filter to current time we make sure
      // all subsequent multi-pages requests load the same set of transactions
    };

    const shouldLoadOnlyFresh =
      !isFirstLoad && !isRestoreActive && !isRestoreCompleted;
    if (shouldLoadOnlyFresh) {
      const tenMinutesAgo = moment
        .utc(Date.now() - TX_AGE_POLLING_THRESHOLD)
        .format('YYYY-MM-DDTHH:mm:ss');
      // Since we load all transactions in a first load, later on we only care about fresh ones
      Object.assign(params, { created_at: `GTE[${tenMinutesAgo}]` });
    }

    const pagesToBeLoaded = Math.ceil(limit / params.per_page);

    try {
      // Load first page of transactions
      const response: Transactions = await getTransactionHistory(
        this.config,
        params
      );
      const { meta, data: txHistory } = response;
      const { totalPages, totalEntries: totalTransactions } = meta.pagination;

      let transactions = txHistory.map(tx =>
        _createTransactionFromServerData(tx)
      );

      // Load additional pages of transactions
      const hasMultiplePages =
        totalPages > 1 && (shouldLoadAll || limit > perPage);
      if (hasMultiplePages) {
        let page = 2;
        const hasNextPage = () => {
          const hasMorePages = page < totalPages + 1;
          if ((isRestoreActive || isRestoreCompleted) && hasMorePages) {
            const loadedTransactions = unionBy(
              transactions,
              cachedTransactions,
              'id'
            );
            const hasMoreTransactions =
              totalTransactions - loadedTransactions.length > 0;
            return hasMoreTransactions;
          }
          return hasMorePages;
        };
        const shouldLoadNextPage = () =>
          shouldLoadAll || page <= pagesToBeLoaded;

        if (isRestoreActive || isRestoreCompleted) {
          const latestLoadedTransactionDate = transactions[0].date;
          const latestLoadedTransactionDateString = moment
            .utc(latestLoadedTransactionDate)
            .format('YYYY-MM-DDTHH:mm:ss');
          // During restoration we need to fetch only transactions older than the latest loaded one
          // as this ensures that both totalPages and totalEntries remain unchanged throught out
          // subsequent page loads (as in the meantime new transactions can be discovered)
          Object.assign(params, {
            created_at: `LTE[${latestLoadedTransactionDateString}]`,
          });
        }

        for (page; hasNextPage() && shouldLoadNextPage(); page++) {
          const { data: pageHistory } = await getTransactionHistory(
            this.config,
            Object.assign(params, { page })
          );
          transactions.push(
            ...pageHistory.map(tx => _createTransactionFromServerData(tx))
          );
        }
      }

      // Merge newly loaded and previously loaded transactions
      // - unionBy also serves the purpose of removing transaction duplicates
      //   which may occur as a side-effect of transaction request pagination
      //   as multi-page requests are not executed at the exact same time!
      transactions = unionBy(transactions, cachedTransactions, 'id');

      // Enforce the limit in case we are not loading all transactions
      if (!shouldLoadAll) transactions.splice(limit);

      const total = transactions.length;

      const responseStats = {
        apiRequested: limit || 'all',
        apiFiltered: shouldLoadOnlyFresh ? 'fresh' : '',
        apiReturned: totalTransactions,
        apiPagesTotal: totalPages,
        apiPagesRequested: params.page,
        daedalusCached: cachedTransactions.length,
        daedalusLoaded: total - cachedTransactions.length,
        daedalusTotal: total,
        requestDurationInMs: moment
          .duration(moment().diff(requestTimestamp))
          .as('milliseconds'),
      };
      Logger.debug(
        `AdaApi::searchHistory success: ${total} transactions loaded`,
        { responseStats }
      );
      return new Promise(resolve => resolve({ transactions, total }));
    } catch (error) {
      Logger.error('AdaApi::searchHistory error', { error });
      throw new GenericApiError();
    }

    */
  };

  createWallet = async (request: CreateWalletRequest): Promise<Wallet> => {
    Logger.debug('AdaApi::createWallet called', {
      parameters: filterLogData(request),
    });
    const {
      name,
      mnemonic,
      mnemonicPassphrase,
      spendingPassword: passwordString,
      addressPoolGap,
    } = request;
    const spendingPassword = passwordString
      ? encryptPassphrase(passwordString)
      : '';
    try {
      const walletInitData = {
        name,
        mnemonic_sentence: split(mnemonic, ' '),
        mnemonic_second_factor: mnemonicPassphrase,
        passphrase: spendingPassword,
        address_pool_gap: addressPoolGap,
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
      const { walletId } = request;
      const response = await deleteWallet(this.config, { walletId });
      Logger.debug('AdaApi::deleteWallet success', { response });
      return true;
    } catch (error) {
      Logger.error('AdaApi::deleteWallet error', { error });
      throw new GenericApiError();
    }
  };

  createTransaction = async (
    request: TransactionRequest
  ): Promise<WalletTransaction> => {
    Logger.debug('AdaApi::createTransaction called', {
      parameters: filterLogData(request),
    });
    const {
      accountIndex,
      walletId,
      address,
      amount,
      spendingPassword: passwordString,
    } = request;
    const spendingPassword = passwordString
      ? encryptPassphrase(passwordString)
      : '';
    try {
      const data = {
        source: {
          accountIndex,
          walletId,
        },
        destinations: [
          {
            address,
            amount,
          },
        ],
        groupingPolicy: 'OptimizeForSecurity',
        spendingPassword,
      };
      const response: Transaction = await createTransaction(this.config, {
        data,
      });
      Logger.debug('AdaApi::createTransaction success', {
        transaction: response,
      });
      return _createTransactionFromServerData(response);
    } catch (error) {
      Logger.error('AdaApi::createTransaction error', { error });
      if (error.message === 'OutputIsRedeem') {
        throw new NotAllowedToSendMoneyToRedeemAddressError();
      }
      if (
        error.message === 'NotEnoughMoney' ||
        error.message === 'UtxoNotEnoughFragmented'
      ) {
        throw new NotEnoughMoneyToSendError();
      }
      if (error.message === 'CannotCreateAddress') {
        throw new IncorrectSpendingPasswordError();
      }
      if (error.message === 'TooBigTransaction') {
        throw new TooBigTransactionError();
      }
      throw new GenericApiError();
    }
  };

  calculateTransactionFee = async (
    request: TransactionRequest
  ): Promise<BigNumber> => {
    Logger.debug('AdaApi::calculateTransactionFee called', {
      parameters: filterLogData(request),
    });
    const { accountIndex, walletId, walletBalance, address, amount } = request;
    try {
      const data = {
        source: {
          accountIndex,
          walletId,
        },
        destinations: [
          {
            address,
            amount,
          },
        ],
        groupingPolicy: 'OptimizeForSecurity',
      };
      const response: TransactionFee = await getTransactionFee(this.config, {
        data,
      });
      Logger.debug('AdaApi::calculateTransactionFee success', {
        transactionFee: response,
      });
      return _createTransactionFeeFromServerData(response);
    } catch (error) {
      Logger.error('AdaApi::calculateTransactionFee error', { error });
      if (
        error.message === 'NotEnoughMoney' ||
        error.message === 'UtxoNotEnoughFragmented'
      ) {
        const errorMessage = get(error, 'diagnostic.details.msg', '');
        if (errorMessage.includes('Not enough coins to cover fee')) {
          // Amount + fees exceeds walletBalance:
          // - error.diagnostic.details.msg === 'Not enough coins to cover fee.'
          // = show "Not enough Ada for fees. Try sending a smaller amount."
          throw new NotEnoughFundsForTransactionFeesError();
        } else if (
          errorMessage.includes('Not enough available coins to proceed')
        ) {
          const availableBalance = new BigNumber(
            get(error, 'diagnostic.details.availableBalance', 0)
          ).dividedBy(LOVELACES_PER_ADA);
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
        } else {
          // Amount exceeds walletBalance:
          // = show "Not enough Ada. Try sending a smaller amount."
          throw new NotEnoughFundsForTransactionError();
        }
      }
      if (error.message === 'TooBigTransaction') {
        throw new TooBigTransactionError();
      }
      throw new GenericApiError();
    }
  };

  createAddress = async (
    request: CreateAddressRequest
  ): Promise<WalletAddress> => {
    Logger.debug('AdaApi::createAddress called', {
      parameters: filterLogData(request),
    });
    const {
      accountIndex,
      walletId,
      spendingPassword: passwordString,
    } = request;
    const spendingPassword = passwordString
      ? encryptPassphrase(passwordString)
      : '';
    try {
      const address: Address = await createAddress(this.config, {
        spendingPassword,
        accountIndex,
        walletId,
      });
      Logger.debug('AdaApi::createAddress success', { address });
      return _createAddressFromServerData(address);
    } catch (error) {
      Logger.error('AdaApi::createAddress error', { error });
      if (error.message === 'CannotCreateAddress') {
        throw new IncorrectSpendingPasswordError();
      }
      throw new GenericApiError();
    }
  };

  async isValidAddress(address: string): Promise<boolean> {
    Logger.debug('AdaApi::isValidAdaAddress called', {
      parameters: { address },
    });
    try {
      const response: Address = await getAddress(this.config, { address });
      Logger.debug('AdaApi::isValidAdaAddress success', { response });
      return true;
    } catch (error) {
      Logger.error('AdaApi::isValidAdaAddress error', { error });
      return false;
    }
  }

  isValidMnemonic = (mnemonic: string): boolean =>
    isValidMnemonic(mnemonic, WALLET_RECOVERY_PHRASE_WORD_COUNT);

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

  // eslint-disable-next-line max-len
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
    const {
      recoveryPhrase,
      walletName,
      spendingPassword: passwordString,
    } = request;
    const spendingPassword = passwordString
      ? encryptPassphrase(passwordString)
      : '';
    const walletInitData = {
      mnemonic_sentence: split(recoveryPhrase, ' '),
      name: walletName,
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

  importWalletFromKey = async (
    request: ImportWalletFromKeyRequest
  ): Promise<Wallet> => {
    Logger.debug('AdaApi::importWalletFromKey called', {
      parameters: filterLogData(request),
    });
    const { filePath, spendingPassword: passwordString } = request;
    const spendingPassword = passwordString
      ? encryptPassphrase(passwordString)
      : '';
    try {
      const importedWallet: AdaWallet = await importWalletAsKey(this.config, {
        filePath,
        spendingPassword,
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
    const { filePath, spendingPassword: passwordString } = request;
    const spendingPassword = passwordString
      ? encryptPassphrase(passwordString)
      : '';
    const isKeyFile =
      filePath
        .split('.')
        .pop()
        .toLowerCase() === 'key';
    try {
      const importedWallet: AdaWallet = isKeyFile
        ? await importWalletAsKey(this.config, { filePath, spendingPassword })
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
      if (errorCode === 'wrong_encryption_passphrase') {
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

  testReset = async (): Promise<void> => {
    Logger.debug('AdaApi::testReset called');
    try {
      const wallets: AdaWallets = await getWallets(this.config);
      wallets.map(wallet => deleteWallet(this.config, { walletId: wallet.id }));
      Logger.debug('AdaApi::testReset success');
    } catch (error) {
      Logger.error('AdaApi::testReset error', { error });
      throw new GenericApiError();
    }
  };

  getNetworkStatus = async (
    queryInfoParams?: NodeInfoQueryParams
  ): Promise<GetNetworkStatusResponse> => {
    const isForceNTPCheck = !!queryInfoParams;
    const loggerText = `AdaApi::getNetworkStatus${
      isForceNTPCheck ? ' (FORCE-NTP-CHECK)' : ''
    }`;
    Logger.debug(`${loggerText} called`);
    try {
      /* @API TODO: Uncomment once implemented

      const nodeInfo: NodeInfoResponse = await getNodeInfo(
        this.config,
        queryInfoParams
      );
      Logger.debug(`${loggerText} success`, { nodeInfo });

      const {
        blockchainHeight,
        subscriptionStatus,
        syncProgress,
        localBlockchainHeight,
        localTimeInformation,
      } = nodeInfo;
      */

      const blockchainHeight = { quantity: 100 };
      const subscriptionStatus = 'subscribed';
      const syncProgress = { quantity: 1 };
      const localTimeInformation = { status: 'available' };
      const localBlockchainHeight = { quantity: 100 };

      // extract relevant data before sending to NetworkStatusStore
      return {
        subscriptionStatus,
        syncProgress: syncProgress.quantity,
        blockchainHeight: get(blockchainHeight, 'quantity', 0),
        localBlockchainHeight: localBlockchainHeight.quantity,
        localTimeInformation: {
          status: localTimeInformation.status,
          difference: get(
            localTimeInformation,
            'localTimeDifference.quantity',
            null
          ),
        },
      };
    } catch (error) {
      Logger.error(`${loggerText} error`, { error });
      if (error.code === TlsCertificateNotValidError.API_ERROR) {
        throw new TlsCertificateNotValidError();
      }
      throw new GenericApiError(error);
    }
  };

  getNodeSettings = async (): Promise<GetNodeSettingsResponse> => {
    Logger.debug('AdaApi::getNodeSettings called');
    try {
      const nodeSettings: NodeSettingsResponse = await getNodeSettings(
        this.config
      );
      Logger.debug('AdaApi::getNodeSettings success', {
        nodeSettings,
      });
      const { slotId } = nodeSettings;
      return { slotId };
    } catch (error) {
      Logger.error('AdaApi::getNodeSettings error', { error });
      if (error.code === TlsCertificateNotValidError.API_ERROR) {
        throw new TlsCertificateNotValidError();
      }
      throw new GenericApiError(error);
    }
  };

  getCurrentEpochFallback = async (): Promise<GetCurrentEpochFallbackResponse> => {
    Logger.debug('AdaApi::getCurrentEpochFallback called');
    try {
      const currentEpochInfo: CardanoExplorerResponse = await getCurrentEpoch();
      const currentEpochPath = 'Right[1][0].cbeEpoch';
      const currentEpoch = get(currentEpochInfo, currentEpochPath, null);
      Logger.debug('AdaApi::getCurrentEpochFallback success', {
        currentEpoch,
        currentEpochInfo,
      });
      return { currentEpoch };
    } catch (error) {
      Logger.error('AdaApi::getCurrentEpochFallback error', { error });
      throw new GenericApiError();
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

  setCardanoNodeFault = async (fault: FaultInjectionIpcRequest) => {
    await cardanoFaultInjectionChannel.send(fault);
  };

  // No implementation here but can be overwritten
  getLocalTimeDifference: Function;
  setLocalTimeDifference: Function;
  setNextUpdate: Function;
  setSubscriptionStatus: Function;
  setLocalBlockHeight: Function;
  setNetworkBlockHeight: Function;
  setLatestAppVersion: Function;
  setApplicationVersion: Function;
  setFaultyNodeSettingsApi: boolean;
}

// ========== TRANSFORM SERVER DATA INTO FRONTEND MODELS =========

const _createWalletFromServerData = action(
  'AdaApi::_createWalletFromServerData',
  (data: AdaWallet) => {
    const {
      id,
      address_pool_gap, // eslint-disable-line
      balance,
      name,
      state,
      passphrase,
      delegation,
    } = data;

    const isDelegated =
      delegation.status === WalletDelegationStatuses.DELEGATING;
    const passphraseLastUpdatedAt = get(passphrase, 'last_updated_at', null);
    const walletBalance =
      balance.total.unit === 'lovelace'
        ? new BigNumber(balance.total.quantity).dividedBy(LOVELACES_PER_ADA)
        : new BigNumber(balance.total.quantity);

    return new Wallet({
      id,
      addressPoolGap: address_pool_gap,
      name,
      amount: walletBalance,
      hasPassword: size(passphrase) > 0,
      passwordUpdateDate:
        passphraseLastUpdatedAt && new Date(passphraseLastUpdatedAt),
      syncState: state,
      isLegacy: false, // @API TODO - legacy declaration not exist for now
      isDelegated,
      // @API TODO - integrate once "Stake Pools" endpoints are done
      // inactiveStakePercentage: 0,
      // delegatedStakePool: new StakePool(),
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

const _conditionToTxState = (condition: string) => {
  switch (condition) {
    case 'applying':
    case 'creating':
      return 'pending';
    case 'wontApply':
      return 'failed';
    default:
      return 'ok';
    // Others V0: CPtxInBlocks && CPtxNotTracked
    // Others V1: "inNewestBlocks" "persisted" "creating"
  }
};

const _createTransactionFromServerData = action(
  'AdaApi::_createTransactionFromServerData',
  (data: Transaction) => {
    const {
      id,
      direction,
      amount,
      confirmations,
      creationTime,
      inputs,
      outputs,
      status,
    } = data;
    return new WalletTransaction({
      id,
      title: direction === 'outgoing' ? 'Ada sent' : 'Ada received',
      type:
        direction === 'outgoing'
          ? transactionTypes.EXPEND
          : transactionTypes.INCOME,
      amount: new BigNumber(
        direction === 'outgoing' ? amount * -1 : amount
      ).dividedBy(LOVELACES_PER_ADA),
      date: utcStringToDate(creationTime),
      description: '',
      numberOfConfirmations: Math.min(
        confirmations,
        MAX_TRANSACTION_CONFIRMATIONS + 1
      ),
      addresses: {
        from: inputs.map(({ address }) => address),
        to: outputs.map(({ address }) => address),
      },
      state: _conditionToTxState(status.tag),
    });
  }
);

const _createTransactionFeeFromServerData = action(
  'AdaApi::_createTransactionFeeFromServerData',
  (data: TransactionFee) =>
    new BigNumber(data.estimatedAmount).dividedBy(LOVELACES_PER_ADA)
);
