// @flow
import { split, get } from 'lodash';
import { action } from 'mobx';
import { ipcRenderer } from 'electron';
import BigNumber from 'bignumber.js';

// domains
import Wallet from '../domains/Wallet';
import WalletTransaction, { transactionTypes } from '../domains/WalletTransaction';
import WalletAddress from '../domains/WalletAddress';

// Accounts requests
import { getAccounts } from './accounts/requests/getAccounts';

// Addresses requests
import { getAddress } from './addresses/requests/getAddress';
import { createAddress } from './addresses/requests/createAddress';

// Common requests
import { sendBugReport } from './common/requests/sendBugReport';

// Nodes requests
import { applyNodeUpdate } from './nodes/requests/applyNodeUpdate';
import { getNodeInfo } from './nodes/requests/getNodeInfo';
import { getNextNodeUpdate } from './nodes/requests/getNextNodeUpdate';
import { postponeNodeUpdate } from './nodes/requests/postponeNodeUpdate';

// Transactions requests
import { getTransactionFee } from './transactions/requests/getTransactionFee';
import { getTransactionHistory } from './transactions/requests/getTransactionHistory';
import { createTransaction } from './transactions/requests/createTransaction';
import { redeemAda } from './transactions/requests/redeemAda';
import { redeemPaperVendedAda } from './transactions/requests/redeemPaperVendedAda';

// Wallets requests
import { resetWalletState } from './wallets/requests/resetWalletState';
import { changeSpendingPassword } from './wallets/requests/changeSpendingPassword';
import { deleteWallet } from './wallets/requests/deleteWallet';
import { exportWalletAsJSON } from './wallets/requests/exportWalletAsJSON';
import { importWalletAsJSON } from './wallets/requests/importWalletAsJSON';
import { getWallets } from './wallets/requests/getWallets';
import { importWalletAsKey } from './wallets/requests/importWalletAsKey';
import { createWallet } from './wallets/requests/createWallet';
import { restoreWallet } from './wallets/requests/restoreWallet';
import { updateWallet } from './wallets/requests/updateWallet';

// utility functions
import patchAdaApi from './utils/patchAdaApi';
import { isValidMnemonic } from '../../../common/decrypt';
import { utcStringToDate, encryptPassphrase } from './utils';
import {
  Logger,
  stringifyData,
  stringifyError
} from '../../../common/logging';
import {
  isValidRedemptionKey,
  isValidPaperVendRedemptionKey
} from '../utils/redemption-key-validation';
import {
  unscrambleMnemonics,
  scrambleMnemonics,
  generateAccountMnemonics,
  generateAdditionalMnemonics
} from './utils/mnemonics';

// config constants
import {
  LOVELACES_PER_ADA,
  MAX_TRANSACTIONS_PER_PAGE
} from '../config/numbersConfig';
import {
  ADA_CERTIFICATE_MNEMONIC_LENGTH,
  ADA_REDEMPTION_PASSPHRASE_LENGTH,
  WALLET_RECOVERY_PHRASE_WORD_COUNT
} from '../config/cryptoConfig';

// Accounts types
import type { Accounts } from './accounts/types';

// Addresses Types
import type {
  Address,
  GetAddressesRequest,
  CreateAddressRequest,
  GetAddressesResponse
} from './addresses/types';

// Common Types
import type {
  RequestConfig,
  SendBugReportRequest
} from './common/types';

// Nodes Types
import type {
  NodeInfo,
  NodeSoftware,
  GetNetworkStatusResponse
} from './nodes/types';
import type { NodeQueryParams } from './nodes/requests/getNodeInfo';

// Transactions Types
import type { RedeemAdaParams } from './transactions/requests/redeemAda';
import type { RedeemPaperVendedAdaParams } from './transactions/requests/redeemPaperVendedAda';
import type {
  Transaction,
  Transactions,
  TransactionFee,
  TransactionRequest,
  GetTransactionsRequest,
  GetTransactionsResponse
} from './transactions/types';

// Wallets Types
import type {
  AdaWallet,
  AdaWallets,
  CreateWalletRequest,
  DeleteWalletRequest,
  RestoreWalletRequest,
  UpdateWalletPasswordRequest,
  ExportWalletToFileRequest,
  GetWalletCertificateRecoveryPhraseRequest,
  GetWalletRecoveryPhraseFromCertificateRequest,
  ImportWalletFromKeyRequest,
  ImportWalletFromFileRequest,
  UpdateWalletRequest
} from './wallets/types';

// Common errors
import {
  GenericApiError,
  IncorrectWalletPasswordError,
  ReportRequestError,
  InvalidMnemonicError,
  ForbiddenMnemonicError
} from './common/errors';

// Wallets errors
import {
  WalletAlreadyRestoredError,
  WalletAlreadyImportedError,
  WalletFileImportError
} from './wallets/errors';

// Transactions errors
import {
  NotAllowedToSendMoneyToRedeemAddressError,
  NotEnoughFundsForTransactionFeesError,
  NotEnoughMoneyToSendError,
  RedeemAdaError
} from './transactions/errors';


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
      Logger.debug('AdaApi::getWallets success: ' + stringifyData(response));
      return response.map(data => _createWalletFromServerData(data));
    } catch (error) {
      Logger.error('AdaApi::getWallets error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  getAddresses = async (request: GetAddressesRequest): Promise<GetAddressesResponse> => {
    Logger.debug('AdaApi::getAddresses called: ' + stringifyData(request));
    const { walletId } = request;
    try {
      const accounts: Accounts = await getAccounts(this.config, { walletId });
      Logger.debug('AdaApi::getAddresses success: ' + stringifyData(accounts));

      if (!accounts || !accounts.length) {
        return new Promise(resolve => resolve({ accountIndex: null, addresses: [] }));
      }

      // For now only the first wallet account is used
      const firstAccount = accounts[0];
      const { index: accountIndex, addresses } = firstAccount;

      return new Promise(resolve => resolve({ accountIndex, addresses }));
    } catch (error) {
      Logger.error('AdaApi::getAddresses error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  getTransactions = async (request: GetTransactionsRequest): Promise<GetTransactionsResponse> => {
    Logger.debug('AdaApi::searchHistory called: ' + stringifyData(request));
    const { walletId, skip, limit } = request;
    const accounts: Accounts = await getAccounts(this.config, { walletId });

    if (!accounts.length || !accounts[0].index) {
      return new Promise(resolve => resolve({ transactions: [], total: 0 }));
    }

    let perPage = limit;
    if (limit === null || limit > MAX_TRANSACTIONS_PER_PAGE) {
      perPage = MAX_TRANSACTIONS_PER_PAGE;
    }

    const params = {
      accountIndex: accounts[0].index,
      page: skip === 0 ? 1 : (skip / limit) + 1,
      per_page: perPage,
      wallet_id: walletId,
      sort_by: 'DES[created_at]',
    };
    const pagesToBeLoaded = Math.ceil(limit / params.per_page);

    try {
      const response: Transactions = await getTransactionHistory(this.config, params);
      const { meta, data: txnHistory } = response;
      const { totalPages } = meta.pagination;
      const hasMultiplePages = (totalPages > 1 && limit > MAX_TRANSACTIONS_PER_PAGE);

      if (hasMultiplePages) {
        let page = 2;
        const hasNextPage = () => page < totalPages + 1;
        const shouldLoadNextPage = () => limit === null || page <= pagesToBeLoaded;

        for (page; (hasNextPage() && shouldLoadNextPage()); page++) {
          const { data: pageHistory } =
            await getTransactionHistory(this.config, Object.assign(params, { page }));
          txnHistory.push(...pageHistory);
        }
        if (limit !== null) txnHistory.splice(limit);
      }

      const transactions = txnHistory.map(txn => _createTransactionFromServerData(txn));
      const total = transactions.length;
      Logger.debug('AdaApi::searchHistory success: ' + stringifyData(txnHistory));
      return new Promise(resolve => resolve({ transactions, total }));
    } catch (error) {
      Logger.error('AdaApi::searchHistory error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  createWallet = async (request: CreateWalletRequest): Promise<Wallet> => {
    Logger.debug('AdaApi::createWallet called');
    const { name, mnemonic, spendingPassword } = request;
    const assuranceLevel = 'normal';
    try {
      const walletInitData = {
        operation: 'create',
        backupPhrase: split(mnemonic, ' '),
        assuranceLevel,
        name,
        spendingPassword: spendingPassword ? encryptPassphrase(spendingPassword) : null,
      };
      const wallet: AdaWallet = await createWallet(this.config, { walletInitData });
      Logger.debug('AdaApi::createWallet success');
      return _createWalletFromServerData(wallet);
    } catch (error) {
      Logger.error('AdaApi::createWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  deleteWallet = async (request: DeleteWalletRequest): Promise<boolean> => {
    Logger.debug('AdaApi::deleteWallet called: ' + stringifyData(request));
    try {
      const { walletId } = request;
      const response = await deleteWallet(this.config, { walletId });
      Logger.debug('AdaApi::deleteWallet success: ' + stringifyData(response));
      return true;
    } catch (error) {
      Logger.error('AdaApi::deleteWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  createTransaction = async (
    request: TransactionRequest
  ): Promise<WalletTransaction> => {
    Logger.debug('AdaApi::createTransaction called');
    const { accountIndex, walletId, address, amount, spendingPassword: passwordString } = request;
    const spendingPassword = passwordString ? encryptPassphrase(passwordString) : '';
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
      const response: Transaction = await createTransaction(this.config, { data });
      Logger.debug('AdaApi::createTransaction success: ' + stringifyData(response));
      return _createTransactionFromServerData(response);
    } catch (error) {
      Logger.debug('AdaApi::createTransaction error: ' + stringifyError(error));
      if (error.message === 'OutputIsRedeem') {
        throw new NotAllowedToSendMoneyToRedeemAddressError();
      }
      if (error.message === 'NotEnoughMoney') {
        throw new NotEnoughMoneyToSendError();
      }
      if (error.message === 'CannotCreateAddress') {
        throw new IncorrectWalletPasswordError();
      }
      throw new GenericApiError();
    }
  };

  calculateTransactionFee = async (
    request: TransactionRequest
  ): Promise<BigNumber> => {
    Logger.debug('AdaApi::calculateTransactionFee called');
    const { accountIndex, walletId, address, amount, spendingPassword: passwordString } = request;
    const spendingPassword = passwordString ? encryptPassphrase(passwordString) : '';
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
      const response: TransactionFee = await getTransactionFee(this.config, { data });
      Logger.debug('AdaApi::calculateTransactionFee success: ' + stringifyData(response));
      return _createTransactionFeeFromServerData(response);
    } catch (error) {
      Logger.debug('AdaApi::calculateTransactionFee error: ' + stringifyError(error));
      if (error.message === 'NotEnoughMoney') {
        throw new NotEnoughFundsForTransactionFeesError();
      }
      throw new GenericApiError();
    }
  };

  createAddress = async (request: CreateAddressRequest): Promise<Address> => {
    Logger.debug('AdaApi::createAddress called');
    const { spendingPassword: passwordString, accountIndex, walletId } = request;
    const spendingPassword = passwordString ? encryptPassphrase(passwordString) : '';
    try {
      const address: Address = await createAddress(
        this.config, { spendingPassword, accountIndex, walletId }
      );
      Logger.debug('AdaApi::createAddress success: ' + stringifyData(address));
      return _createAddressFromServerData(address);
    } catch (error) {
      Logger.debug('AdaApi::createAddress error: ' + stringifyError(error));
      if (error.message === 'CannotCreateAddress') {
        throw new IncorrectWalletPasswordError();
      }
      throw new GenericApiError();
    }
  };

  async isValidAddress(address: string): Promise<boolean> {
    Logger.debug('AdaApi::isValidAdaAddress called');
    try {
      const response: Address = await getAddress(this.config, { address });
      Logger.debug(`AdaApi::isValidAdaAddress success: ${stringifyData(response)}`);
      return true;
    } catch (error) {
      Logger.debug(`AdaApi::isValidAdaAddress error: ${stringifyError(error)}`);
      return false;
    }
  }

  isValidMnemonic = (mnemonic: string): boolean => (
    isValidMnemonic(mnemonic, WALLET_RECOVERY_PHRASE_WORD_COUNT)
  );

  isValidRedemptionKey = (mnemonic: string): boolean => (isValidRedemptionKey(mnemonic));

  isValidPaperVendRedemptionKey = (mnemonic: string): boolean => (
    isValidPaperVendRedemptionKey(mnemonic)
  );

  isValidRedemptionMnemonic = (mnemonic: string): boolean => (
    isValidMnemonic(mnemonic, ADA_REDEMPTION_PASSPHRASE_LENGTH)
  );

  isValidCertificateMnemonic = (mnemonic: string): boolean => (
    mnemonic.split(' ').length === ADA_CERTIFICATE_MNEMONIC_LENGTH
  );

  getWalletRecoveryPhrase(): Promise<Array<string>> {
    Logger.debug('AdaApi::getWalletRecoveryPhrase called');
    try {
      const response: Promise<Array<string>> = new Promise(
        (resolve) => resolve(generateAccountMnemonics())
      );
      Logger.debug('AdaApi::getWalletRecoveryPhrase success');
      return response;
    } catch (error) {
      Logger.error('AdaApi::getWalletRecoveryPhrase error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  // eslint-disable-next-line max-len
  getWalletCertificateAdditionalMnemonics(): Promise<Array<string>> {
    Logger.debug('AdaApi::getWalletCertificateAdditionalMnemonics called');
    try {
      const response: Promise<Array<string>> = new Promise(
        (resolve) => resolve(generateAdditionalMnemonics())
      );
      Logger.debug('AdaApi::getWalletCertificateAdditionalMnemonics success');
      return response;
    } catch (error) {
      Logger.error('AdaApi::getWalletCertificateAdditionalMnemonics error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  getWalletCertificateRecoveryPhrase(
    request: GetWalletCertificateRecoveryPhraseRequest
  ): Promise<Array<string>> {
    Logger.debug('AdaApi::getWalletCertificateRecoveryPhrase called');
    const { passphrase, input: scrambledInput } = request;
    try {
      const response: Promise<Array<string>> = new Promise(
        (resolve) => resolve(scrambleMnemonics({ passphrase, scrambledInput }))
      );
      Logger.debug('AdaApi::getWalletCertificateRecoveryPhrase success');
      return response;
    } catch (error) {
      Logger.error('AdaApi::getWalletCertificateRecoveryPhrase error: ' + stringifyError(error));
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
      Logger.debug('AdaApi::getWalletRecoveryPhraseFromCertificate error: ' + stringifyError(error));
      return Promise.reject(new InvalidMnemonicError());
    }
  }

  restoreWallet = async (request: RestoreWalletRequest): Promise<Wallet> => {
    Logger.debug('AdaApi::restoreWallet called');
    const { recoveryPhrase, walletName, walletPassword: passwordString } = request;
    const assuranceLevel = 'normal';
    const spendingPassword = passwordString ? encryptPassphrase(passwordString) : null;
    const walletInitData = {
      operation: 'restore',
      backupPhrase: split(recoveryPhrase, ' '),
      assuranceLevel,
      name: walletName,
      spendingPassword
    };

    try {
      const wallet: AdaWallet = await restoreWallet(this.config, { walletInitData });
      Logger.debug('AdaApi::restoreWallet success');
      return _createWalletFromServerData(wallet);
    } catch (error) {
      Logger.debug('AdaApi::restoreWallet error: ' + stringifyError(error));
      if (error.message === 'WalletAlreadyExists') {
        throw new WalletAlreadyRestoredError();
      }
      if (error.message === 'JSONValidationFailed') {
        const validationError = get(error, 'diagnostic.validationError', '');
        if (validationError.includes('Forbidden Mnemonic: an example Mnemonic has been submitted')) {
          throw new ForbiddenMnemonicError();
        }
      }
      throw new GenericApiError();
    }
  };

  importWalletFromKey = async (
    request: ImportWalletFromKeyRequest
  ): Promise<Wallet> => {
    Logger.debug('AdaApi::importWalletFromKey called');
    try {
      const importedWallet: AdaWallet = await importWalletAsKey(this.config, request);
      Logger.debug('AdaApi::importWalletFromKey success');
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      Logger.debug('AdaApi::importWalletFromKey error: ' + stringifyError(error));
      if (error.message === 'WalletAlreadyExists') {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  };

  importWalletFromFile = async (
    request: ImportWalletFromFileRequest
  ): Promise<Wallet> => {
    Logger.debug('AdaApi::importWalletFromFile called');
    const { filePath } = request;
    const isKeyFile = filePath.split('.').pop().toLowerCase() === 'key';
    try {
      const importedWallet: AdaWallet = isKeyFile ? (
        await importWalletAsKey(this.config, request)
      ) : (
        await importWalletAsJSON(this.config, filePath)
      );
      Logger.debug('AdaApi::importWalletFromFile success');
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      Logger.debug('AdaApi::importWalletFromFile error: ' + stringifyError(error));
      if (error.message === 'WalletAlreadyExists') {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  };

  async redeemAda(request: RedeemAdaParams): Promise<WalletTransaction> {
    Logger.debug('AdaApi::redeemAda called');
    try {
      const transaction: Transaction = await redeemAda(this.config, request);
      Logger.debug('AdaApi::redeemAda success');
      return _createTransactionFromServerData(transaction);
    } catch (error) {
      Logger.debug('AdaApi::redeemAda error: ' + stringifyError(error));
      if (error.message === 'CannotCreateAddress') {
        throw new IncorrectWalletPasswordError();
      }
      throw new RedeemAdaError();
    }
  }

  async redeemPaperVendedAda(
    request: RedeemPaperVendedAdaParams
  ): Promise<WalletTransaction> {
    Logger.debug('AdaApi::redeemAdaPaperVend called');
    try {
      const transaction: Transaction = await redeemPaperVendedAda(this.config, request);
      Logger.debug('AdaApi::redeemAdaPaperVend success');
      return _createTransactionFromServerData(transaction);
    } catch (error) {
      Logger.debug('AdaApi::redeemAdaPaperVend error: ' + stringifyError(error));
      if (error.message === 'CannotCreateAddress') {
        throw new IncorrectWalletPasswordError();
      }
      throw new RedeemAdaError();
    }
  }

  async sendBugReport(requestFormData: SendBugReportRequest): Promise<any> {
    Logger.debug('AdaApi::sendBugReport called: ' + stringifyData(requestFormData));
    try {
      await sendBugReport({ requestFormData });
      Logger.debug('AdaApi::sendBugReport success');
      return true;
    } catch (error) {
      Logger.error('AdaApi::sendBugReport error: ' + stringifyError(error));
      throw new ReportRequestError();
    }
  }

  nextUpdate = async (): Promise<NodeSoftware | null> => {
    Logger.debug('AdaApi::nextUpdate called');
    try {
      const nodeUpdate = await getNextNodeUpdate(this.config);
      if (nodeUpdate && nodeUpdate.version) {
        Logger.debug('AdaApi::nextUpdate success: ' + stringifyData(nodeUpdate));
        return nodeUpdate;
      }
      Logger.debug('AdaApi::nextUpdate success: No Update Available');
    } catch (error) {
      Logger.error('AdaApi::nextUpdate error: ' + stringifyError(error));
      throw new GenericApiError();
    }
    return null;
  };

  postponeUpdate = async (): Promise<void> => {
    Logger.debug('AdaApi::postponeUpdate called');
    try {
      const response: Promise<any> = await postponeNodeUpdate(this.config);
      Logger.debug('AdaApi::postponeUpdate success: ' + stringifyData(response));
    } catch (error) {
      Logger.error('AdaApi::postponeUpdate error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  applyUpdate = async (): Promise<void> => {
    Logger.debug('AdaApi::applyUpdate called');
    try {
      const response: Promise<any> = await applyNodeUpdate(this.config);
      Logger.debug('AdaApi::applyUpdate success: ' + stringifyData(response));
      ipcRenderer.send('kill-process');
    } catch (error) {
      Logger.error('AdaApi::applyUpdate error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  updateWallet = async (request: UpdateWalletRequest): Promise<Wallet> => {
    Logger.debug('AdaApi::updateWallet called: ' + stringifyData(request));
    const { walletId, assuranceLevel, name } = request;
    try {
      const wallet: AdaWallet = await updateWallet(
        this.config, { walletId, assuranceLevel, name }
      );
      Logger.debug('AdaApi::updateWallet success: ' + stringifyData(wallet));
      return _createWalletFromServerData(wallet);
    } catch (error) {
      Logger.error('AdaApi::updateWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  updateWalletPassword = async (
    request: UpdateWalletPasswordRequest
  ): Promise<boolean> => {
    Logger.debug('AdaApi::updateWalletPassword called');
    const { walletId, oldPassword, newPassword } = request;
    try {
      await changeSpendingPassword(this.config, { walletId, oldPassword, newPassword });
      Logger.debug('AdaApi::updateWalletPassword success');
      return true;
    } catch (error) {
      Logger.debug('AdaApi::updateWalletPassword error: ' + stringifyError(error));
      const errorMessage = get(error, 'diagnostic.msg', '');
      if (errorMessage.includes('UpdateWalletPasswordOldPasswordMismatch')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new GenericApiError();
    }
  };

  exportWalletToFile = async (
    request: ExportWalletToFileRequest
  ): Promise<[]> => {
    const { walletId, filePath } = request;
    Logger.debug('AdaApi::exportWalletToFile called');
    try {
      const response: Promise<[]> = await exportWalletAsJSON(this.config, {
        walletId,
        filePath
      });
      Logger.debug('AdaApi::exportWalletToFile success: ' + stringifyData(response));
      return response;
    } catch (error) {
      Logger.error('AdaApi::exportWalletToFile error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  testReset = async (): Promise<void> => {
    Logger.debug('AdaApi::testReset called');
    try {
      const response: Promise<void> = await resetWalletState(this.config);
      Logger.debug('AdaApi::testReset success: ' + stringifyData(response));
      return response;
    } catch (error) {
      Logger.error('AdaApi::testReset error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  getNetworkStatus = async (
    queryParams?: NodeQueryParams
  ): Promise<GetNetworkStatusResponse> => {
    const isForceNTPCheck = !!queryParams;
    const loggerText = `AdaApi::getNetworkStatus${isForceNTPCheck ? ' (FORCE-NTP-CHECK)' : ''}`;
    Logger.debug(`${loggerText} called`);
    try {
      const status: NodeInfo = await getNodeInfo(this.config, queryParams);
      Logger.debug(`${loggerText} success: ${stringifyData(status)}`);

      const {
        blockchainHeight,
        subscriptionStatus,
        syncProgress,
        localBlockchainHeight,
        localTimeInformation,
      } = status;

      // extract relevant data before sending to NetworkStatusStore
      return {
        subscriptionStatus,
        syncProgress: syncProgress.quantity,
        blockchainHeight: get(blockchainHeight, 'quantity', 0),
        localBlockchainHeight: localBlockchainHeight.quantity,
        localTimeDifference: get(localTimeInformation, 'differenceFromNtpServer.quantity', null),
      };
    } catch (error) {
      Logger.error(`${loggerText} error: ${stringifyError(error)}`);
      throw new GenericApiError(error);
    }
  };
}

// ========== TRANSFORM SERVER DATA INTO FRONTEND MODELS =========

const _createWalletFromServerData = action(
  'AdaApi::_createWalletFromServerData', (data: AdaWallet) => {
    const {
      id, balance, name, assuranceLevel,
      hasSpendingPassword, spendingPasswordLastUpdate,
      syncState,
    } = data;

    return new Wallet({
      id,
      amount: new BigNumber(balance).dividedBy(LOVELACES_PER_ADA),
      name,
      assurance: assuranceLevel,
      hasPassword: hasSpendingPassword,
      passwordUpdateDate: new Date(`${spendingPasswordLastUpdate}Z`),
      syncState,
    });
  }
);

const _createAddressFromServerData = action(
  'AdaApi::_createAddressFromServerData',
  (address: Address) => new WalletAddress(address)
);

const _conditionToTxState = (condition: string) => {
  switch (condition) {
    case 'applying':
    case 'creating': return 'pending';
    case 'wontApply': return 'failed';
    default: return 'ok';
    // Others V0: CPtxInBlocks && CPtxNotTracked
    // Others V1: "inNewestBlocks" "persisted" "creating"
  }
};

const _createTransactionFromServerData = action(
  'AdaApi::_createTransactionFromServerData', (data: Transaction) => {
    const { id, direction, amount, confirmations, creationTime, inputs, outputs, status } = data;
    return new WalletTransaction({
      id,
      title: direction === 'outgoing' ? 'Ada sent' : 'Ada received',
      type: direction === 'outgoing' ? transactionTypes.EXPEND : transactionTypes.INCOME,
      amount: new BigNumber(direction === 'outgoing' ? (amount * -1) : amount).dividedBy(LOVELACES_PER_ADA),
      date: utcStringToDate(creationTime),
      description: '',
      numberOfConfirmations: confirmations,
      addresses: {
        from: inputs.map(({ address }) => address),
        to: outputs.map(({ address }) => address),
      },
      state: _conditionToTxState(status.tag),
    });
  }
);

const _createTransactionFeeFromServerData = action(
  'AdaApi::_createTransactionFeeFromServerData', (data: TransactionFee) =>
    new BigNumber(data.estimatedAmount).dividedBy(LOVELACES_PER_ADA)
);
