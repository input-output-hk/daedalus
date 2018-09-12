// @flow
import { split, get } from 'lodash';
import { action } from 'mobx';
import { ipcRenderer } from 'electron';
import BigNumber from 'bignumber.js';
import { Logger, stringifyData, stringifyError } from '../../../common/logging';
import patchAdaApi from './utils/patchAdaApi';
import Wallet from '../domains/Wallet';
import WalletTransaction, { transactionTypes } from '../domains/WalletTransaction';
import WalletAddress from '../domains/WalletAddress';
import { isValidMnemonic } from '../../../common/decrypt';
import { isValidRedemptionKey, isValidPaperVendRedemptionKey } from '../../../common/redemption-key-validation';
import { LOVELACES_PER_ADA, MAX_TRANSACTIONS_PER_PAGE } from '../config/numbersConfig';
import { getAccounts } from './accounts/requests/getAccounts';
import { getAddress } from './addresses/requests/getAddress';
import { createAddress } from './addresses/requests/createAddress';
import { resetWalletState } from './wallets/requests/resetWalletState';
import { sendBugReport } from './common/requests/sendBugReport';
import { applyNodeUpdate } from './nodes/requests/applyNodeUpdate';
import { getNodeInfo } from './nodes/requests/getNodeInfo';
import { getNextNodeUpdate } from './nodes/requests/getNextNodeUpdate';
import { postponeNodeUpdate } from './nodes/requests/postponeNodeUpdate';
import { getTransactionFee } from './transactions/requests/getTransactionFee';
import { getTransactionHistory } from './transactions/requests/getTransactionHistory';
import { createTransaction } from './transactions/requests/createTransaction';
import { redeemAda } from './transactions/requests/redeemAda';
import { redeemAdaPaperVend } from './transactions/requests/redeemAdaPaperVend';
import { changeSpendingPassword } from './wallets/requests/changeSpendingPassword';
import { deleteWallet } from './wallets/requests/deleteWallet';
import { exportWalletAsJSON } from './wallets/requests/exportWalletAsJSON';
import { importWalletAsJSON } from './wallets/requests/importWalletAsJSON';
import { getWallets } from './wallets/requests/getWallets';
import { importWalletAsKey } from './wallets/requests/importWalletAsKey';
import { createWallet } from './wallets/requests/createWallet';
import { restoreWallet } from './wallets/requests/restoreWallet';
import { updateWallet } from './wallets/requests/updateWallet';
import { utcStringToDate, encryptPassphrase } from './utils';
import {
  unscrambleMnemonics,
  scrambleMnemonics,
  generateAccountMnemonics,
  generateAdditionalMnemonics
} from './utils/mnemonics';

import type {
  AdaAddress,
  AdaAddresses,
  AdaAccounts,
  AdaTransaction,
  AdaTransactions,
  AdaTransactionFee,
  AdaWallet,
  AdaWallets,
  WalletAssuranceLevel,
  RedeemAdaParams,
  RedeemPaperVendedAdaParams,
  RequestConfig,
  NodeInfo,
  NodeSoftware,
} from './types';
import type {
  CreateWalletRequest,
  DeleteWalletRequest,
  GetNetworkStatusResponse,
  GetTransactionsRequest,
  GetTransactionsResponse,
  RestoreWalletRequest,
  SendBugReportRequest,
  UpdateWalletPasswordRequest,
} from '../common';
import {
  GenericApiError,
  IncorrectWalletPasswordError,
  WalletAlreadyRestoredError,
  ReportRequestError,
  InvalidMnemonicError
} from '../common';
import {
  AllFundsAlreadyAtReceiverAddressError,
  NotAllowedToSendMoneyToRedeemAddressError,
  NotAllowedToSendMoneyToSameAddressError,
  NotEnoughFundsForTransactionFeesError,
  NotEnoughMoneyToSendError,
  RedeemAdaError,
  WalletAlreadyImportedError,
  WalletFileImportError,
} from './errors';
import {
  ADA_CERTIFICATE_MNEMONIC_LENGHT,
  ADA_REDEMPTION_PASSPHRASE_LENGHT,
  WALLET_RECOVERY_PHRASE_WORD_COUNT
} from '../../config/cryptoConfig';

/**
 * The api layer that is used for all requests to the
 * cardano backend when working with the ADA coin.
 */

// ADA specific Request / Response params
export type GetAddressesResponse = {
  accountIndex: ?number,
  addresses: AdaAddresses,
};

export type GetAddressesRequest = {
  walletId: string,
};
export type CreateAddressResponse = AdaAddress;
export type CreateAddressRequest = {
  spendingPassword?: string,
  accountIndex: number,
  walletId: string,
};
export type UpdateWalletRequest = {
  walletId: string,
  assuranceLevel: WalletAssuranceLevel,
  name: string
};
export type ImportWalletFromKeyRequest = {
  filePath: string,
  spendingPassword: ?string,
};
export type ImportWalletFromKeyResponse = AdaWallet;
export type ImportWalletFromFileRequest = {
  filePath: string,
  spendingPassword: ?string,
  walletName: ?string,
};
export type ImportWalletFromFileResponse = AdaWallet;
export type PostponeUpdateResponse = Promise<void>;
export type ApplyUpdateResponse = Promise<void>;
export type TransactionRequest = {
  accountIndex: number,
  walletId: string,
  address: string,
  amount: number,
  spendingPassword?: ?string,
};
export type TransactionFeeResponse = BigNumber;
export type ExportWalletToFileRequest = {
  walletId: string,
  filePath: string,
  password: ?string
};
export type CreateTransactionRequest = TransactionRequest;
export type TransactionFeeRequest = TransactionRequest;
export type ExportWalletToFileResponse = [];
export type GetWalletCertificateRecoveryPhraseRequest = {
  passphrase: string,
  input: string,
};
export type GetWalletRecoveryPhraseFromCertificateRequest = {
  passphrase: string,
  scrambledInput: string,
};

// const notYetImplemented = () => new Promise((_, reject) => {
//   reject(new ApiMethodNotYetImplementedError());
// });

// Commented out helper code for testing async APIs
// (async () => {
//   const result = await ClientApi.nextUpdate();
//   console.log('nextUpdate', result);
// })();

// Commented out helper code for testing sync APIs
// (() => {
//   const result = ClientApi.isValidRedeemCode('HSoXEnt9X541uHvtzBpy8vKfTo1C9TkAX3wat2c6ikg=');
//   console.log('isValidRedeemCode', result);
// })();

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
      return response.map(data => _createWalletFromServerV1Data(data));
    } catch (error) {
      Logger.error('AdaApi::getWallets error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  getAddresses = async (request: GetAddressesRequest): Promise<GetAddressesResponse> => {
    Logger.debug('AdaApi::getAddresses called: ' + stringifyData(request));
    const { walletId } = request;
    try {
      const accounts: AdaAccounts = await getAccounts(this.config, { walletId });
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
    const accounts: AdaAccounts = await getAccounts(this.config, { walletId });

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
      const {
        data: history,
        meta
      }: AdaTransactions = await getTransactionHistory(this.config, params);
      const { totalPages } = meta.pagination;
      const hasMultiplePages = (totalPages > 1 && limit > MAX_TRANSACTIONS_PER_PAGE);

      if (hasMultiplePages) {
        let page = 2;
        const hasNextPage = () => page < totalPages + 1;
        const shouldLoadNextPage = () => limit === null || page <= pagesToBeLoaded;

        for (page; (hasNextPage() && shouldLoadNextPage()); page++) {
          const { data: pageHistory } =
            await getTransactionHistory(this.config, Object.assign(params, { page }));
          history.push(...pageHistory);
        }
        if (limit !== null) history.splice(limit);
      }

      const transactions = history.map(data => _createTransactionFromServerDataV1(data));
      Logger.debug('AdaApi::searchHistory success: ' + stringifyData(history));
      return new Promise((resolve) => resolve({
        transactions,
        total: transactions.length,
      }));
    } catch (error) {
      Logger.error('AdaApi::searchHistory error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  createWallet = async (request: CreateWalletRequest): Promise<Wallet> => {
    Logger.debug('AdaApi::createWallet called');
    const { name, mnemonic, spendingPassword: passwordString } = request;
    const assuranceLevel = 'normal';
    try {
      const walletInitData = {
        operation: 'create',
        backupPhrase: split(mnemonic, ' '),
        assuranceLevel,
        name,
        spendingPassword: passwordString ? encryptPassphrase(passwordString) : null,
      };
      const wallet: AdaWallet = await createWallet(this.config, { walletInitData });
      Logger.debug('AdaApi::createWallet success');
      return _createWalletFromServerV1Data(wallet);
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
    request: CreateTransactionRequest
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
      const response: AdaTransaction = await createTransaction(this.config, { data });
      Logger.debug('AdaApi::createTransaction success: ' + stringifyData(response));
      return _createTransactionFromServerDataV1(response);
    } catch (error) {
      Logger.debug('AdaApi::createTransaction error: ' + stringifyError(error));
      // eslint-disable-next-line max-len
      if (error.message.includes('It\'s not allowed to send money to the same address you are sending from')) {
        throw new NotAllowedToSendMoneyToSameAddressError();
      }
      if (error.message.includes('Destination address can\'t be redeem address')) {
        throw new NotAllowedToSendMoneyToRedeemAddressError();
      }
      if (error.message.includes('Not enough money')) {
        throw new NotEnoughMoneyToSendError();
      }
      if (error.message.includes('Passphrase doesn\'t match')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new GenericApiError();
    }
  };

  calculateTransactionFee = async (
    request: TransactionFeeRequest
  ): Promise<TransactionFeeResponse> => {
    Logger.debug('AdaApi::calculateTransactionFee called');
    const { accountIndex, walletId, address, amount, spendingPassword } = request;
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
      const response: AdaTransactionFee = await getTransactionFee(this.config, { data });
      Logger.debug('AdaApi::calculateTransactionFee success: ' + stringifyData(response));
      return _createTransactionFeeFromServerData(response);
    } catch (error) {
      Logger.debug('AdaApi::calculateTransactionFee error: ' + stringifyError(error));
      // eslint-disable-next-line max-len
      if (error.message.includes('not enough money on addresses which are not included in output addresses set')) {
        throw new AllFundsAlreadyAtReceiverAddressError();
      }
      if (error.message.includes('not enough money')) {
        throw new NotEnoughFundsForTransactionFeesError();
      }
      throw new GenericApiError();
    }
  };

  createAddress = async (request: CreateAddressRequest): Promise<CreateAddressResponse> => {
    Logger.debug('AdaApi::createAddress called');
    const { spendingPassword: passwordString, accountIndex, walletId } = request;
    const spendingPassword = passwordString ? encryptPassphrase(passwordString) : '';
    try {
      const address: AdaAddress = await createAddress(
        this.config, { spendingPassword, accountIndex, walletId }
      );
      Logger.debug('AdaApi::createAddress success: ' + stringifyData(address));
      return _createAddressFromServerData(address);
    } catch (error) {
      Logger.debug('AdaApi::createAddress error: ' + stringifyError(error));
      if (error.message.includes('Passphrase doesn\'t match')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new GenericApiError();
    }
  };

  async isValidAddress(address: string): Promise<boolean> {
    Logger.debug('AdaApi::isValidAdaAddress called');
    try {
      const response: AdaAddress = await getAddress(this.config, { address });
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
    isValidMnemonic(mnemonic, ADA_REDEMPTION_PASSPHRASE_LENGHT)
  );

  isValidCertificateMnemonic = (mnemonic: string): boolean => (
    mnemonic.split(' ').length === ADA_CERTIFICATE_MNEMONIC_LENGHT
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
    const { passphrase, input } = request;
    try {
      const response: Promise<Array<string>> = new Promise(
        (resolve) => resolve(scrambleMnemonics({
          passphrase,
          scrambledInput: input,
        }))
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
      // TODO: backend will return something different here, if multiple wallets
      // are restored from the key and if there are duplicate wallets we will get
      // some kind of error and present the user with message that some wallets
      // where not imported/restored if some where. if no wallets are imported
      // we will error out completely with throw block below
      if (error.message.includes('Wallet with that mnemonics already exists')) {
        throw new WalletAlreadyRestoredError();
      }
      // We don't know what the problem was -> throw generic error
      throw new GenericApiError();
    }
  };

  importWalletFromKey = async (
    request: ImportWalletFromKeyRequest
  ): Promise<ImportWalletFromKeyResponse> => {
    Logger.debug('AdaApi::importWalletFromKey called');
    try {
      const importedWallet: AdaWallet = await importWalletAsKey(this.config, request);
      Logger.debug('AdaApi::importWalletFromKey success');
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      Logger.debug('AdaApi::importWalletFromKey error: ' + stringifyError(error));
      if (error.message.includes('already exists')) {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  };

  importWalletFromFile = async (
    request: ImportWalletFromFileRequest
  ): Promise<ImportWalletFromFileResponse> => {
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
      if (error.message.includes('already exists')) {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  };

  async redeemAda(request: RedeemAdaParams): Promise<AdaTransaction> {
    Logger.debug('AdaApi::redeemAda called');
    try {
      const transaction: AdaTransaction = await redeemAda(this.config, request);
      Logger.debug('AdaApi::redeemAda success');
      return _createTransactionFromServerDataV1(transaction);
    } catch (error) {
      Logger.debug('AdaApi::redeemAda error: ' + stringifyError(error));
      if (error.message.includes('Passphrase doesn\'t match')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new RedeemAdaError();
    }
  }

  async redeemPaperVendedAda(
    request: RedeemPaperVendedAdaParams
  ): Promise<AdaTransaction> {
    Logger.debug('AdaApi::redeemAdaPaperVend called');
    try {
      const transaction: AdaTransaction = await redeemAdaPaperVend(this.config, request);
      Logger.debug('AdaApi::redeemAdaPaperVend success');
      return _createTransactionFromServerDataV1(transaction);
    } catch (error) {
      Logger.debug('AdaApi::redeemAdaPaperVend error: ' + stringifyError(error));
      if (error.message.includes('Passphrase doesn\'t match')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new RedeemAdaError();
    }
  }

  async sendBugReport(requestFormData: SendBugReportRequest): Promise<*> {
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

  postponeUpdate = async (): PostponeUpdateResponse => {
    Logger.debug('AdaApi::postponeUpdate called');
    try {
      const response: Promise<any> = await postponeNodeUpdate(this.config);
      Logger.debug('AdaApi::postponeUpdate success: ' + stringifyData(response));
    } catch (error) {
      Logger.error('AdaApi::postponeUpdate error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  applyUpdate = async (): ApplyUpdateResponse => {
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
      return _createWalletFromServerV1Data(wallet);
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
      if (error.message.includes('Invalid old passphrase given')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new GenericApiError();
    }
  };

  exportWalletToFile = async (
    request: ExportWalletToFileRequest
  ): Promise<ExportWalletToFileResponse> => {
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

  getNetworkStatus = async (): Promise<GetNetworkStatusResponse> => {
    Logger.debug('AdaApi::getNetworkStatus called');
    try {
      const status: NodeInfo = await getNodeInfo(this.config);
      Logger.debug('AdaApi::getNetworkStatus success: ' + stringifyData(status));

      const {
        blockchainHeight,
        subscriptionStatus,
        syncProgress,
        localBlockchainHeight
      } = status;

      // extract relevant data before sending to NetworkStatusStore
      return {
        subscriptionStatus,
        syncProgress: syncProgress.quantity,
        blockchainHeight: get(blockchainHeight, 'quantity', null),
        localBlockchainHeight: localBlockchainHeight.quantity
      };
    } catch (error) {
      Logger.error('AdaApi::getNetworkStatus error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  // returns time difference in microseconds between user's local machine and NtpServer
  // ensures time on user's node is synced with peer nodes on the network
  getLocalTimeDifference = async (): Promise<number> => {
    Logger.debug('AdaApi::getLocalTimeDifference called');
    try {
      const response: NodeInfo = await getNodeInfo(this.config);
      Logger.debug('AdaApi::getLocalTimeDifference success: ' + stringifyData(response));
      const differenceFromNtpServer = get(
        response.localTimeInformation,
        'differenceFromNtpServer',
        null
      );

      // if the optional property 'differenceFromNtpServer' doesn't exist
      // return 0 microseconds, otherwise return the required property 'quantity'
      if (!differenceFromNtpServer) { return 0; }
      return differenceFromNtpServer.quantity;
    } catch (error) {
      Logger.error('AdaApi::getLocalTimeDifference error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }
}

// ========== TRANSFORM SERVER DATA INTO FRONTEND MODELS =========

const _createWalletFromServerData = action(
  'AdaApi::_createWalletFromServerData', (data: AdaWallet) => (
    new Wallet({
      id: data.id,
      amount: BigNumber(data.balance).dividedBy(LOVELACES_PER_ADA),
      name: data.name,
      assurance: data.assuranceLevel,
      hasPassword: data.hasSpendingPassword,
      passwordUpdateDate: new Date(`${data.spendingPasswordLastUpdate}Z`),
    })
  )
);

const _createAddressFromServerData = action(
  'AdaApi::_createAddressFromServerData',
  (address: AdaAddress) => new WalletAddress(address)
);

const _conditionToTxStateV1 = (condition: string) => {
  switch (condition) {
    case 'applying':
    case 'creating': return 'pending';
    case 'wontApply': return 'failed';
    default: return 'ok';
    // Others V0: CPtxInBlocks && CPtxNotTracked
    // Others V1: "inNewestBlocks" "persisted" "creating"
  }
};

const _createTransactionFromServerDataV1 = action(
  'AdaApi::_createTransactionFromServerData', (data: AdaTransaction) => {
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
      state: _conditionToTxStateV1(status.tag),
    });
  }
);

const _createTransactionFeeFromServerData = action(
  'AdaApi::_createTransactionFeeFromServerData', (data: AdaTransactionFee) =>
    new BigNumber(data.estimatedAmount).dividedBy(LOVELACES_PER_ADA)
);


// ========== V1 API =========

const _createWalletFromServerV1Data = action(
  'AdaApi::_createWalletFromServerV1Data', (data: AdaWallet) => {
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
