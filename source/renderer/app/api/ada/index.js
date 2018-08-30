// @flow
import { split } from 'lodash';
import { action } from 'mobx';
import { ipcRenderer, remote } from 'electron';
import BigNumber from 'bignumber.js';
import { Logger, stringifyData, stringifyError } from '../../../../common/logging';
import { unixTimestampToDate, utcStringToDate } from './lib/utils';
import { encryptPassphrase } from './lib/encryptPassphrase';
import Wallet from '../../domains/Wallet';
import WalletTransaction, { transactionTypes } from '../../domains/WalletTransaction';
import WalletAddress from '../../domains/WalletAddress';
import { isValidMnemonic } from '../../../../common/decrypt';
import { isValidRedemptionKey, isValidPaperVendRedemptionKey } from '../../../../common/redemption-key-validation';
import { LOVELACES_PER_ADA } from '../../config/numbersConfig';
import environment from '../../../../common/environment';
import patchAdaApi from './mocks/patchAdaApi';

import { getAdaWallets } from './getAdaWallets';
import { changeAdaWalletPassphrase } from './changeAdaWalletPassphrase';
import { deleteAdaWallet } from './deleteAdaWallet';
import { newAdaWallet } from './newAdaWallet';
import { newAdaWalletAddress } from './newAdaWalletAddress';
import { restoreAdaWallet } from './restoreAdaWallet';
import { updateAdaWallet } from './updateAdaWallet';
import { exportAdaBackupJSON } from './exportAdaBackupJSON';
import { importAdaBackupJSON } from './importAdaBackupJSON';
import { importAdaWallet } from './importAdaWallet';
import { getAdaWalletAccounts } from './getAdaWalletAccounts';
import { getAdaAddress } from './getAdaAddress';
import { adaTxFee } from './adaTxFee';
import { newAdaPayment } from './newAdaPayment';
import { redeemAda } from './redeemAda';
import { redeemAdaPaperVend } from './redeemAdaPaperVend';
import { nextAdaUpdate } from './nextAdaUpdate';
import { postponeAdaUpdate } from './postponeAdaUpdate';
import { applyAdaUpdate } from './applyAdaUpdate';
import { adaTestReset } from './adaTestReset';
import { getAdaHistoryByWallet } from './getAdaHistoryByWallet';
import { getAdaAccountRecoveryPhrase } from './getAdaAccountRecoveryPhrase';
import { getAdaWalletCertificateAdditionalMnemonics } from './getAdaWalletCertificateAdditionalMnemonics';
import { getAdaWalletCertificateRecoveryPhrase } from './getAdaWalletCertificateRecoveryPhrase';
import { getAdaWalletRecoveryPhraseFromCertificate } from './getAdaWalletRecoveryPhraseFromCertificate';
import { getNodeInfo } from './getNodeInfo';
import { sendAdaBugReport } from './sendAdaBugReport';

import type {
  AdaAddress,
  AdaAddresses,
  AdaAccounts,
  AdaTransaction,
  AdaTransactionV1,
  AdaTransactionsV1,
  AdaTransactionFee,
  AdaWalletV0,
  AdaWallet,
  AdaV1Wallet,
  AdaV1Wallets,
  AdaWalletRecoveryPhraseResponse,
  AdaWalletCertificateAdditionalMnemonicsResponse,
  AdaWalletCertificateRecoveryPhraseResponse,
  GetWalletCertificateAdditionalMnemonicsResponse,
  GetWalletCertificateRecoveryPhraseResponse,
  GetWalletRecoveryPhraseFromCertificateResponse,
  NodeInfo,
  ResponseBaseV1,
  AdaV1Assurance
} from './types';

import type {
  CreateWalletRequest,
  CreateWalletResponse,
  CreateTransactionResponse,
  DeleteWalletRequest,
  DeleteWalletResponse,
  GetLocalTimeDifferenceResponse,
  GetSyncProgressResponse,
  GetTransactionsRequest,
  GetTransactionsResponse,
  GetWalletRecoveryPhraseResponse,
  GetWalletsResponse,
  IsValidAddressResponse,
  RestoreWalletRequest,
  RestoreWalletResponse,
  SendBugReportRequest,
  SendBugReportResponse,
  UpdateWalletResponse,
  UpdateWalletPasswordRequest,
  UpdateWalletPasswordResponse,
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

const ca = remote.getGlobal('ca');

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
  assuranceLevel: AdaV1Assurance,
  name: string
};
export type RedeemAdaRequest = {
  redemptionCode: string,
  accountId: string,
  walletPassword: ?string,
};
export type RedeemAdaResponse = Wallet;
export type RedeemPaperVendedAdaRequest = {
  shieldedRedemptionKey: string,
  mnemonics: string,
  accountId: string,
  walletPassword: ?string,
};
export type RedeemPaperVendedAdaResponse = RedeemPaperVendedAdaRequest;
export type ImportWalletFromKeyRequest = {
  filePath: string,
  walletPassword: ?string,
};
export type ImportWalletFromKeyResponse = AdaWalletV0;
export type ImportWalletFromFileRequest = {
  filePath: string,
  walletPassword: ?string,
  walletName: ?string,
};
export type ImportWalletFromFileResponse = AdaWalletV0;
export type NodeSettings = {
  slotDuration: {
    quantity: number,
    unit?: 'milliseconds'
  },
  softwareInfo: {
    version: number,
    applicationName: string
  },
  projectVersion: string,
  gitRevision: string
};
export type NextUpdateResponse = ?{
  data?: NodeSettings,
  ...ResponseBaseV1
};
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

  constructor() {
    if (environment.isTest()) {
      patchAdaApi(this);
    }
  }

  async getWallets(): Promise<GetWalletsResponse> {
    Logger.debug('AdaApi::getWallets called');
    try {
      const response: AdaV1Wallets = await getAdaWallets({ ca });
      Logger.debug('AdaApi::getWallets success: ' + stringifyData(response));
      return response.map(data => _createWalletFromServerV1Data(data));
    } catch (error) {
      Logger.error('AdaApi::getWallets error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async getAddresses(request: GetAddressesRequest): Promise<GetAddressesResponse> {
    Logger.debug('AdaApi::getAddresses called: ' + stringifyData(request));
    const { walletId } = request;
    try {
      const accounts: AdaAccounts = await getAdaWalletAccounts({ ca, walletId });
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
  }

  async getTransactions(request: GetTransactionsRequest): Promise<GetTransactionsResponse> {
    Logger.debug('AdaApi::searchHistory called: ' + stringifyData(request));
    const { walletId, skip, limit } = request;

    const accounts: AdaAccounts = await getAdaWalletAccounts({ ca, walletId });
    const accountIndex = accounts[0].index;
    const page = skip === 0 ? 1 : (skip / limit) + 1;
    const perPage = limit > 50 ? 50 : limit;

    const params = {
      accountIndex,
      ca,
      page,
      per_page: perPage,
      wallet_id: walletId,
      sort_by: 'created_at',
    };

    try {
      const history: AdaTransactionsV1 = await getAdaHistoryByWallet(params);
      const transactions = history.map(data => _createTransactionFromServerDataV1(data));
      Logger.debug('AdaApi::searchHistory success: ' + stringifyData(transactions));

      return new Promise((resolve) => resolve({
        transactions,
        total: transactions.length,
      }));
    } catch (error) {
      Logger.error('AdaApi::searchHistory error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async createWallet(request: CreateWalletRequest): Promise<CreateWalletResponse> {
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
      const wallet: AdaWallet = await newAdaWallet({ ca, walletInitData });
      Logger.debug('AdaApi::createWallet success');
      return _createWalletFromServerV1Data(wallet);
    } catch (error) {
      Logger.error('AdaApi::createWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async deleteWallet(request: DeleteWalletRequest): Promise<DeleteWalletResponse> {
    Logger.debug('AdaApi::deleteWallet called: ' + stringifyData(request));
    try {
      const { walletId } = request;
      const response = await deleteAdaWallet({ ca, walletId });
      Logger.debug('AdaApi::deleteWallet success: ' + stringifyData(response));
      return true;
    } catch (error) {
      Logger.error('AdaApi::deleteWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async createTransaction(request: CreateTransactionRequest): Promise<CreateTransactionResponse> {
    Logger.debug('AdaApi::createTransaction called');
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
      const response: AdaTransactionV1 = await newAdaPayment({ ca, data });
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
  }

  async calculateTransactionFee(request: TransactionFeeRequest): Promise<TransactionFeeResponse> {
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
      const response: adaTxFee = await adaTxFee({ ca, data });
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
  }

  async createAddress(request: CreateAddressRequest): Promise<CreateAddressResponse> {
    Logger.debug('AdaApi::createAddress called');
    const { spendingPassword: passwordString, accountIndex, walletId } = request;
    const spendingPassword = encryptPassphrase(passwordString);
    try {
      const address: AdaAddress = await newAdaWalletAddress(
        { ca, spendingPassword, accountIndex, walletId }
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
  }

  async isValidAddress(address: string): Promise<IsValidAddressResponse> {
    Logger.debug('AdaApi::isValidAdaAddress called');
    try {
      const response: AdaAddress = await getAdaAddress({ ca, address });
      Logger.debug(`AdaApi::isValidAdaAddress success: ${stringifyData(response)}`);
      return true;
    } catch (error) {
      Logger.debug(`AdaApi::isValidAdaAddress error: ${stringifyError(error)}`);
      return false;
    }
  }

  isValidMnemonic(mnemonic: string): Promise<boolean> {
    return isValidMnemonic(mnemonic, WALLET_RECOVERY_PHRASE_WORD_COUNT);
  }

  isValidRedemptionKey(mnemonic: string): Promise<boolean> {
    return isValidRedemptionKey(mnemonic);
  }

  isValidPaperVendRedemptionKey(mnemonic: string): Promise<boolean> {
    return isValidPaperVendRedemptionKey(mnemonic);
  }

  isValidRedemptionMnemonic(mnemonic: string): Promise<boolean> {
    return isValidMnemonic(mnemonic, ADA_REDEMPTION_PASSPHRASE_LENGHT);
  }

  isValidCertificateMnemonic(mnemonic: string): boolean {
    return mnemonic.split(' ').length === ADA_CERTIFICATE_MNEMONIC_LENGHT;
  }

  getWalletRecoveryPhrase(): Promise<GetWalletRecoveryPhraseResponse> {
    Logger.debug('AdaApi::getWalletRecoveryPhrase called');
    try {
      const response: Promise<AdaWalletRecoveryPhraseResponse> = new Promise(
        (resolve) => resolve(getAdaAccountRecoveryPhrase())
      );
      Logger.debug('AdaApi::getWalletRecoveryPhrase success');
      return response;
    } catch (error) {
      Logger.error('AdaApi::getWalletRecoveryPhrase error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  // eslint-disable-next-line max-len
  getWalletCertificateAdditionalMnemonics(): Promise<GetWalletCertificateAdditionalMnemonicsResponse> {
    Logger.debug('AdaApi::getWalletCertificateAdditionalMnemonics called');
    try {
      const response: Promise<AdaWalletCertificateAdditionalMnemonicsResponse> = new Promise(
        (resolve) => resolve(getAdaWalletCertificateAdditionalMnemonics())
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
  ): Promise<GetWalletCertificateRecoveryPhraseResponse> {
    Logger.debug('AdaApi::getWalletCertificateRecoveryPhrase called');
    const { passphrase, input } = request;
    try {
      const response: Promise<AdaWalletCertificateRecoveryPhraseResponse> = new Promise(
        (resolve) => resolve(getAdaWalletCertificateRecoveryPhrase({
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
  ): Promise<GetWalletRecoveryPhraseFromCertificateResponse> {
    Logger.debug('AdaApi::getWalletRecoveryPhraseFromCertificate called');
    const { passphrase, scrambledInput } = request;
    try {
      const response = getAdaWalletRecoveryPhraseFromCertificate({ passphrase, scrambledInput });
      Logger.debug('AdaApi::getWalletRecoveryPhraseFromCertificate success');
      return Promise.resolve(response);
    } catch (error) {
      Logger.debug('AdaApi::getWalletRecoveryPhraseFromCertificate error: ' + stringifyError(error));
      return Promise.reject(new InvalidMnemonicError());
    }
  }

  async restoreWallet(request: RestoreWalletRequest): Promise<RestoreWalletResponse> {
    Logger.debug('AdaApi::restoreWallet called');
    const { recoveryPhrase, walletName, walletPassword } = request;
    const assuranceLevel = 'normal';
    const spendingPassword = encryptPassphrase(walletPassword);
    const walletInitData = {
      operation: 'restore',
      backupPhrase: split(recoveryPhrase, ' '),
      assuranceLevel,
      name: walletName,
      spendingPassword
    };

    try {
      const wallet: AdaWallet = await restoreAdaWallet({ ca, walletInitData });
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
  }

  async importWalletFromKey(
    request: ImportWalletFromKeyRequest
  ): Promise<ImportWalletFromKeyResponse> {
    Logger.debug('AdaApi::importWalletFromKey called');
    const { filePath, walletPassword } = request;
    try {
      const importedWallet: AdaWalletV0 = await importAdaWallet(
        { ca, walletPassword, filePath }
      );
      Logger.debug('AdaApi::importWalletFromKey success');
      return _createWalletFromServerDataV0(importedWallet);
    } catch (error) {
      Logger.debug('AdaApi::importWalletFromKey error: ' + stringifyError(error));
      if (error.message.includes('already exists')) {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  }

  async importWalletFromFile(
    request: ImportWalletFromFileRequest
  ): Promise<ImportWalletFromFileResponse> {
    Logger.debug('AdaApi::importWalletFromFile called');
    const { filePath, walletPassword } = request;
    const isKeyFile = filePath.split('.').pop().toLowerCase() === 'key';
    try {
      const importedWallet: AdaWalletV0 = isKeyFile ? (
        await importAdaWallet({ ca, walletPassword, filePath })
      ) : (
        await importAdaBackupJSON({ ca, filePath })
      );
      Logger.debug('AdaApi::importWalletFromFile success');
      return _createWalletFromServerDataV0(importedWallet);
    } catch (error) {
      Logger.debug('AdaApi::importWalletFromFile error: ' + stringifyError(error));
      if (error.message.includes('already exists')) {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  }

  async redeemAda(request: RedeemAdaRequest): Promise<RedeemAdaResponse> {
    Logger.debug('AdaApi::redeemAda called');
    const { redemptionCode, accountId, walletPassword } = request;
    try {
      const walletRedeemData = {
        crWalletId: accountId,
        crSeed: redemptionCode,
      };

      const response: AdaTransaction = await redeemAda(
        { ca, walletPassword, walletRedeemData }
      );

      Logger.debug('AdaApi::redeemAda success');
      return _createTransactionFromServerData(response);
    } catch (error) {
      Logger.debug('AdaApi::redeemAda error: ' + stringifyError(error));
      if (error.message.includes('Passphrase doesn\'t match')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new RedeemAdaError();
    }
  }

  async redeemPaperVendedAda(
    request: RedeemPaperVendedAdaRequest
  ): Promise<RedeemPaperVendedAdaResponse> {
    Logger.debug('AdaApi::redeemAdaPaperVend called');
    const { shieldedRedemptionKey, mnemonics, accountId, walletPassword } = request;
    try {
      const redeemPaperVendedData = {
        pvWalletId: accountId,
        pvSeed: shieldedRedemptionKey,
        pvBackupPhrase: {
          bpToList: split(mnemonics),
        }
      };

      const response: AdaTransaction = await redeemAdaPaperVend(
        { ca, walletPassword, redeemPaperVendedData }
      );

      Logger.debug('AdaApi::redeemAdaPaperVend success');
      return _createTransactionFromServerData(response);
    } catch (error) {
      Logger.debug('AdaApi::redeemAdaPaperVend error: ' + stringifyError(error));
      if (error.message.includes('Passphrase doesn\'t match')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new RedeemAdaError();
    }
  }

  async sendBugReport(requestFormData: SendBugReportRequest): Promise<SendBugReportResponse> {
    Logger.debug('AdaApi::sendBugReport called: ' + stringifyData(requestFormData));
    try {
      await sendAdaBugReport({ requestFormData });
      Logger.debug('AdaApi::sendBugReport success');
      return true;
    } catch (error) {
      Logger.error('AdaApi::sendBugReport error: ' + stringifyError(error));
      throw new ReportRequestError();
    }
  }

  async nextUpdate(): Promise<NextUpdateResponse> {
    Logger.debug('AdaApi::nextUpdate called');
    let nextUpdate = null;
    try {
      const response = await nextAdaUpdate({ ca });
      const { projectVersion, softwareInfo } = response;
      const { version, applicationName } = softwareInfo;

      Logger.debug('AdaApi::nextUpdate success: ' + stringifyData(response));
      if (version > projectVersion) {
        nextUpdate = {
          version,
          name: applicationName
        };
      }
    } catch (error) {
      if (error.message.includes('No updates available')) {
        Logger.debug('AdaApi::nextUpdate success: No updates available');
      } else {
        Logger.error('AdaApi::nextUpdate error: ' + stringifyError(error));
      }
      // throw new GenericApiError();
    }
    return nextUpdate;
    // TODO: remove hardcoded response after node update is tested
    // nextUpdate = {
    //   cuiSoftwareVersion: {
    //     svAppName: {
    //       getApplicationName: 'cardano'
    //     },
    //     svNumber: 1
    //   },
    //   cuiBlockVesion: {
    //     bvMajor: 0,
    //     bvMinor: 1,
    //     bvAlt: 0
    //   },
    //   cuiScriptVersion: 1,
    //   cuiImplicit: false,
    //   cuiVotesFor: 2,
    //   cuiVotesAgainst: 0,
    //   cuiPositiveStake: {
    //     getCoin: 66666
    //   },
    //   cuiNegativeStake: {
    //     getCoin: 0
    //   }
    // };
    // if (nextUpdate && nextUpdate.cuiSoftwareVersion && nextUpdate.cuiSoftwareVersion.svNumber) {
    //   return { version: nextUpdate.cuiSoftwareVersion.svNumber };
    // } else if (nextUpdate) {
    //   return { version: null };
    // }
    // return null;
  }

  async postponeUpdate(): PostponeUpdateResponse {
    Logger.debug('AdaApi::postponeUpdate called');
    try {
      const response: Promise<any> = await postponeAdaUpdate({ ca });
      Logger.debug('AdaApi::postponeUpdate success: ' + stringifyData(response));
    } catch (error) {
      Logger.error('AdaApi::postponeUpdate error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async applyUpdate(): ApplyUpdateResponse {
    Logger.debug('AdaApi::applyUpdate called');
    try {
      const response: Promise<any> = await applyAdaUpdate({ ca });
      Logger.debug('AdaApi::applyUpdate success: ' + stringifyData(response));
      ipcRenderer.send('kill-process');
    } catch (error) {
      Logger.error('AdaApi::applyUpdate error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  getSyncProgress = async (): Promise<GetSyncProgressResponse> => {
    Logger.debug('AdaApi::syncProgress called');
    try {
      const response: NodeInfo = await getNodeInfo({ ca });
      Logger.debug('AdaApi::syncProgress success: ' + stringifyData(response));
      const { localBlockchainHeight, blockchainHeight, syncProgress } = response;
      return {
        localBlockchainHeight: localBlockchainHeight.quantity,
        blockchainHeight: blockchainHeight.quantity,
        syncProgress: syncProgress.quantity
      };
    } catch (error) {
      Logger.debug('AdaApi::syncProgress error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  async updateWallet(request: UpdateWalletRequest): Promise<UpdateWalletResponse> {
    Logger.debug('AdaApi::updateWallet called: ' + stringifyData(request));
    const { walletId, assuranceLevel, name } = request;
    try {
      const wallet: AdaV1Wallet = await updateAdaWallet({ ca, walletId, assuranceLevel, name });
      Logger.debug('AdaApi::updateWallet success: ' + stringifyData(wallet));
      return _createWalletFromServerV1Data(wallet);
    } catch (error) {
      Logger.error('AdaApi::updateWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async updateWalletPassword(
    request: UpdateWalletPasswordRequest
  ): Promise<UpdateWalletPasswordResponse> {
    Logger.debug('AdaApi::updateWalletPassword called');
    const { walletId, oldPassword, newPassword } = request;
    try {
      await changeAdaWalletPassphrase({ ca, walletId, oldPassword, newPassword });
      Logger.debug('AdaApi::updateWalletPassword success');
      return true;
    } catch (error) {
      Logger.debug('AdaApi::updateWalletPassword error: ' + stringifyError(error));
      if (error.message.includes('Invalid old passphrase given')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new GenericApiError();
    }
  }

  async exportWalletToFile(
    request: ExportWalletToFileRequest
  ): Promise<ExportWalletToFileResponse> {
    const { walletId, filePath } = request;
    Logger.debug('AdaApi::exportWalletToFile called');
    try {
      const response: Promise<[]> = await exportAdaBackupJSON({ ca, walletId, filePath });
      Logger.debug('AdaApi::exportWalletToFile success: ' + stringifyData(response));
      return response;
    } catch (error) {
      Logger.error('AdaApi::exportWalletToFile error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async testReset(): Promise<void> {
    Logger.debug('AdaApi::testReset called');
    try {
      const response: Promise<void> = await adaTestReset({ ca });
      Logger.debug('AdaApi::testReset success: ' + stringifyData(response));
      return response;
    } catch (error) {
      Logger.error('AdaApi::testReset error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async getLocalTimeDifference(): Promise<GetLocalTimeDifferenceResponse> {
    Logger.debug('AdaApi::getLocalTimeDifference called');
    try {
      const response: NodeInfo = await getNodeInfo({ ca });
      Logger.debug('AdaApi::getLocalTimeDifference success: ' + stringifyData(response));

      const { localTimeInformation: { differenceFromNtpServer } } = response;
      const timeDifference = differenceFromNtpServer.quantity;
      return timeDifference;
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
      passwordUpdateDate: unixTimestampToDate(data.spendingPasswordLastUpdate),
    })
  )
);

const _createWalletFromServerDataV0 = action(
  'AdaApi::_createWalletFromServerData', (data: AdaWalletV0) => (
    new Wallet({
      id: data.cwId,
      amount: new BigNumber(data.cwAmount.getCCoin).dividedBy(LOVELACES_PER_ADA),
      name: data.cwMeta.cwName,
      assurance: data.cwMeta.cwAssurance,
      hasPassword: data.cwHasPassphrase,
      passwordUpdateDate: unixTimestampToDate(data.cwPassphraseLU),
    })
  )
);

const _createAddressFromServerData = action(
  'AdaApi::_createAddressFromServerData',
  (address: AdaAddress) => new WalletAddress(address)
);

const _conditionToTxState = (condition: string) => {
  switch (condition) {
    case 'CPtxApplying': return 'pending';
    case 'CPtxWontApply': return 'failed';
    default: return 'ok'; // CPtxInBlocks && CPtxNotTracked
  }
};

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

const _createTransactionFromServerData = action(
  'AdaApi::_createTransactionFromServerData', (data: AdaTransaction) => {
    const coins = data.ctAmount.getCCoin;
    const { ctmTitle, ctmDescription, ctmDate } = data.ctMeta;
    return new WalletTransaction({
      id: data.ctId,
      title: ctmTitle || data.ctIsOutgoing ? 'Ada sent' : 'Ada received',
      type: data.ctIsOutgoing ? transactionTypes.EXPEND : transactionTypes.INCOME,
      amount: new BigNumber(data.ctIsOutgoing ? -1 * coins : coins).dividedBy(LOVELACES_PER_ADA),
      date: unixTimestampToDate(ctmDate),
      description: ctmDescription || '',
      numberOfConfirmations: data.ctConfirmations,
      addresses: {
        from: data.ctInputs.map(address => address[0]),
        to: data.ctOutputs.map(address => address[0]),
      },
      state: _conditionToTxState(data.ctCondition),
    });
  }
);

const _createTransactionFromServerDataV1 = action(
  'AdaApi::_createTransactionFromServerData', (data: AdaTransactionV1) => {
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
  'AdaApi::_createWalletFromServerV1Data', (data: AdaV1Wallet) => {
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
