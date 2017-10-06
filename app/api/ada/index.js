import type {
  ApiAccounts,
  ApiAddress,
  ApiTransaction,
  ApiTransactionFee,
  ApiTransactions,
  ApiWallet,
  ApiWallets,
} from 'daedalus-client-api';
// @flow
import ClientApi from 'daedalus-client-api';
import {action} from 'mobx';
import {ipcRenderer, remote} from 'electron';
import {Logger, stringifyData, stringifyError} from '../../lib/logger';
import Wallet from '../../domain/Wallet';
import WalletTransaction from '../../domain/WalletTransaction';
import WalletAddress from '../../domain/WalletAddress';
import type { GetSyncProgressResponse } from "../common";
import { GenericApiError } from '../common';
import {
  AllFundsAlreadyAtReceiverAddressError,
  IncorrectWalletPasswordError,
  NotAllowedToSendMoneyToRedeemAddressError,
  NotAllowedToSendMoneyToSameAddressError,
  NotEnoughFundsForTransactionFeesError,
  NotEnoughMoneyToSendError,
  RedeemAdaError,
  WalletAlreadyImportedError,
  WalletAlreadyRestoredError,
  WalletFileImportError,
} from './errors';
import {LOVELACES_PER_ADA} from '../../config/numbersConfig';
import {getAdaSyncProgress} from './getAdaSyncProgress';
import environment from '../../environment';
import patchAdaApi from './mocks/patchAdaApi';
import BigNumber from "bignumber.js";
// import { makePayment } from './js-api/makePayment';

/**
 * The api layer that is used for all requests to the
 * cardano backend when working with the ADA coin.
 */

const ca = remote.getGlobal('ca');
const tlsConfig = ClientApi.tlsInit(ca);

export type GetWalletsResponse = Wallet[];
export type GetAddressesResponse = {
  accountId: ?string,
  addresses: WalletAddress[],
};
export type GetAddressesRequest = {
  walletId: string,
};
export type CreateAddressResponse = WalletAddress;
export type CreateAddressRequest = {
  accountId: string,
  password: ?string,
};
export type GetTransactionsRequest = {
  walletId: string,
  searchTerm: string,
  skip: number,
  limit: number,
};
export type GetTransactionsResponse = {
  transactions: WalletTransaction[],
  total: number,
};
export type CreateWalletRequest = {
  name: string,
  mnemonic: string,
  password: ?string,
};
export type CreateWalletResponse = Wallet;
export type DeleteWalletRequest = {
  walletId: string,
};
export type DeleteWalletResponse = boolean;
export type CreateTransactionRequest = {
  sender: string,
  receiver: string,
  amount: string,
  password: ?string,
};
export type CreateTransactionResponse = WalletTransaction;

export type GetWalletRecoveryPhraseResponse = string[];

export type RestoreWalletRequest = {
  recoveryPhrase: string,
  walletName: string,
  walletPassword: ?string,
};
export type RestoreWalletResponse = Wallet;
export type UpdateWalletRequest = {
  walletId: string,
  name: string,
  assurance: string,
};
export type UpdateWalletResponse = Wallet;
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
export type ImportWalletFromKeyResponse = Wallet;
export type ImportWalletFromFileRequest = {
  filePath: string,
  walletPassword: ?string,
  walletName: ?string,
};
export type ImportWalletFromFileResponse = Wallet;
export type NextUpdateResponse = {
  version: string,
};
export type PostponeUpdateResponse = void;
export type ApplyUpdateResponse = void;
export type UpdateWalletPasswordRequest = {
  walletId: string,
  oldPassword: ?string,
  newPassword: ?string,
};
export type UpdateWalletPasswordResponse = boolean;
export type TransactionFeeRequest = {
  sender: string,
  receiver: string,
  amount: string,
};
export type TransactionFeeResponse = BigNumber;
export type ExportWalletToFileRequest = {
  walletId: string,
  filePath: string,
  password: ?string
};
export type ExportWalletToFileResponse = {};
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
    Logger.debug('CardanoClientApi::getWallets called');
    try {
      const response: ApiWallets = await ClientApi.getWallets(tlsConfig);
      Logger.debug('CardanoClientApi::getWallets success: ' + stringifyData(response));
      const wallets = response.map(data => _createWalletFromServerData(data));
      return wallets;
    } catch (error) {
      Logger.error('CardanoClientApi::getWallets error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async getAddresses(request: GetAddressesRequest): Promise<GetAddressesResponse> {
    Logger.debug('CardanoClientApi::getAddresses called: ' + stringifyData(request));
    const { walletId } = request;
    try {
      const response: ApiAccounts = await ClientApi.getWalletAccounts(tlsConfig, walletId);
      Logger.debug('CardanoClientApi::getAddresses success: ' + stringifyData(response));

      if (!response.length) {
        return new Promise((resolve) => resolve({ accountId: null, addresses: [] }));
      }

      // For now only the first wallet account is used
      const firstAccount = response[0];
      const firstAccountId = firstAccount.caId;
      const firstAccountAddresses = firstAccount.caAddresses;

      return new Promise((resolve) => resolve({
        accountId: firstAccountId,
        addresses: firstAccountAddresses.map(data => _createAddressFromServerData(data)),
      }));
    } catch (error) {
      Logger.error('CardanoClientApi::getAddresses error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async getTransactions(request: GetTransactionsRequest): Promise<GetTransactionsResponse> {
    Logger.debug('CardanoClientApi::searchHistory called: ' + stringifyData(request));
    const { walletId, skip, limit } = request;
    try {
      const history: ApiTransactions = await ClientApi.getHistoryByWallet(
        tlsConfig, walletId, skip, limit
      );
      Logger.debug('CardanoClientApi::searchHistory success: ' + stringifyData(history));
      return new Promise((resolve) => resolve({
        transactions: history[0].map(data => _createTransactionFromServerData(data)),
        total: history[1]
      }));
    } catch (error) {
      Logger.error('CardanoClientApi::searchHistory error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async createWallet(request: CreateWalletRequest): Promise<CreateWalletResponse> {
    // wallets are created WITHOUT an account!!!
    // after creation ClientApi.newAccount API call should be triggered
    Logger.debug('CardanoClientApi::createWallet called');
    const { name, mnemonic, password } = request;
    const assurance = 'CWANormal';
    const unit = 0;
    try {
      const wallet: ApiWallet = await ClientApi.newWallet(
        tlsConfig, name, assurance, unit, mnemonic, password
      );
      Logger.debug('CardanoClientApi::createWallet success');
      return _createWalletFromServerData(wallet);
    } catch (error) {
      Logger.error('CardanoClientApi::createWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async deleteWallet(request: DeleteWalletRequest): Promise<DeleteWalletResponse> {
    Logger.debug('CardanoClientApi::deleteWallet called: ' + stringifyData(request));
    try {
      await ClientApi.deleteWallet(tlsConfig, request.walletId);
      Logger.debug('CardanoClientApi::deleteWallet success: ' + stringifyData(request));
      return true;
    } catch (error) {
      Logger.error('CardanoClientApi::deleteWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async createTransaction(request: CreateTransactionRequest): Promise<CreateTransactionResponse> {
    Logger.debug('CardanoClientApi::createTransaction called');
    const { sender, receiver, amount, password } = request;
    // sender must be set as accountId (account.caId) and not walletId
    try {
      const response: ApiTransaction = await ClientApi.newPayment(
        tlsConfig, sender, receiver, amount, password
      );
      // Passphrase handling is broken in js-api
      // const response = await makePayment(
      //   ca, { from: sender, to: receiver, amount }, { passphrase: password }
      // );
      Logger.debug('CardanoClientApi::createTransaction success: ' + stringifyData(response));
      return _createTransactionFromServerData(response);
    } catch (error) {
      Logger.error('CardanoClientApi::createTransaction error: ' + stringifyError(error));
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
    Logger.debug('CardanoClientApi::calculateTransactionFee called');
    const { sender, receiver, amount } = request;
    try {
      const response: ApiTransactionFee = await ClientApi.txFee(
        tlsConfig, sender, receiver, amount
      );
      Logger.debug('CardanoClientApi::calculateTransactionFee success: ' + stringifyData(response));
      return _createTransactionFeeFromServerData(response);
    } catch (error) {
      Logger.error('CardanoClientApi::calculateTransactionFee error: ' + stringifyError(error));
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
    Logger.debug('CardanoClientApi::createAddress called: ' + stringifyData(request));
    const { accountId, password } = request;
    try {
      const response: ApiAddress = await ClientApi.newWAddress(
        tlsConfig, accountId, password
      );
      Logger.debug('CardanoClientApi::createAddress success: ' + stringifyData(response));
      return _createAddressFromServerData(response);
    } catch (error) {
      Logger.error('CardanoClientApi::createAddress error: ' + stringifyError(error));
      if (error.message.includes('Passphrase doesn\'t match')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new GenericApiError();
    }
  }

  isValidAddress(address: string): Promise<boolean> {
    return ClientApi.isValidAddress(tlsConfig, address);
  }

  isValidMnemonic(mnemonic: string): Promise<boolean> {
    return ClientApi.isValidMnemonic(12, mnemonic);
  }

  isValidRedemptionKey(mnemonic: string): Promise<boolean> {
    return ClientApi.isValidRedemptionKey(mnemonic);
  }

  isValidPaperVendRedemptionKey(mnemonic: string): Promise<boolean> {
    return ClientApi.isValidPaperVendRedemptionKey(mnemonic);
  }

  isValidRedemptionMnemonic(mnemonic: string): Promise<boolean> {
    return ClientApi.isValidMnemonic(9, mnemonic);
  }

  getWalletRecoveryPhrase(): Promise<GetWalletRecoveryPhraseResponse> {
    Logger.debug('CardanoClientApi::getWalletRecoveryPhrase called');
    try {
      const response = new Promise((resolve) => resolve(ClientApi.generateMnemonic().split(' ')));
      Logger.debug('CardanoClientApi::getWalletRecoveryPhrase success');
      return response;
    } catch (error) {
      Logger.error('CardanoClientApi::getWalletRecoveryPhrase error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async restoreWallet(request: RestoreWalletRequest): Promise<RestoreWalletResponse> {
    Logger.debug('CardanoClientApi::restoreWallet called');
    const { recoveryPhrase, walletName, walletPassword } = request;
    const assurance = 'CWANormal';
    const unit = 0;
    try {
      const wallet: ApiWallet = await ClientApi.restoreWallet(
        tlsConfig, walletName, assurance, unit, recoveryPhrase, walletPassword
      );
      Logger.debug('CardanoClientApi::restoreWallet success');
      return _createWalletFromServerData(wallet);
    } catch (error) {
      Logger.error('CardanoClientApi::restoreWallet error: ' + stringifyError(error));
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

  async importWalletFromKey(request: ImportWalletFromKeyRequest): Promise<ImportWalletFromKeyResponse> {
    Logger.debug('CardanoClientApi::importWalletFromKey called');
    const { filePath, walletPassword } = request;
    try {
      const importedWallet: ApiWallet = await ClientApi.importWallet(
        tlsConfig, filePath, walletPassword
      );
      Logger.debug('CardanoClientApi::importWalletFromKey success');
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      Logger.error('CardanoClientApi::importWalletFromKey error: ' + stringifyError(error));
      if (error.message.includes('already exists')) {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  }

  async importWalletFromFile(request: ImportWalletFromFileRequest): Promise<ImportWalletFromFileResponse> {
    Logger.debug('CardanoClientApi::importWalletFromFile called');
    const { filePath, walletPassword, walletName } = request;
    const isKeyFile = filePath.split('.').pop().toLowerCase() === 'key';
    try {
      const importedWallet: ApiWallet = isKeyFile ? (
        await ClientApi.importWallet(
          tlsConfig, filePath, walletPassword
        )
      ) : (
        await ClientApi.importBackupJSON(
          tlsConfig, filePath, walletPassword, walletName
        )
      );
      Logger.debug('CardanoClientApi::importWalletFromFile success');
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      Logger.error('CardanoClientApi::importWalletFromFile error: ' + stringifyError(error));
      if (error.message.includes('already exists')) {
        throw new WalletAlreadyImportedError();
      }
      throw new WalletFileImportError();
    }
  }

  async redeemAda(request: RedeemAdaRequest): Promise<RedeemAdaResponse> {
    Logger.debug('CardanoClientApi::redeemAda called');
    const { redemptionCode, accountId, walletPassword } = request;
    try {
      const response: ApiTransaction = await ClientApi.redeemAda(
        tlsConfig, redemptionCode, accountId, walletPassword
      );
      Logger.debug('CardanoClientApi::redeemAda success');
      return _createTransactionFromServerData(response);
    } catch (error) {
      Logger.error('CardanoClientApi::redeemAda error: ' + stringifyError(error));
      if (error.message.includes('Passphrase doesn\'t match')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new RedeemAdaError();
    }
  }

  async redeemPaperVendedAda(request: RedeemPaperVendedAdaRequest): Promise<RedeemPaperVendedAdaResponse> {
    Logger.debug('CardanoClientApi::redeemAdaPaperVend called');
    const { shieldedRedemptionKey, mnemonics, accountId, walletPassword } = request;
    try {
      const response: ApiTransaction = await ClientApi.redeemAdaPaperVend(
        tlsConfig, shieldedRedemptionKey, mnemonics, accountId, walletPassword
      );
      Logger.debug('CardanoClientApi::redeemAdaPaperVend success');
      return _createTransactionFromServerData(response);
    } catch (error) {
      Logger.error('CardanoClientApi::redeemAdaPaperVend error: ' + stringifyError(error));
      if (error.message.includes('Passphrase doesn\'t match')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new RedeemAdaError();
    }
  }

  generateMnemonic(): string {
    Logger.debug('CardanoClientApi::generateMnemonic called');
    try {
      const response = ClientApi.generateMnemonic().split(' ');
      Logger.debug('CardanoClientApi::generateMnemonic success');
      return response;
    } catch (error) {
      Logger.error('CardanoClientApi::generateMnemonic error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async nextUpdate(): Promise<NextUpdateResponse> {
    Logger.debug('CardanoClientApi::nextUpdate called');
    let nextUpdate = null;
    try {
      // TODO: add flow type definitions for nextUpdate response
      const response = await ClientApi.nextUpdate(tlsConfig);
      Logger.debug('CardanoClientApi::nextUpdate success: ' + stringifyData(response));
      if (response) {
        nextUpdate = {
          version: response.cuiSoftwareVersion && response.cuiSoftwareVersion.svNumber || null
        };
      }
    } catch (error) {
      if (error.message.includes('No updates available')) {
        Logger.debug('CardanoClientApi::nextUpdate success: No updates available');
      } else {
        Logger.error('CardanoClientApi::nextUpdate error: ' + stringifyError(error));
      }
    }
    return nextUpdate;
    // TODO: remove hardcoded response after node update is tested
    // nextUpdate = {
    //   cuiSoftwareVersion: {
    //     svAppName: {
    //       getApplicationName: "cardano"
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
    Logger.debug('CardanoClientApi::postponeUpdate called');
    try {
      const response = await ClientApi.postponeUpdate(tlsConfig);
      Logger.debug('CardanoClientApi::postponeUpdate success: ' + stringifyData(response));
    } catch (error) {
      Logger.error('CardanoClientApi::postponeUpdate error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async applyUpdate(): ApplyUpdateResponse {
    Logger.debug('CardanoClientApi::applyUpdate called');
    try {
      const response = await ClientApi.applyUpdate(tlsConfig);
      Logger.debug('CardanoClientApi::applyUpdate success: ' + stringifyData(response));
      ipcRenderer.send('kill-process');
    } catch (error) {
      Logger.error('CardanoClientApi::applyUpdate error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  getSyncProgress = async (): Promise<GetSyncProgressResponse> => {
    Logger.debug('CardanoClientApi::syncProgress called');

    try {
      const response = await getAdaSyncProgress(ca);
      Logger.debug('CardanoClientApi::syncProgress success: ' + stringifyData(response));
      const localDifficulty = response._spLocalCD.getChainDifficulty.getBlockCount;
      // In some cases we dont get network difficulty & we need to wait for it from the notify API
      let networkDifficulty = null;
      if (response._spNetworkCD) {
        networkDifficulty = response._spNetworkCD.getChainDifficulty.getBlockCount;
      }
      return { localDifficulty, networkDifficulty };
    } catch (error) {
      Logger.error('CardanoClientApi::syncProgress error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  };

  async updateWallet(request: UpdateWalletRequest): Promise<UpdateWalletResponse> {
    Logger.debug('CardanoClientApi::updateWallet called: ' + stringifyData(request));
    const { walletId, name, assurance } = request;
    const unit = 0;
    try {
      const wallet: ApiWallet = await ClientApi.updateWallet(
        tlsConfig, walletId, name, assurance, unit
      );
      Logger.debug('CardanoClientApi::updateWallet success: ' + stringifyData(wallet));
      return _createWalletFromServerData(wallet);
    } catch (error) {
      Logger.error('CardanoClientApi::updateWallet error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async updateWalletPassword(request: UpdateWalletPasswordRequest): Promise<UpdateWalletPasswordResponse> {
    Logger.debug('CardanoClientApi::updateWalletPassword called');
    const { walletId, oldPassword, newPassword } = request;
    try {
      await ClientApi.changeWalletPass(tlsConfig, walletId, oldPassword, newPassword);
      Logger.debug('CardanoClientApi::updateWalletPassword success');
      return true;
    } catch (error) {
      Logger.error('CardanoClientApi::updateWalletPassword error: ' + stringifyError(error));
      if (error.message.includes('Invalid old passphrase given')) {
        throw new IncorrectWalletPasswordError();
      }
      throw new GenericApiError();
    }
  }

  async exportWalletToFile(request: ExportWalletToFileRequest): Promise<ExportWalletToFileResponse> {
    const { walletId, filePath, password } = request;
    Logger.debug('CardanoClientApi::exportWalletToFile called');
    try {
      const response = await ClientApi.exportBackupJSON(tlsConfig, walletId, filePath, password);
      Logger.debug('CardanoClientApi::exportWalletToFile success: ' + stringifyData(response));
      return response;
    } catch (error) {
      Logger.error('CardanoClientApi::exportWalletToFile error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

  async testReset(): void {
    Logger.debug('CardanoClientApi::testReset called');
    try {
      const response = await ClientApi.testReset(tlsConfig);
      Logger.debug('CardanoClientApi::testReset success: ' + stringifyData(response));
      return response;
    } catch (error) {
      Logger.error('CardanoClientApi::testReset error: ' + stringifyError(error));
      throw new GenericApiError();
    }
  }

}

// ========== TRANSFORM SERVER DATA INTO FRONTEND MODELS =========

const _createWalletFromServerData = action(
  'CardanoClientApi::_createWalletFromServerData', (data: ApiWallet) => (
    new Wallet({
      id: data.cwId,
      amount: new BigNumber(data.cwAmount.getCCoin).dividedBy(LOVELACES_PER_ADA),
      name: data.cwMeta.cwName,
      assurance: data.cwMeta.cwAssurance,
      hasPassword: data.cwHasPassphrase,
      passwordUpdateDate: new Date(data.cwPassphraseLU * 1000),
    })
  )
);

const _createAddressFromServerData = action(
  'CardanoClientApi::_createAddressFromServerData', (data: ApiAddress) => (
    new WalletAddress({
      id: data.cadId,
      amount: new BigNumber(data.cadAmount.getCCoin).dividedBy(LOVELACES_PER_ADA),
      isUsed: data.cadIsUsed,
    })
  )
);

const _createTransactionFromServerData = action(
  'CardanoClientApi::_createTransactionFromServerData', (data: ApiTransaction) => {
    const coins = data.ctAmount.getCCoin;
    const { ctmTitle, ctmDescription, ctmDate } = data.ctMeta;
    return new WalletTransaction({
      id: data.ctId,
      title: ctmTitle || data.ctIsOutgoing ? 'Ada sent' : 'Ada received',
      type: data.ctIsOutgoing ? 'adaExpend' : 'adaIncome',
      amount: new BigNumber(data.ctIsOutgoing ? -1 * coins : coins).dividedBy(LOVELACES_PER_ADA),
      date: new Date(ctmDate * 1000),
      description: ctmDescription || '',
      numberOfConfirmations: data.ctConfirmations,
      addresses: {
        from: data.ctInputs.map(address => address[0]),
        to: data.ctOutputs.map(address => address[0]),
      },
      condition: data.ctCondition,
    });
  }
);

const _createTransactionFeeFromServerData = action(
  'CardanoClientApi::_createTransactionFeeFromServerData', (data: ApiTransactionFee) => {
    const coins = data.getCCoin;
    return new BigNumber(coins).dividedBy(LOVELACES_PER_ADA);
  }
);
