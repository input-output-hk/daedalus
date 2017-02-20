// @flow
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';

// STRUCTS

export type walletStruct = {
  id: string,
  userId: string,
  address: string,
  type: string,
  currency: string,
  amount: number,
};

export type transactionStruct = {
  id: string,
  walletId: string,
  type: string,
  title: string,
  amount: number,
  currency: string,
  date: Date,
  description: string,
  exchange: ?string,
  conversionRate: ?string,
};

export type walletRecoveryPhraseStruct = {
  walletId: string,
  recoveryPhrase: [string]
};

// REQUESTS

export type getTransactionsRequest = {
  walletId: string,
  searchTerm: string,
  skip: number,
  limit: number
};

export type createWalletRequest = {
  name: string,
  currency: string,
  mnemonic: string,
};

export type createTransactionRequest = {
  walletId: string,
  sender: string,
  receiver: string,
  amount: number,
  currency: string,
  title: string,
  description: ?string,
};

export type getWalletRecoveryPhraseRequest = {
  walletId: string
};

export type walletRestoreRequest = {
  recoveryPhrase: string
};

export type redeemAdaRequest = {
  redemptionCode: string,
  walletId: string,
};

export type importKeyRequest = {
  filePath: string,
};

// INTERFACE

export type Api = {
  getWallets(): Promise<[Wallet]>,
  getTransactions(request: getTransactionsRequest): Promise<{
    transactions: [WalletTransaction],
    total: number
  }>,
  createWallet(request: createWalletRequest): Promise<Wallet>,
  createTransaction(request: createTransactionRequest): Promise<WalletTransaction>,
  isValidAddress(currency: string, address: string): Promise<boolean>,
  getTermsOfUse(): Promise<string>,
  getWalletRecoveryPhrase(request: getWalletRecoveryPhraseRequest): Promise<string>,
  restoreWallet(request: walletRestoreRequest) : Promise<any>
};
