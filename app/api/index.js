// @flow
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';

// REQUEST & RESPONSE TYPES

export type GetWalletsResponse = Wallet[];

export type GetTransactionsRequest = {
  walletId: string,
  searchTerm: string,
  skip: number,
  limit: number
};

export type GetTransactionsResponse = {
  transactions: WalletTransaction[],
  total: number
};

export type CreateWalletRequest = {
  name: string,
  currency: string,
  mnemonic: string,
  password: string,
};

export type CreateWalletResponse = Wallet;

export type DeleteWalletRequest = {
  walletId: string,
};

export type DeleteWalletResponse = boolean;

export type CreateTransactionRequest = {
  walletId: string,
  sender: string,
  receiver: string,
  amount: string,
  currency: string,
  title: string,
  description: ?string,
};

export type CreateTransactionResponse = WalletTransaction;

export type GetWalletRecoveryPhraseRequest = {
  walletId: string
};

export type GetWalletRecoveryPhraseResponse = string[];

export type RestoreWalletRequest = {
  recoveryPhrase: string,
  walletName: string,
};

export type RestoreWalletResponse = Wallet;

export type UpdateWalletRequest = {
  walletId: string,
  type: string,
  currency: string,
  name: string,
  assurance: string,
};

export type UpdateWalletResponse = boolean;

export type RedeemAdaRequest = {
  redemptionCode: string,
  walletId: string,
};

export type RedeemAdaResponse = Wallet;

export type RedeemPaperVendedAdaRequest = {
  shieldedRedemptionKey: string,
  mnemonics: string,
  walletId: string,
};

export type RedeemPaperVendedAdaResponse = RedeemPaperVendedAdaRequest;

export type ImportKeyRequest = {
  filePath: string,
};

export type ImportKeyResponse = Wallet;

export type GetSyncProgressResponse = {
  localDifficulty: number,
  networkDifficulty: number
};

export type NextUpdateResponse = {
  version: string,
};

export type ApplyUpdateResponse = void;

export type ChangeWalletPasswordRequest = {
  walletId: string,
  oldPassword: string,
  newPassword: string,
};

export type ChangeWalletPasswordResponse = ChangeWalletPasswordRequest;

export type SetWalletPasswordRequest = {
  walletId: string,
  password: string,
};

export type SetWalletPasswordResponse = SetWalletPasswordRequest;

// API INTERFACE

export type Api = {
  notify(onSuccess: Function, onError?: Function): void,
  reset(): void,
  getWallets(): Promise<GetWalletsResponse>,
  getTransactions(request: GetTransactionsRequest): Promise<GetTransactionsResponse>,
  createWallet(request: CreateWalletRequest): Promise<CreateWalletResponse>,
  deleteWallet(request: DeleteWalletRequest): Promise<DeleteWalletResponse>,
  createTransaction(request: CreateTransactionRequest): Promise<CreateTransactionResponse>,
  isValidAddress(currency: string, address: string): Promise<boolean>,
  isValidMnemonic(mnemonic: string): Promise<boolean>,
  isValidRedemptionKey(mnemonic: string): Promise<boolean>,
  isValidRedemptionMnemonic(mnemonic: string): Promise<boolean>,
  isValidPaperVendRedemptionKey(mnemonic: string): Promise<boolean>,
  getWalletRecoveryPhrase(request: GetWalletRecoveryPhraseRequest): Promise<GetWalletRecoveryPhraseResponse>,
  restoreWallet(request: RestoreWalletRequest): Promise<RestoreWalletResponse>,
  importWalletFromKey(request: ImportKeyRequest): Promise<ImportKeyResponse>,
  redeemAda(request: RedeemAdaRequest): Promise<RedeemAdaResponse>,
  redeemPaperVendedAda(request: RedeemPaperVendedAdaRequest): Promise<RedeemPaperVendedAdaResponse>,
  generateMnemonic(): string,
  nextUpdate(): Promise<NextUpdateResponse>,
  applyUpdate(): ApplyUpdateResponse,
  getSyncProgress(): Promise<GetSyncProgressResponse>,
  setUserLocale(locale: string): Promise<string>,
  getUserLocale(): Promise<string>,
  updateWallet(request: UpdateWalletRequest): Promise<UpdateWalletResponse>,
  testReset(): void,
  changeWalletPassword(request: ChangeWalletPasswordRequest): Promise<ChangeWalletPasswordResponse>,
  setWalletPassword(request: SetWalletPasswordRequest): Promise<SetWalletPasswordResponse>,
};
