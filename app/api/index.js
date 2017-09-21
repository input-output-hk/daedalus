// @flow
import BigNumber from 'bignumber.js';
import Wallet from '../domain/Wallet';
import WalletAddress from '../domain/WalletAddress';
import WalletTransaction from '../domain/WalletTransaction';

// REQUEST & RESPONSE TYPES

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

export type GetWalletRecoveryPhraseRequest = {
  walletId: string,
};

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

export type GetSyncProgressResponse = {
  localDifficulty: number,
  networkDifficulty: number
};

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

// API INTERFACE
/* eslint-disable max-len */
export type Api = {
  notify(onSuccess: Function, onError?: Function): void,
  reset(): void,
  getWallets(): Promise<GetWalletsResponse>,
  getAddresses(): Promise<GetAddressesResponse>,
  getTransactions(request: GetTransactionsRequest): Promise<GetTransactionsResponse>,
  createWallet(request: CreateWalletRequest): Promise<CreateWalletResponse>,
  deleteWallet(request: DeleteWalletRequest): Promise<DeleteWalletResponse>,
  createTransaction(request: CreateTransactionRequest): Promise<CreateTransactionResponse>,
  createAddress(request: CreateAddressRequest): Promise<CreateAddressResponse>,
  isValidAddress(address: string): Promise<boolean>,
  isValidMnemonic(mnemonic: string): Promise<boolean>,
  isValidRedemptionKey(mnemonic: string): Promise<boolean>,
  isValidRedemptionMnemonic(mnemonic: string): Promise<boolean>,
  isValidPaperVendRedemptionKey(mnemonic: string): Promise<boolean>,
  getWalletRecoveryPhrase(request: GetWalletRecoveryPhraseRequest): Promise<GetWalletRecoveryPhraseResponse>,
  restoreWallet(request: RestoreWalletRequest): Promise<RestoreWalletResponse>,
  importWalletFromKey(request: ImportWalletFromKeyRequest): Promise<ImportWalletFromKeyResponse>,
  importWalletFromFile(request: ImportWalletFromFileRequest): Promise<ImportWalletFromFileResponse>,
  redeemAda(request: RedeemAdaRequest): Promise<RedeemAdaResponse>,
  redeemPaperVendedAda(request: RedeemPaperVendedAdaRequest): Promise<RedeemPaperVendedAdaResponse>,
  generateMnemonic(): string,
  nextUpdate(): Promise<NextUpdateResponse>,
  postponeUpdate(): PostponeUpdateResponse,
  applyUpdate(): ApplyUpdateResponse,
  getSyncProgress(): Promise<GetSyncProgressResponse>,
  setUserLocale(locale: string): Promise<string>,
  getUserLocale(): Promise<string>,
  setTermsOfUseAcceptance(): Promise<boolean>,
  getTermsOfUseAcceptance(): Promise<boolean>,
  setSendLogsChoice(sendLogs: boolean): Promise<boolean>,
  getSendLogsChoice(): Promise<boolean>,
  updateWallet(request: UpdateWalletRequest): Promise<UpdateWalletResponse>,
  updateWalletPassword(request: UpdateWalletPasswordRequest): Promise<UpdateWalletPasswordResponse>,
  calculateTransactionFee(request: TransactionFeeRequest): Promise<TransactionFeeResponse>,
  testReset(): void,
  exportWalletToFile(request: ExportWalletToFileRequest): Promise<ExportWalletToFileResponse>,
  setUserTheme(theme: string): Promise<string>,
  getUserTheme(): Promise<string>,
};
/* eslint-disable max-len */
