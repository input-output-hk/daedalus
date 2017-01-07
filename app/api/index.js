// @flow
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';
import User from '../domain/User';

// STRUCTS

export type userStruct = {
  id: string,
  profile: {
    name: string,
    email: string,
    phoneNumber: string,
    passwordHash: string,
    passwordUpdateDate: string,
    languageLocale: string
  }
};

export type walletStruct = {
  id: string,
  userId: string,
  address: string,
  type: string,
  currency: string,
  amount: number,
  name: string,
  lastUsed: ?bool,
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
  transactionId: ?string,
};

// REQUESTS

export type loginRequest = {
  email: string,
  passwordHash: string
}

export type getTransactionsRequest = {
  walletId: string,
  searchTerm: string,
  limit: number
}

export type createUserRequest = {
  name: string,
  email: string,
  phoneNumber: string,
  password: string,
  languageLocale: string
}

export type createWalletRequest = {
  accountId: string,
  name: string,
  currency: string
}

export type createTransactionRequest = {
  walletId: string,
  sender: string,
  receiver: string,
  amount: number,
  currency: string,
  description: ?string,
}

export type updateUserProfileFieldRequest = {
  field: string,
  value: string
}

// INTERFACE

export type Api = {
  login(request: loginRequest): Promise<boolean>,
  getUser(): Promise<User>,
  getWallets(accountId: string): Promise<[Wallet]>,
  getTransactions(request: getTransactionsRequest): Promise<{
    transactions: [WalletTransaction],
    total: number
  }>,
  createUser(request: createUserRequest): Promise<userStruct>,
  createWallet(request: createWalletRequest): Promise<Wallet>,
  createTransaction(request: createTransactionRequest): Promise<WalletTransaction>,
  updateProfileField(request: updateUserProfileFieldRequest): Promise<any>,
  isValidAddress(currency: string, address: string): Promise<boolean>,
  getTermsOfUse(): Promise<string>
}
