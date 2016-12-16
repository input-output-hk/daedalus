// @flow

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
  login(request: loginRequest): Promise<boolean>
  getUser(): Promise<userStruct>,
  getWallets(accountId: string): Promise<[walletStruct]>,
  getTransactions(request: getTransactionsRequest): Promise<{
    transactions: [transactionStruct],
    total: number
  }>,
  createUser(request: createUserRequest): Promise<userStruct>,
  createWallet(request: createWalletRequest): Promise<walletStruct>,
  createTransaction(request: createTransactionRequest): Promise<transactionStruct>,
  updateProfileField(request: updateUserProfileFieldRequest): Promise<any>,
}
