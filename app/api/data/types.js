export type accountType = {
  profile: {
    name: string,
    email: string,
    phoneNumber: string,
    passwordHash: string,
    passwordUpdateDate: string,
    languageLocale: string
  }
};

export type walletType = {
 address: string,
 type: string,
 currency: string,
 amount: number,
 name: string,
 lastUsed: ?bool,
};

export type transactionType = {
  id: string,
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
