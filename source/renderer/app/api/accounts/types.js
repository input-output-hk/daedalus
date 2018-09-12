export type AdaAccount = {
  amount: number,
  addresses: AdaAddresses,
  name: string,
  walletId: string,
  index: number
};

export type AdaAccounts = Array<AdaAccount>;
