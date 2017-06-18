declare module 'daedalus-client-api' {

  // ========= Response Types =========

  declare type ApiAssurance = 'CWANormal' | 'CWAStrict';
  declare type ApiAmount = {
    getCCoin: number,
  };
  declare type ApiTransactionTag = 'CTIn' | 'CTOut';

  declare type ApiAddress = {
    cadAmount: ApiAmount,
    cadId: string,
    cadIsUsed: boolean,
  };

  declare type ApiAddresses = Array<ApiAddress>;

  declare type ApiAccount = {
    caAddresses: ApiAddresses,
    caAmount: ApiAmount,
    caId: string,
    caMeta: {
      caName: string,
    },
  };

  declare type ApiAccounts = Array<ApiAccount>;

  declare type ApiTransaction = {
    ctAmount: ApiAmount,
    ctConfirmations: number,
    ctId: string,
    ctInputAddrs: Array<string>,
    ctIsOutgoing: boolean,
    ctMeta: {
      ctmDate: Date,
      ctmDescription: ?string,
      ctmTitle: ?string,
    },
    ctOutputAddrs: Array<string>,
  };

  declare type ApiTransactions = [
    Array<ApiTransaction>,
    number,
  ];

  declare type ApiWallet = {
    cwAccountsNumber: number,
    cwAmount: ApiAmount,
    cwHasPassphrase: boolean,
    cwId: string,
    cwMeta: {
      cwAssurance: ApiAssurance,
      cwName: string,
      csUnit: number,
    },
    cwPassphraseLU: Date,
  };

  declare type ApiWallets = Array<ApiWallet>;


  // ========= Functions =========

  // Common
  declare function generateMnemonic(): string;

  // Status
  declare function notify(onSuccess: Function, onError?: Function): void;
  declare function nextUpdate(): any;
  declare function applyUpdate(): any;
  declare function syncProgress(): any;

  // Validators
  declare function isValidAddress(address: string): Promise<boolean>;
  declare function isValidMnemonic(length: number, mnemonic: string): Promise<boolean>;
  declare function isValidRedemptionKey(mnemonic: string): Promise<boolean>;
  declare function isValidPaperVendRedemptionKey(mnemonic: string): Promise<boolean>;

  // Accounts
  declare function getAccounts(): Promise<ApiAccounts>;
  declare function newAccount(walletId: string, walletName: string, walletPassword: ?string): Promise<ApiAccount>;

  // Transactions
  // * getHistory has first three parameters optional
  //    - `getHistory(wId, null, null, 1, 10)` if you want to search with wallet
  //    - `getHistory(null, accId, null, 1, 10)` if you want to search with account
  //    - `getHistory(null, accId, addressId, 1, 10)` if you want to search specific address
  //    - `getHistory(wId, null, addressId, 1, 10)` if you want to search specific address
  //    - `getHistory` - any other combination will throw an error
  // * `getHistoryByWallet(wId, 1, 10)` - search only by wallet
  // * `getHistoryByAccount(acId, 1, 10)` - search only by account
  // * `getHistoryByAddress(acId, addressId, 1, 10)` - search  by account and address within
  declare function getHistory(walletId: ?string, accountId: ?string, addressId: ?string, skip: number, limit: number): Promise<ApiTransactions>;
  declare function getHistoryByWallet(walletId: string, skip: number, limit: number): Promise<ApiTransactions>;
  declare function getHistoryByAccount(accountId: string, skip: number, limit: number): Promise<ApiTransactions>;
  // declare function getHistoryByAddress(accountId: string, addressId: string, skip: number, limit: number): Promise<ApiTransactions>;

  declare function newPayment(senderAccountId: string, receiverAddress: string, amount: string, password: ?string): Promise<ApiTransaction>;

  // Ada Redemption
  declare function redeemAda(redemptionCode: string, accountId: string, walletPassword: ?string): Promise<ApiTransaction>;
  declare function redeemAdaPaperVend(shieldedRedemptionKey: string, mnemonics: string, accountId: string, walletPassword: ?string): Promise<ApiTransaction>;

  // Wallet
  declare function getWallets(): ApiWallets;
  declare function getWalletAccounts(walletId: string): Promise<ApiAccounts>;
  declare function newWallet(walletName: string, assurance: string, unit: number, walletMnemonic: string, walletPassword: ?string): Promise<ApiWallet>;
  declare function deleteWallet(walletId: string): Promise<{}>;
  declare function restoreWallet(walletName: string, assurance: string, unit: number, walletMnemonic: string, walletPassword: ?string): Promise<ApiWallet>;
  declare function updateWallet(walletId: string, walletName: string, assurance: string, unit: number): Promise<ApiWallet>;
  declare function importWallet(filePath: string, walletPassword: ?string): Promise<ApiWallet>;
  declare function newWAddress(accountId: string, walletPassword: ?string): Promise<ApiAddress>;
  declare function changeWalletPass(walletId: string, oldPassword: ?string, newPassword: ?string): Promise<{}>;
  declare function renameWalletSet(walletId: string, walletName: string): Promise<ApiWallet>;

  // Test
  declare function testReset(): void;

}
