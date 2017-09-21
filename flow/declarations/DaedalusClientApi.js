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

  declare type ApiTransactionFee = ApiAmount;

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

  declare type TlsConfig = Object;


  // ========= Functions =========

  // Common
  declare function generateMnemonic(): string;
  declare function tlsInit(ca: string): Object;

  // Status
  declare function notify(tls: TlsConfig, onSuccess: Function, onError?: Function): void;
  declare function nextUpdate(tls: TlsConfig): any;
  declare function postponeUpdate(tls: TlsConfig): any;
  declare function applyUpdate(tls: TlsConfig): any;
  declare function syncProgress(tls: TlsConfig): any;

  // Validators
  declare function isValidAddress(tls: TlsConfig, address: string): Promise<boolean>;
  declare function isValidMnemonic(length: number, mnemonic: string): Promise<boolean>;
  declare function isValidRedemptionKey(mnemonic: string): Promise<boolean>;
  declare function isValidPaperVendRedemptionKey(mnemonic: string): Promise<boolean>;

  // Accounts
  declare function getAccounts(tls: TlsConfig): Promise<ApiAccounts>;
  declare function newAccount(tls: TlsConfig, walletId: string, walletName: string, walletPassword: ?string): Promise<ApiAccount>;

  // Transactions
  // * getHistory has first three parameters optional
  //    - `getHistory(wId, null, null, 1, 10)` if you want to search with wallet
  //    - `getHistory(null, accId, null, 1, 10)` if you want to search with account
  //    - `getHistory(null, accId, addressId, 1, 10)` if you want to search specific address
  //    - `getHistory(wId, null, addressId, 1, 10)` if you want to search specific address
  //    - `getHistory` - any other combination will throw an error
  // * `getHistoryByWallet(wId, 1, 10)` - search only by wallet
  // * `getHistoryByAccount(acId, 1, 10)` - search only by account
  // * `getAddressHistory(acId, addressId, 1, 10)` - search  by account and address within
  declare function getHistory(tls: TlsConfig, walletId: ?string, accountId: ?string, addressId: ?string, skip: number, limit: number): Promise<ApiTransactions>;
  declare function getHistoryByWallet(tls: TlsConfig, walletId: string, skip: number, limit: number): Promise<ApiTransactions>;
  declare function getHistoryByAccount(tls: TlsConfig, accountId: string, skip: number, limit: number): Promise<ApiTransactions>;
  declare function getAddressHistory(tls: TlsConfig, accountId: string, addressId: string, skip: number, limit: number): Promise<ApiTransactions>;

  declare function newPayment(tls: TlsConfig, senderAccountId: string, receiverAddress: string, amount: string, password: ?string): Promise<ApiTransaction>;
  declare function txFee(tls: TlsConfig, senderAccountId: string, receiverAddress: string, amount: string): Promise<ApiTransactionFee>;

  // Ada Redemption
  declare function redeemAda(tls: TlsConfig, redemptionCode: string, accountId: string, walletPassword: ?string): Promise<ApiTransaction>;
  declare function redeemAdaPaperVend(tls: TlsConfig, shieldedRedemptionKey: string, mnemonics: string, accountId: string, walletPassword: ?string): Promise<ApiTransaction>;

  // Wallet
  declare function getWallets(tls: TlsConfig): ApiWallets;
  declare function getWalletAccounts(tls: TlsConfig, walletId: string): Promise<ApiAccounts>;
  declare function newWallet(tls: TlsConfig, walletName: string, assurance: string, unit: number, walletMnemonic: string, walletPassword: ?string): Promise<ApiWallet>;
  declare function deleteWallet(tls: TlsConfig, walletId: string): Promise<{}>;
  declare function restoreWallet(tls: TlsConfig, walletName: string, assurance: string, unit: number, walletMnemonic: string, walletPassword: ?string): Promise<ApiWallet>;
  declare function updateWallet(tls: TlsConfig, walletId: string, walletName: string, assurance: string, unit: number): Promise<ApiWallet>;
  declare function importWallet(tls: TlsConfig, filePath: string, walletPassword: ?string): Promise<ApiWallet>;
  declare function importBackupJSON(tls: TlsConfig, filePath: string, walletPassword: ?string, walletName: ?string): Promise<ApiWallet>;
  declare function newWAddress(tls: TlsConfig, accountId: string, walletPassword: ?string): Promise<ApiAddress>;
  declare function changeWalletPass(tls: TlsConfig, walletId: string, oldPassword: ?string, newPassword: ?string): Promise<{}>;
  declare function exportBackupJSON(tls: TlsConfig, walletId: string, filePath: string, walletPassword: ?string): Promise<string>;

  // Test
  declare function testReset(tls: TlsConfig): void;

}
