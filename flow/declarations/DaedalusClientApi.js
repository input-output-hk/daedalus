declare module 'daedalus-client-api' {

  // ========= Response Types =========

  declare type ApiAssurance = 'CWANormal' | 'CWAStrict';
  declare type ApiCurrency = 'ADA';
  declare type ApiWalletType = 'CWTPersonal';
  declare type ApiAmount = {
    getCCoin: number,
  };
  declare type ApiTransactionTag = 'CTIn' | 'CTOut';

  declare type ApiAddress = {
    cadAmount: ApiAmount,
    cadId: string,
  };

  declare type ApiAddresses = Array<ApiAddress>;

  declare type ApiAccount = {
    caAccount: ApiAddresses,
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
    cwPassphraseLU: Date,
    cwMeta: {
      cwAssurance: ApiAssurance,
      cwName: string,
      csUnit: number,
    },
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
  declare function searchHistory(senderAccountId: string, searchTerm: string, skip: number, limit: number): Promise<ApiTransactions>;
  declare function newPaymentExtended(senderAccountId: string, receiverAddress: string, amount: string, title: string, description: ?string, password: ?string): Promise<ApiTransaction>;

  // Ada Redemption
  declare function redeemAda(redemptionCode: string, walletId: string): Promise<ApiTransaction>;
  declare function redeemAdaPaperVend(shieldedRedemptionKey: string, mnemonics: string, walletId: string): Promise<ApiTransaction>;

  // Wallet
  declare function getWallets(): ApiWallets;
  declare function getWalletAccounts(walletId: string): Promise<ApiAccounts>;
  declare function newWallet(walletName: string, assurance: string, unit: number, walletMnemonic: string, walletPassword: ?string): Promise<ApiWallet>;
  declare function deleteWallet(walletId: string): Promise<{}>;
  declare function restoreWallet(walletName: string, assurance: string, unit: number, walletMnemonic: string, walletPassword: ?string): Promise<ApiWallet>;
  declare function updateWallet(walletId: string, walletType: string, walletCurrency: string, walletName: string, assurance: string, unit: number): Promise<ApiWallet>;
  declare function importWallet(filePath: string): Promise<ApiWallet>;
  declare function newWAddress(accountId: string, walletPassword: ?string): Promise<ApiAddress>;

  // Test
  declare function testReset(): void;

}
