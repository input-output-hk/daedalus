declare module 'daedalus-client-api' {

  // ========= Response Types =========

  declare type ApiAssurance = 'CWANormal' | 'CWAStrict';
  declare type ApiCurrency = 'ADA';
  declare type ApiWalletType = 'CWTPersonal';
  declare type ApiAmount = {
    getCoin: number,
  };
  declare type ApiTransactionTag = 'CTIn' | 'CTOut';

  declare type ApiAddress = {
    ctAmount: ApiAmount,
    ctConfirmations: number,
    ctId: string,
    ctType: {
      tag: ApiTransactionTag,
      contents: {
        ctmCurrency: ApiCurrency,
        ctmDate: Date,
        ctmDescription: ?string,
        ctmTitle: ?string,
      }
    },
  };

  declare type ApiTransaction = {
    ctAmount: ApiAmount,
    ctConfirmations: number,
    ctId: string,
    ctType: {
      tag: ApiTransactionTag,
      contents: {
        ctmCurrency: ApiCurrency,
        ctmDate: Date,
        ctmDescription: ?string,
        ctmTitle: ?string,
      }
    },
  };

  declare type ApiTransactions = [
    Array<ApiTransaction>,
    number,
  ];

  declare type ApiWallet = {
    cwId: string,
    cwAmount: ApiAmount,
    cwMeta: {
      cwAssurance: ApiAssurance,
      cwCurrency: ApiCurrency,
      cwName: string,
      cwType: ApiWalletType,
      cwUnit: number,
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
  declare function isValidAddress(currency: string, address: string): Promise<boolean>;
  declare function isValidMnemonic(length: number, mnemonic: string): Promise<boolean>;
  declare function isValidRedemptionKey(mnemonic: string): Promise<boolean>;
  declare function isValidPaperVendRedemptionKey(mnemonic: string): Promise<boolean>;

  // Transactions
  declare function searchHistory(walletId: string, searchTerm: string, skip: number, limit: number): Promise<ApiTransactions>;
  declare function sendExtended(sender: string, receiver: string, amount: string, currency: string, title: string, description: ?string, password: ?string): Promise<ApiTransaction>;

  // Ada Redemption
  declare function redeemAda(redemptionCode: string, walletId: string): Promise<ApiTransaction>;
  declare function redeemAdaPaperVend(shieldedRedemptionKey: string, mnemonics: string, walletId: string): Promise<ApiTransaction>;

  // Wallet
  declare function getWallets(): ApiWallets;
  declare function newWallet(walletType: string, walletCurrency: string, walletName: string, walletMnemonic: string, walletPassword: ?string): Promise<ApiWallet>;
  declare function deleteWallet(walletId: string): Promise<{}>;
  declare function restoreWallet(walletType: string, walletCurrency: string, walletName: string, walletMnemonic: string, walletPassword: ?string): Promise<ApiWallet>;
  declare function updateWallet(walletId: string, walletType: string, walletCurrency: string, walletName: string, assurance: string, unit: number): Promise<ApiWallet>;
  declare function importKey(filePath: string): Promise<ApiWallet>;

  // Test
  declare function testReset(): void;

}
