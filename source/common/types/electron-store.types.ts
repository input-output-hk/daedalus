export type SubmissionTransactionState =
  | 'pending'
  | 'in_ledger'
  | 'expired'
  | 'failed'
  | 'submission-unknown';
export type SubmissionTransactionRecord = Readonly<{
  transactionId: string;
  state: SubmissionTransactionState;
  createdAt: string;
  amount: string;
  fee: string;
  transferAmount?: string;
  isSelfTransfer?: boolean;
  amountIsKnown: boolean;
  hasCertificates: boolean;
  type: 'expend' | 'income';
  title: string;
  toAddress?: string;
  assets: readonly Readonly<{
    policyId: string;
    assetName: string;
    quantity: string;
  }>[];
  dismissed: boolean;
  notified: boolean;
}>;
export type SubmissionTransactionsData = Readonly<{
  version: 1;
  records: readonly SubmissionTransactionRecord[];
}>;

export type StorageType = 'get' | 'set' | 'delete' | 'reset';
export type StorageKey =
  | 'ALONZO-INFO-WAS-OPEN'
  | 'APP-AUTOMATIC-UPDATE-FAILED'
  | 'APP-UPDATE-COMPLETED'
  | 'ASSET-DATA'
  | 'ASSET-SETTINGS-DIALOG-WAS-OPENED'
  | 'CURRENCY-ACTIVE'
  | 'CURRENCY-SELECTED'
  | 'DATA-LAYER-MIGRATION-ACCEPTANCE'
  | 'DISCREET-MODE-ENABLED'
  | 'DOWNLOAD-MANAGER'
  | 'DREP-FAVORITES'
  | 'HARDWARE-WALLET-DEVICES'
  | 'LIST-VIEW-PREFERENCES'
  | 'HARDWARE-WALLETS'
  | 'SUBMISSION-TRANSACTIONS'
  | 'READ-NEWS'
  | 'RESET'
  | 'SMASH-SERVER'
  | 'STAKE-POOLS-LIST-VIEW-TOOLTIP'
  | 'TERMS-OF-USE-ACCEPTANCE'
  | 'THEME'
  | 'TOKEN-FAVORITES'
  | 'USER-DATE-FORMAT-ENGLISH'
  | 'USER-DATE-FORMAT-JAPANESE'
  | 'USER-LOCALE'
  | 'USER-NUMBER-FORMAT'
  | 'USER-TIME-FORMAT'
  | 'WALLET-MIGRATION-STATUS'
  | 'WALLETS'
  | 'ANALYTICS-ACCEPTANCE'
  | 'USER-ID'
  | 'WINDOW-BOUNDS'
  | 'CUSTOM-CHAIN-PATH';
export type StoreMessage = {
  type: StorageType;
  key: StorageKey;
  data?: any;
  id?: string;
};
