// @flow

// ========= General ==========

export type RequestConfig = {
  port: number,
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
};

export type ResponseBaseV1 = {
  status: ResponseStatus,
  meta: Pagination
};

export type ResponseStatus = 'success' | 'fail' | 'error';

export type Pagination = {
  pagination: {
    totalPages: number,
    page: number,
    perPage: number,
    totalEntries: number
  }
};

// ========= Responses =========
export type AdaWalletRecoveryPhraseResponse = Array<string>;
export type AdaWalletCertificateAdditionalMnemonicsResponse = Array<string>;
export type AdaWalletCertificateRecoveryPhraseResponse = Array<string>;
export type AdaWalletRecoveryPhraseFromCertificateResponse = Array<string>;
export type GetWalletCertificateAdditionalMnemonicsResponse = Array<string>;
export type GetWalletCertificateRecoveryPhraseResponse = Array<string>;
export type GetWalletRecoveryPhraseFromCertificateResponse = Array<string>;

// ========= Transactions  =========

export type AdaTransactions = Array<AdaTransaction>;
export type AdaTransaction = {
  amount: number,
  confirmations: number,
  creationTime: string,
  direction: 'outgoing' | 'incoming',
  id: string,
  type: 'local' | 'foreign',
  inputs: Array<PaymentDistribution>,
  outputs: Array<PaymentDistribution>,
  status: {
    tag: 'applying' | 'inNewestBlocks' | 'persisted' | 'wontApply' | 'creating',
    data: {},
  },
};

export type PaymentDistribution = {
  address: string,
  amount: number
};

export type TxnAssuranceLevel = 'low' | 'medium' | 'high';
export type TransactionState = 'pending' | 'failed' | 'ok';

export type AdaTransactionFee = {
  estimatedAmount: number,
  ...ResponseBaseV1
};

export type AdaTransactionParams = {
  data: {
    source: {
      accountIndex: number,
      walletId: string,
    },
    destinations: Array<PaymentDistribution>,
    groupingPolicy: ?'OptimizeForSecurity' | 'OptimizeForSize',
    spendingPassword: ?string
  },
};

export type AdaTxFeeParams = AdaTransactionParams;

// ========= Accounts  =========

export type AdaAccount = {
  amount: number,
  addresses: AdaAddresses,
  name: string,
  walletId: string,
  index: number
};

export type AdaAccounts = Array<AdaAccount>;

// ========= Addresses  =========

export type AdaAddress = {
  id: string,
  used: boolean,
  changeAddress: boolean
};

export type AdaAddresses = Array<AdaAddress>;

// ========= Wallets  =========

export type AdaWallet = {
  createdAt: string,
  syncState: WalletSyncState,
  balance: number,
  hasSpendingPassword: boolean,
  assuranceLevel: WalletAssuranceLevel,
  name: string,
  id: string,
  spendingPasswordLastUpdate: string,
};

export type AdaWallets = Array<AdaWallet>;

export type WalletAssuranceLevel = 'normal' | 'strict';

export type WalletAssuranceMode = { low: number, medium: number };

export type SyncStateTag = 'restoring' | 'synced';

export type WalletSyncState = {
  data: ?{
    estimatedCompletionTime: {
      quantity: number,
      unit: 'milliseconds',
    },
    percentage: {
      quantity: number,
      unit: 'percent',
    },
    throughput: {
      quantity: number,
      unit: 'blocksPerSecond',
    },
  },
  tag: SyncStateTag,
};

export type AdaWalletInitData = {
  operation: 'create' | 'restore',
  backupPhrase: [string],
  assuranceLevel: WalletAssuranceLevel,
  name: string,
  spendingPassword: ?string,
};

// ========= Node  =========

export type NodeInfo = {
  syncProgress: {
    quantity: number,
    unit: 'percent'
  },
  blockchainHeight: {
    quantity: number,
    unit: 'blocks'
  },
  localBlockchainHeight: {
    quantity: number,
    unit: 'blocks'
  },
  localTimeInformation: {
    differenceFromNtpServer: {
      quantity: number,
      unit: 'microseconds'
    }
  },
  subscriptionStatus: any
};

export type NodeSettings = {
  slotDuration: {
    quantity: number,
    unit: ?'milliseconds'
  },
  softwareInfo: NodeSoftware,
  projectVersion: string,
  gitRevision: string
};

export type NodeSoftware = {
  applicationName: string,
  version: number
};

// ========== V1 API =========
export type RedeemAdaParams = {
  redemptionCode: string,
  mnemonic: ?Array<string>,
  spendingPassword: string,
  walletId: string,
  accountIndex: number
};

export type RedeemPaperVendedAdaParams = {
  mnemonic: Array<string>,
  ...RedeemAdaParams
};
