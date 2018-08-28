// @flow

// ========= Response Types =========
export type AdaAssurance = 'CWANormal' | 'CWAStrict';
export type AdaAssuranceV1 = 'normal' | 'strict';
export type AdaTransactionCondition = 'CPtxApplying' | 'CPtxInBlocks' | 'CPtxWontApply' | 'CPtxNotTracked';
export type AdaWalletRecoveryPhraseResponse = Array<string>;
export type AdaWalletCertificateAdditionalMnemonicsResponse = Array<string>;
export type AdaWalletCertificateRecoveryPhraseResponse = Array<string>;
export type AdaWalletRecoveryPhraseFromCertificateResponse = Array<string>;
export type GetWalletCertificateAdditionalMnemonicsResponse = Array<string>;
export type GetWalletCertificateRecoveryPhraseResponse = Array<string>;
export type GetWalletRecoveryPhraseFromCertificateResponse = Array<string>;

export type AdaSyncProgressResponse = {
  _spLocalCD: {
    getChainDifficulty: {
      getBlockCount: number,
    }
  },
  _spNetworkCD: {
    getChainDifficulty: {
      getBlockCount: number,
    }
  },
  _spPeers: number,
};

export type AdaWalletInitData = {
  operation: 'create' | 'restore',
  backupPhrase: [string],
  assuranceLevel: AdaAssuranceV1,
  name: string,
  spendingPassword: ?string,
};

export type AdaAmount = {
  getCCoin: number,
};

export type AdaTransactionTag = 'CTIn' | 'CTOut';

export type AdaAddress = {
  id: string,
  used: boolean,
  changeAddress: boolean
};

export type AdaAddresses = Array<AdaAddress>;

export type AdaAccount = {
  amount: number,
  addresses: AdaAddresses,
  name: string,
  walletId: string,
  index: number
};

export type AdaAccounts = Array<AdaAccount>;

export type AdaTransaction = {
  ctAmount: AdaAmount,
  ctConfirmations: number,
  ctId: string,
  ctInputs: AdaTransactionInputOutput,
  ctIsOutgoing: boolean,
  ctMeta: {
    ctmDate: Date,
    ctmDescription: ?string,
    ctmTitle: ?string,
  },
  ctOutputs: AdaTransactionInputOutput,
  ctCondition: AdaTransactionCondition,
};

export type AdaTransactions = [
  Array<AdaTransaction>,
  number,
];

export type AdaTransactionInputOutput = [
  [string, AdaAmount],
];

export type AdaWallet = {
  createdAt: Date,
  syncState: AdaV1WalletSyncState,
  balance: number,
  hasSpendingPassword: boolean,
  assuranceLevel: AdaAssuranceV1,
  name: string,
  id: string,
  spendingPasswordLastUpdate: Date,
};

export type AdaWalletV0 = {
  cwAccountsNumber: number,
  cwAmount: AdaAmount,
  cwHasPassphrase: boolean,
  cwId: string,
  cwMeta: {
    cwAssurance: AdaAssurance,
    cwName: string,
    csUnit: number,
  },
  cwPassphraseLU: Date,
};


export type AdaWallets = Array<AdaWallet>;

export type AdaLocalTimeDifference = number;


// ========== V1 API =========

export type AdaV1Assurance = 'normal' | 'strict';
export type AdaV1WalletSyncStateTag = 'restoring' | 'synced';

export type AdaV1WalletSyncState = {
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
  tag: AdaV1WalletSyncStateTag,
};

export type AdaV1Wallet = {
  assuranceLevel: AdaV1Assurance,
  balance: number,
  createdAt: string,
  hasSpendingPassword: boolean,
  id: string,
  name: string,
  spendingPasswordLastUpdate: string,
  syncState: AdaV1WalletSyncState,
};

export type AdaV1Wallets = Array<AdaV1Wallet>;

export const AdaV1AssuranceOptions: {
  NORMAL: AdaV1Assurance, STRICT: AdaV1Assurance,
} = {
  NORMAL: 'normal', STRICT: 'strict',
};

export type AdaAccountV1 = {
  data: [
    {
      amount: number,
      addresses: [
        {
          used: boolean,
          changeAddress: boolean,
          id: string
        }
      ],
      name: string,
      walletId: string,
      index: number
    }
  ],
  status: string,
  meta: {
    pagination: {
      totalPages: number,
      page: number,
      perPage: number,
      totalEntries: number,
    }
  }
};
export type AdaTransactionsV1 = Array<AdaTransactionV1>;
export type AdaTransactionV1 = {
  amount: number,
  confirmations: number,
  creationTime: string,
  direction: 'outgoing' | 'incoming',
  id: string,
  type: 'local' | 'foreign',
  inputs: AdaTransactionInputOutputV1,
  outputs: AdaTransactionInputOutputV1,
  status: {
    tag: 'applying' | 'inNewestBlocks' | 'persisted' | 'wontApply' | 'creating',
    data: {},
  },
};

export type AdaTransactionInputOutputV1 = [
  {
    address: string,
    amount: number,
  },
];

export type AdaTransactionFee = {
  estimatedAmount: number,
  status: "success",
  meta: {
    pagination: {}
  }
};

export type AdaTransactionParams = {
  ca: string,
  data: {
    source: {
      accountIndex: number,
      walletId: string,
    },
    destinations: [
      {
        address: string,
        amount: number,
      },
    ],
    groupingPolicy: ?'OptimizeForSecurity' | 'OptimizeForSize',
    spendingPassword: ?string
  },
};

export type AdaTxFeeParams = AdaTransactionParams;

export type Pagination = {
  pagination: {
    totalPages: number,
    page: number,
    perPage: number,
    totalEntries: number
  }
};

export type ResponseStatus = 'success' | 'fail' | 'error';

export type ResponseBaseV1 = {
  status: ResponseStatus,
  meta: Pagination
};
