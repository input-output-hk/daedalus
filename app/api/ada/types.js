// @flow

// ========= Response Types =========
export type ApiAssurance = 'CWANormal' | 'CWAStrict';
export type ApiTransactionCondition = 'CPtxApplying' | 'CPtxInBlocks' | 'CPtxWontApply' | 'CPtxNotTracked';
export type ApiWalletRecoveryPhraseResponse = Array<string>;

export type ApiSyncProgressResponse = {
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

export type ApiWalletInitData = {
  cwInitMeta: {
    cwName: string,
    cwAssurance: ApiAssurance,
    cwUnit: number,
  },
  cwBackupPhrase: {
    bpToList: [],
  }
};

export type ApiAmount = {
  getCCoin: number,
};
export type ApiTransactionTag = 'CTIn' | 'CTOut';

export type ApiAddress = {
  cadAmount: ApiAmount,
  cadId: string,
  cadIsUsed: boolean,
};

export type ApiAddresses = Array<ApiAddress>;

export type ApiAccount = {
  caAddresses: ApiAddresses,
  caAmount: ApiAmount,
  caId: string,
  caMeta: {
    caName: string,
  },
};

export type ApiAccounts = Array<ApiAccount>;

export type ApiTransaction = {
  ctAmount: ApiAmount,
  ctConfirmations: number,
  ctId: string,
  ctInputs: ApiTransactionInputOutput,
  ctIsOutgoing: boolean,
  ctMeta: {
    ctmDate: Date,
    ctmDescription: ?string,
    ctmTitle: ?string,
  },
  ctOutputs: ApiTransactionInputOutput,
  ctCondition: ApiTransactionCondition,
};

export type ApiTransactions = [
  Array<ApiTransaction>,
  number,
];

export type ApiTransactionInputOutput = [
  [string, ApiAmount],
];

export type ApiTransactionFee = ApiAmount;

export type ApiWallet = {
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

export type ApiWallets = Array<ApiWallet>;
