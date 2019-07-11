// @flow
export type AdaWallet = {
  id: string,
  balance: {
    available: {
      quantity: number,
      unit: WalletUnit,
    },
    total: {
      quantity: number,
      unit: WalletUnit,
    },
  },
  name: string,
  state: {
    status: 'ready' | string,
  },
  delegation: {
    status: string,
    target: string,
  },
  passphrase: {
    last_updated_at: string,
  },
};

export type WalletUnit = 'lovelace' | 'ada';

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

export type Histogram = {
  [string]: number,
};

export type WalletUtxos = {
  allStakes: number,
  boundType: string,
  histogram: {
    [string]: number,
  },
};

// req/res Wallet types
export type CreateWalletRequest = {
  name: string,
  mnemonic: string,
  spendingPassword: ?string,
};

export type UpdateSpendingPasswordRequest = {
  walletId: string,
  oldPassword?: string,
  newPassword: ?string,
};

export type DeleteWalletRequest = {
  walletId: string,
};

export type GetWalletUtxosRequest = {
  walletId: string,
};

export type RestoreWalletRequest = {
  recoveryPhrase: string,
  walletName: string,
  spendingPassword?: ?string,
};

export type UpdateWalletRequest = {
  walletId: string,
  assuranceLevel: WalletAssuranceLevel,
  name: string,
};
export type ImportWalletFromKeyRequest = {
  filePath: string,
  spendingPassword: ?string,
};

export type ImportWalletFromFileRequest = {
  filePath: string,
  spendingPassword: ?string,
  walletName: ?string,
};

export type ExportWalletToFileRequest = {
  walletId: string,
  filePath: string,
  password: ?string,
};

export type GetWalletCertificateRecoveryPhraseRequest = {
  passphrase: string,
  input: string,
};

export type GetWalletRecoveryPhraseFromCertificateRequest = {
  passphrase: string,
  scrambledInput: string,
};
