// @flow
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

export type WalletUtxos = {
  allStakes: number,
  boundType: string,
  histogram: {
    '10': number,
    '100': number,
    '1000': number,
    '10000': number,
    '100000': number,
    '1000000': number,
    '10000000': number,
    '100000000': number,
    '1000000000': number,
    '10000000000': number,
    '100000000000': number,
    '1000000000000': number,
    '10000000000000': number,
    '100000000000000': number,
    '1000000000000000': number,
    '10000000000000000': number,
    '45000000000000000': number,
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
