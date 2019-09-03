// @flow
export type AdaWallet = {
  id: string,
  address_pool_gap: number,
  balance: {
    available: WalletBalance,
    total: WalletBalance,
  },
  delegation: WalletDelegation,
  name: string,
  passphrase?: {
    last_updated_at: string,
  },
  state: WalletSyncState,
};

export type WalletUnit = 'lovelace' | 'ada';

export type AdaWallets = Array<AdaWallet>;

export type SyncStateStatus = 'ready' | 'restoring';

export type DelegationStatus = 'delegating' | 'not_delegating';

export type WalletSyncStateProgress = {
  quantity: number,
  unit: 'percentage',
};

export type WalletSyncState = {
  status: SyncStateStatus,
  progress?: WalletSyncStateProgress,
};

export type WalletBalance = {
  quantity: number,
  unit: 'lovelace' | 'ada',
};

export type WalletDelegation = {
  status: DelegationStatus,
  target?: string,
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

export type GetWalletRequest = {
  walletId: string,
};
