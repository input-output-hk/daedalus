// @flow
import BigNumber from 'bignumber.js';

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
  createdAt: Date,
};

export type LegacyAdaWallet = {
  id: string,
  balance: {
    available: WalletBalance,
    total: WalletBalance,
  },
  name: string,
  passphrase?: {
    last_updated_at: string,
  },
  state: WalletSyncState,
  tip: {
    slot_number: number,
    epoch_number: number,
    height: {
      quantity: number,
      unit: 'block',
    },
  },
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

export type WalletUtxoTotal = {
  quantity: number,
  unit: 'lovelace',
};

export type WalletUtxos = {
  total: WalletUtxoTotal,
  scale: 'log10',
  distribution: {
    [string]: number,
  },
};

export type WalletInitData = {
  name: string,
  mnemonic_sentence: [string], // [ 15 .. 24 ] words
  mnemonic_second_factor?: [string], // [ 9 .. 12 ] words
  passphrase: string,
  address_pool_gap?: number, // 20
};

export type LegacyWalletInitData = {
  name: string,
  mnemonic_sentence: [string], // [ 12 ] words
  passphrase: string,
};

export type WalletIdAndBalance = {
  walletId: string,
  balance: ?BigNumber,
};

// req/res Wallet types
export type CreateWalletRequest = {
  name: string,
  mnemonic: [string],
  mnemonicPassphrase?: [string],
  spendingPassword: string,
  addressPoolGap?: number,
};

export type UpdateSpendingPasswordRequest = {
  walletId: string,
  oldPassword: string,
  newPassword: string,
};

export type DeleteWalletRequest = {
  walletId: string,
};

export type GetWalletUtxosRequest = {
  walletId: string,
};

export type GetWalletIdAndBalanceRequest = {
  recoveryPhrase: Array<string>,
  getBalance: boolean,
};

export type GetWalletIdAndBalanceResponse = {
  walletId: string,
  balance: ?number,
};

export type RestoreWalletRequest = {
  recoveryPhrase: string,
  walletName: string,
  spendingPassword: string,
};

export type RestoreLegacyWalletRequest = {
  recoveryPhrase: string,
  walletName: string,
  spendingPassword: string,
};

export type UpdateWalletRequest = {
  walletId: string,
  name: string,
};
export type ImportWalletFromKeyRequest = {
  filePath: string,
  spendingPassword: string,
};

export type ImportWalletFromFileRequest = {
  filePath: string,
  spendingPassword: string,
  walletName: ?string,
};

export type ExportWalletToFileRequest = {
  walletId: string,
  filePath: string,
  password: string,
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
