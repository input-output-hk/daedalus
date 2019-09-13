// @flow
import BigNumber from 'bignumber.js';

export type AdaWallet = {
  createdAt: Date,
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

export type WalletIdAndBalance = {
  walletId: string,
  balance: ?BigNumber,
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
