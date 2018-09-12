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

// I/O Wallet types
export type AdaWalletInitData = {
  operation: 'create' | 'restore',
  backupPhrase: [string],
  assuranceLevel: WalletAssuranceLevel,
  name: string,
  spendingPassword: ?string,
};

export type CreateWalletRequest = {
  name: string,
  mnemonic: string,
  spendingPassword: ?string,
};

export type UpdateWalletPasswordRequest = {
  walletId: string,
  oldPassword?: string,
  newPassword: ?string,
};

export type DeleteWalletRequest = {
  walletId: string,
};

export type RestoreWalletRequest = {
  recoveryPhrase: string,
  walletName: string,
  walletPassword: ?string,
};
