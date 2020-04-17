// @flow
import BigNumber from 'bignumber.js';

import { WalletUnits } from '../../domains/Wallet';
import type { ExportedByronWallet } from '../../types/walletExportTypes';

export type Block = {
  slot_number: number,
  epoch_number: number,
  height: {
    quantity: number,
    unit: 'block',
  },
};

export type Input = {
  address?: string,
  amount?: {
    quantity: number,
    unit: WalletUnits.LOVELACE,
  },
  id: string,
  index: number,
};

export type Output = {
  address: string,
  amount: {
    quantity: number,
    unit: WalletUnits.LOVELACE,
  },
};

export type AdaWallet = {
  id: string,
  address_pool_gap: number,
  balance: {
    available: WalletBalance,
    total: WalletBalance,
    reward: WalletBalance,
  },
  delegation: {
    active: WalletDelegation,
    next?: WalletNextDelegation,
  },
  name: string,
  passphrase?: {
    last_updated_at: string,
  },
  state: WalletSyncState,
  discovery: Discovery,
  isLegacy: boolean,
};

export type LegacyAdaWallet = {
  id: string,
  balance: {
    available: WalletBalance,
    total: WalletBalance,
    reward: WalletBalance, // Unused prop - hack to keep flow happy
  },
  name: string,
  passphrase?: {
    last_updated_at: string,
  },
  state: WalletSyncState,
  discovery: Discovery,
  tip: Block,
};

export type LegacyAdaWallets = Array<LegacyAdaWallet>;

export type WalletUnit = WalletUnits.LOVELACE | WalletUnits.ADA;

export type AdaWallets = Array<AdaWallet>;

export type SyncStateStatus =
  | 'ready'
  | 'restoring'
  | 'syncing'
  | 'not_responding';

export type Discovery = 'random' | 'sequential';

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
  unit: WalletUnits.LOVELACE | WalletUnits.ADA,
};

export type DelegationStakePool = {
  active: WalletDelegation,
  next?: WalletNextDelegation,
};

export type WalletNextDelegationEpoch = {
  epoch_number: number,
  epoch_start_time: string,
};

export type WalletDelegation = {
  status: DelegationStatus,
  target?: string,
};

export type WalletPendingDelegations = Array<WalletNextDelegation>;

export type WalletNextDelegation = {
  status: DelegationStatus,
  target?: string,
  changes_at: WalletNextDelegationEpoch,
};

export type Histogram = {
  [string]: number,
};

export type WalletUtxoTotal = {
  quantity: number,
  unit: WalletUnits.LOVELACE,
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
  mnemonic_sentence: Array<string>, // [ 15 .. 24 ] words
  mnemonic_second_factor?: Array<string>, // [ 9 .. 12 ] words
  passphrase: string,
  address_pool_gap?: number, // 20
};

export type LegacyWalletInitData = {
  name: string,
  mnemonic_sentence: Array<string>, // [ 12 ] words
  passphrase: string,
};

export type WalletIdAndBalance = {
  walletId: string,
  balance: ?BigNumber,
};

// req/res Wallet types
export type CreateWalletRequest = {
  name: string,
  mnemonic: Array<string>,
  mnemonicPassphrase?: Array<string>,
  spendingPassword: string,
  addressPoolGap?: number,
};

export type UpdateSpendingPasswordRequest = {
  walletId: string,
  oldPassword: string,
  newPassword: string,
  isLegacy: boolean,
};

export type DeleteWalletRequest = {
  walletId: string,
  isLegacy: boolean,
};

export type GetWalletUtxosRequest = {
  walletId: string,
  isLegacy: boolean,
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
  recoveryPhrase: Array<string>,
  walletName: string,
  spendingPassword: string,
};

export type RestoreLegacyWalletRequest = {
  recoveryPhrase: Array<string>,
  walletName: string,
  spendingPassword: string,
};

export type RestoreExportedByronWalletRequest = ExportedByronWallet;

export type UpdateWalletRequest = {
  walletId: string,
  name: string,
  isLegacy: boolean,
};

export type ForceWalletResyncRequest = {
  walletId: string,
  isLegacy: boolean,
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
  isLegacy: boolean,
};

export type TransferFundsCalculateFeeRequest = {
  sourceWalletId: string,
};

export type TransferFundsCalculateFeeResponse = {
  migration_cost: {
    quantity: number,
    unit: WalletUnits.LOVELACE,
  },
};

export type TransferFundsRequest = {
  sourceWalletId: string,
  targetWalletId: string,
  passphrase: string,
};

export type TransferFundsResponse = {
  id: string,
  amount: {
    quantity: number,
    unit: WalletUnits.LOVELACE,
  },
  inserted_at?: {
    time: Date,
    block: Block,
  },
  pending_since?: {
    time: Date,
    block: Block,
  },
  depth: {
    quantity: number,
    unit: 'block',
  },
  direction: 'incoming' | 'outgoing',
  inputs: Array<Input>,
  outputs: Array<Output>,
  status: 'pending' | 'in_ledger',
};
