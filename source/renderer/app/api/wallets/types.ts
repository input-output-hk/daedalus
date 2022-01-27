import BigNumber from 'bignumber.js';
import { WalletUnits } from '../../domains/Wallet';
import type { ExportedByronWallet } from '../../types/walletExportTypes';
import type { Currency, LocalizedCurrency } from '../../types/currencyTypes';
import type { ApiTokens } from '../assets/types';

export type Block = {
  slot_number: number;
  epoch_number: number;
  height: {
    quantity: number;
    unit: 'block';
  };
};
export type Input = {
  address?: string;
  amount?: {
    quantity: number;
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
    unit: WalletUnits.LOVELACE;
  };
  id: string;
  index: number;
};
export type Output = {
  address: string;
  amount: {
    quantity: number;
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
    unit: WalletUnits.LOVELACE;
  };
};
export type AdaWallet = {
  id: string;
  address_pool_gap: number;
  balance: {
    available: WalletBalance;
    total: WalletBalance;
    reward: WalletBalance;
  };
  assets: {
    available: ApiTokens;
    total: ApiTokens;
  };
  delegation: {
    active: WalletDelegation;
    next?: WalletNextDelegation;
  };
  name: string;
  passphrase?: {
    last_updated_at: string;
  };
  state: WalletSyncState;
  discovery: Discovery;
  isLegacy: boolean;
  isHardwareWallet?: boolean;
};
export type LegacyAdaWallet = {
  id: string;
  balance: {
    available: WalletBalance;
    total: WalletBalance;
    reward: WalletBalance; // Unused prop - hack to keep flow happy
  };
  name: string;
  passphrase?: {
    last_updated_at: string;
  };
  state: WalletSyncState;
  discovery: Discovery;
  tip: Block;
};
export type LegacyAdaWallets = Array<LegacyAdaWallet>;
// @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
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
  quantity: number;
  unit: 'percentage';
};
export type WalletSyncState = {
  status: SyncStateStatus;
  progress?: WalletSyncStateProgress;
};
export type WalletBalance = {
  quantity: number;
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
  unit: WalletUnits.LOVELACE | WalletUnits.ADA;
};
export type DelegationStakePool = {
  active: WalletDelegation;
  next?: WalletNextDelegation;
};
export type WalletNextDelegationEpoch = {
  epoch_number: number | null | undefined;
  epoch_start_time: string | null | undefined;
};
export type WalletDelegation = {
  status: DelegationStatus;
  target?: string;
};
export type WalletPendingDelegations = Array<WalletNextDelegation>;
export type WalletNextDelegation = {
  status: DelegationStatus;
  target?: string;
  changes_at: WalletNextDelegationEpoch;
};
export type Histogram = Record<string, number>;
export type WalletUtxoTotal = {
  quantity: number;
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
  unit: WalletUnits.LOVELACE;
};
export type WalletUtxos = {
  total: WalletUtxoTotal;
  scale: 'log10';
  distribution: Record<string, number>;
};
export type WalletInitData = {
  name: string;
  mnemonic_sentence: Array<string>;
  // [ 15 .. 24 ] words
  mnemonic_second_factor?: Array<string>;
  // [ 9 .. 12 ] words
  passphrase: string;
  address_pool_gap?: number; // 20
};
export type LegacyWalletInitData = {
  name: string;
  mnemonic_sentence: Array<string>;
  // [ 12 ] words
  passphrase: string;
};
// req/res Wallet types
export type CreateWalletRequest = {
  name: string;
  mnemonic: Array<string>;
  mnemonicPassphrase?: Array<string>;
  spendingPassword: string;
  addressPoolGap?: number;
};
export type UpdateSpendingPasswordRequest = {
  walletId: string;
  oldPassword: string;
  newPassword: string;
  isLegacy: boolean;
};
export type DeleteWalletRequest = {
  walletId: string;
  isLegacy: boolean;
  isHardwareWallet?: boolean;
};
export type GetWalletUtxosRequest = {
  walletId: string;
  isLegacy: boolean;
};
export type RestoreWalletRequest = {
  recoveryPhrase: Array<string>;
  walletName: string;
  spendingPassword: string;
};
export type RestoreLegacyWalletRequest = {
  recoveryPhrase: Array<string>;
  walletName: string;
  spendingPassword: string;
};
export type RestoreExportedByronWalletRequest = ExportedByronWallet;
export type UpdateWalletRequest = {
  walletId: string;
  name: string;
  isLegacy: boolean;
  isHardwareWallet?: boolean;
};
export type ImportWalletFromKeyRequest = {
  filePath: string;
  spendingPassword: string;
};
export type ImportWalletFromFileRequest = {
  filePath: string;
  spendingPassword: string;
  walletName: string | null | undefined;
};
export type ExportWalletToFileRequest = {
  walletId: string;
  filePath: string;
  password: string;
};
export type GetWalletCertificateRecoveryPhraseRequest = {
  passphrase: string;
  input: string;
};
export type GetWalletRecoveryPhraseFromCertificateRequest = {
  passphrase: string;
  scrambledInput: string;
};
export type GetWalletRequest = {
  walletId: string;
  isLegacy: boolean;
};
export type GetWalletPublicKeyRequest = {
  walletId: string;
  role: string;
  index: string;
};
export type GetAccountPublicKeyRequest = {
  walletId: string;
  index: string;
  passphrase: string;
  extended: boolean;
};
export type TransferFundsCalculateFeeRequest = {
  sourceWalletId: string;
};
export type TransferFundsCalculateFeeApiResponse = {
  migration_cost: {
    quantity: number;
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
    unit: WalletUnits.LOVELACE;
  };
  leftovers: {
    quantity: number;
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
    unit: WalletUnits.LOVELACE;
  };
};
export type TransferFundsCalculateFeeResponse = {
  fee: BigNumber;
  leftovers: BigNumber;
};
export type TransferFundsRequest = {
  sourceWalletId: string;
  targetWalletAddresses: Array<string> | null | undefined;
  passphrase: string;
};
export type TransferFundsResponse = {
  id: string;
  amount: {
    quantity: number;
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
    unit: WalletUnits.LOVELACE;
  };
  fee: {
    quantity: number;
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
    unit: WalletUnits.LOVELACE;
  };
  deposit: {
    quantity: number;
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
    unit: WalletUnits.LOVELACE;
  };
  inserted_at?: {
    time: Date;
    block: Block;
  };
  pending_since?: {
    time: Date;
    block: Block;
  };
  depth: {
    quantity: number;
    unit: 'block';
  };
  direction: 'incoming' | 'outgoing';
  inputs: Array<Input>;
  outputs: Array<Output>;
  status: 'pending' | 'in_ledger';
};
export type CreateHardwareWalletRequest = {
  walletName: string;
  accountPublicKey: string;
};
export type GetCurrencyListResponse = Array<Currency>;
export type GetCurrencyRateRequest = LocalizedCurrency;
export type GetCurrencyRateResponse = number;
