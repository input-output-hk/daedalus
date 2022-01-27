import { get, pick } from 'lodash';
import { observable, computed, action } from 'mobx';
import BigNumber from 'bignumber.js';
import type {
  WalletSyncState,
  SyncStateStatus,
  DelegationStatus,
  WalletUnit,
  WalletPendingDelegations,
  Discovery,
} from '../api/wallets/types';
import type { WalletTokens } from '../api/assets/types';

export const WalletDiscovery: {
  RANDOM: Discovery;
  SEQUENTIAL: Discovery;
} = {
  RANDOM: 'random',
  SEQUENTIAL: 'sequential',
};
export const WalletSyncStateStatuses: {
  RESTORING: SyncStateStatus;
  SYNCING: SyncStateStatus;
  READY: SyncStateStatus;
  NOT_RESPONDING: SyncStateStatus;
} = {
  RESTORING: 'syncing',
  // @API TODO - calculate if the wallet is restoring!
  SYNCING: 'syncing',
  READY: 'ready',
  NOT_RESPONDING: 'not_responding',
};
export const WalletDelegationStatuses: {
  DELEGATING: DelegationStatus;
  NOT_DELEGATING: DelegationStatus;
} = {
  DELEGATING: 'delegating',
  NOT_DELEGATING: 'not_delegating',
};
export type HwDeviceStatus =
  | 'connecting'
  | 'connecting_failed'
  | 'trezor_bridge_failure'
  | 'launching_cardano_app'
  | 'exporting_public_key'
  | 'exporting_public_key_failed'
  | 'ready'
  | 'verifying_transaction'
  | 'verifying_transaction_failed'
  | 'wrong_firmware'
  | 'wrong_cardano_app_version'
  | 'unsupported_device'
  | 'verifying_transaction_succeeded'
  | 'verifying_address'
  | 'verifying_address_confirmation'
  | 'verifying_address_failed'
  | 'verifying_address_aborted'
  | 'verifying_address_succeeded';
export const HwDeviceStatuses: {
  CONNECTING: HwDeviceStatus;
  CONNECTING_FAILED: HwDeviceStatus;
  LAUNCHING_CARDANO_APP: HwDeviceStatus;
  EXPORTING_PUBLIC_KEY: HwDeviceStatus;
  EXPORTING_PUBLIC_KEY_FAILED: HwDeviceStatus;
  READY: HwDeviceStatus;
  VERIFYING_TRANSACTION: HwDeviceStatus;
  VERIFYING_TRANSACTION_FAILED: HwDeviceStatus;
  VERIFYING_TRANSACTION_SUCCEEDED: HwDeviceStatus;
  WRONG_FIRMWARE: HwDeviceStatus;
  WRONG_CARDANO_APP_VERSION: HwDeviceStatus;
  UNSUPPORTED_DEVICE: HwDeviceStatus;
  TREZOR_BRIDGE_FAILURE: HwDeviceStatus;
  VERIFYING_ADDRESS: HwDeviceStatus;
  VERIFYING_ADDRESS_CONFIRMATION: HwDeviceStatus;
  VERIFYING_ADDRESS_FAILED: HwDeviceStatus;
  VERIFYING_ADDRESS_ABORTED: HwDeviceStatus;
  VERIFYING_ADDRESS_SUCCEEDED: HwDeviceStatus;
} = {
  CONNECTING: 'connecting',
  CONNECTING_FAILED: 'connecting_failed',
  TREZOR_BRIDGE_FAILURE: 'trezor_bridge_failure',
  LAUNCHING_CARDANO_APP: 'launching_cardano_app',
  EXPORTING_PUBLIC_KEY: 'exporting_public_key',
  EXPORTING_PUBLIC_KEY_FAILED: 'exporting_public_key_failed',
  WRONG_FIRMWARE: 'wrong_firmware',
  WRONG_CARDANO_APP_VERSION: 'wrong_cardano_app_version',
  UNSUPPORTED_DEVICE: 'unsupported_device',
  READY: 'ready',
  VERIFYING_TRANSACTION: 'verifying_transaction',
  VERIFYING_TRANSACTION_FAILED: 'verifying_transaction_failed',
  VERIFYING_TRANSACTION_SUCCEEDED: 'verifying_transaction_succeeded',
  VERIFYING_ADDRESS: 'verifying_address',
  VERIFYING_ADDRESS_CONFIRMATION: 'verifying_address_confirmation',
  VERIFYING_ADDRESS_FAILED: 'verifying_address_failed',
  VERIFYING_ADDRESS_ABORTED: 'verifying_address_aborted',
  VERIFYING_ADDRESS_SUCCEEDED: 'verifying_address_succeeded',
};
export const WalletUnits: {
  ADA: WalletUnit;
  LOVELACE: WalletUnit;
} = {
  ADA: 'ada',
  LOVELACE: 'lovelace',
};
export type WalletProps = {
  id: string;
  addressPoolGap: number;
  name: string;
  amount: BigNumber;
  availableAmount: BigNumber;
  reward: BigNumber;
  assets: WalletTokens;
  passwordUpdateDate: Date | null | undefined;
  syncState: WalletSyncState;
  isLegacy: boolean;
  isHardwareWallet?: boolean;
  delegatedStakePoolId?: string | null | undefined;
  delegationStakePoolStatus?: string | null | undefined;
  lastDelegatedStakePoolId?: string | null | undefined;
  lastDelegationStakePoolStatus?: string | null | undefined;
  pendingDelegations?: WalletPendingDelegations;
  discovery: Discovery;
  hasPassword: boolean;
  walletNotConnected?: boolean;
};
export default class Wallet {
  id = '';
  @observable
  addressPoolGap: number;
  @observable
  name = '';
  @observable
  amount: BigNumber;
  @observable
  availableAmount: BigNumber;
  @observable
  reward: BigNumber;
  @observable
  assets: WalletTokens;
  @observable
  passwordUpdateDate: Date | null | undefined;
  @observable
  syncState: WalletSyncState;
  @observable
  isLegacy: boolean;
  @observable
  delegatedStakePoolId: string | null | undefined;
  @observable
  delegationStakePoolStatus: string | null | undefined;
  @observable
  lastDelegatedStakePoolId: string | null | undefined;
  @observable
  lastDelegationStakePoolStatus: string | null | undefined;
  @observable
  pendingDelegations: WalletPendingDelegations;
  @observable
  discovery: Discovery;
  @observable
  hasPassword: boolean;
  @observable
  walletNotConnected: boolean;
  @observable
  isHardwareWallet: boolean;

  constructor(data: WalletProps) {
    Object.assign(this, data);
  }

  @action
  update(other: Wallet) {
    Object.assign(
      this,
      pick(other, [
        'id',
        'addressPoolGap',
        'name',
        'amount',
        'availableAmount',
        'reward',
        'assets',
        'passwordUpdateDate',
        'syncState',
        'isLegacy',
        'delegatedStakePoolId',
        'delegationStakePoolStatus',
        'lastDelegatedStakePoolId',
        'lastDelegationStakePoolStatus',
        'pendingDelegations',
        'discovery',
        'hasPassword',
        'walletNotConnected',
        'isHardwareWallet',
      ])
    );
  }

  @computed
  get hasFunds(): boolean {
    return this.amount.gt(0);
  }

  @computed
  get hasAssets(): boolean {
    return get(this, 'assets.total', []).length > 0;
  }

  @computed
  get isRestoring(): boolean {
    return (
      get(this, 'syncState.status') === WalletSyncStateStatuses.RESTORING &&
      this.restorationProgress < 100
    );
  }

  @computed
  get isSyncing(): boolean {
    return get(this, 'syncState.status') === WalletSyncStateStatuses.SYNCING;
  }

  @computed
  get isNotResponding(): boolean {
    return (
      get(this, 'syncState.status') === WalletSyncStateStatuses.NOT_RESPONDING
    );
  }

  @computed
  get isRandom(): boolean {
    return this.discovery === WalletDiscovery.RANDOM;
  }

  @computed
  get isDelegating(): boolean {
    return this.lastDelegationStakePoolStatus
      ? this.lastDelegationStakePoolStatus ===
          WalletDelegationStatuses.DELEGATING
      : this.delegationStakePoolStatus === WalletDelegationStatuses.DELEGATING;
  }

  @computed
  get isSequential(): boolean {
    return this.discovery !== WalletDiscovery.RANDOM;
  }

  @computed
  get restorationProgress(): number {
    return get(this, 'syncState.progress.quantity', 0);
  }
}
