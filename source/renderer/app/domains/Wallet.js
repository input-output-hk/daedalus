// @flow
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

export const WalletDiscovery: {
  RANDOM: Discovery,
  SEQUENTIAL: Discovery,
} = {
  RANDOM: 'random',
  SEQUENTIAL: 'sequential',
};

export const WalletSyncStateStatuses: {
  RESTORING: SyncStateStatus,
  SYNCING: SyncStateStatus,
  READY: SyncStateStatus,
  NOT_RESPONDING: SyncStateStatus,
} = {
  RESTORING: 'syncing', // @API TODO - calculate if the wallet is restoring!
  SYNCING: 'syncing',
  READY: 'ready',
  NOT_RESPONDING: 'not_responding',
};

export const WalletDelegationStatuses: {
  DELEGATING: DelegationStatus,
  NOT_DELEGATING: DelegationStatus,
} = {
  DELEGATING: 'delegating',
  NOT_DELEGATING: 'not_delegating',
};

export type HwDeviceStatus =
  | 'connecting'
  | 'launching_cardano_app'
  | 'exporting_public_key'
  | 'exporting_public_key_failed'
  | 'ready'
  | 'verifying_transaction'
  | 'verifying_transaction_failed'
  | 'verifying_transaction_succeeded';

export const HwDeviceStatuses: {
  CONNECTING: HwDeviceStatus,
  LAUNCHING_CARDANO_APP: HwDeviceStatus,
  EXPORTING_PUBLIC_KEY: HwDeviceStatus,
  EXPORTING_PUBLIC_KEY_FAILED: HwDeviceStatus,
  READY: HwDeviceStatus,
  VERIFYING_TRANSACTION: HwDeviceStatus,
  VERIFYING_TRANSACTION_FAILED: HwDeviceStatus,
  VERIFYING_TRANSACTION_SUCCEEDED: HwDeviceStatus,
} = {
  CONNECTING: 'connecting',
  LAUNCHING_CARDANO_APP: 'launching_cardano_app',
  EXPORTING_PUBLIC_KEY: 'exporting_public_key',
  EXPORTING_PUBLIC_KEY_FAILED: 'exporting_public_key_failed',
  READY: 'ready',
  VERIFYING_TRANSACTION: 'verifying_transaction',
  VERIFYING_TRANSACTION_FAILED: 'verifying_transaction_failed',
  VERIFYING_TRANSACTION_SUCCEEDED: 'verifying_transaction_succeeded',
};

export const WalletUnits: {
  ADA: WalletUnit,
  LOVELACE: WalletUnit,
} = {
  ADA: 'ada',
  LOVELACE: 'lovelace',
};

export type WalletProps = {
  id: string,
  addressPoolGap: number,
  name: string,
  amount: BigNumber,
  availableAmount: BigNumber,
  reward: BigNumber,
  passwordUpdateDate: ?Date,
  syncState: WalletSyncState,
  isLegacy: boolean,
  delegatedStakePoolId?: ?string,
  delegationStakePoolStatus?: ?string,
  lastDelegationStakePoolId?: ?string,
  pendingDelegations?: WalletPendingDelegations,
  discovery: Discovery,
  hasPassword: boolean,
  walletNotConnected?: boolean,
};

export default class Wallet {
  id: string = '';
  @observable addressPoolGap: number;
  @observable name: string = '';
  @observable amount: BigNumber;
  @observable availableAmount: BigNumber;
  @observable reward: BigNumber;
  @observable passwordUpdateDate: ?Date;
  @observable syncState: WalletSyncState;
  @observable isLegacy: boolean;
  @observable delegatedStakePoolId: ?string;
  @observable delegationStakePoolStatus: ?string;
  @observable lastDelegationStakePoolId: ?string;
  @observable pendingDelegations: WalletPendingDelegations;
  @observable discovery: Discovery;
  @observable hasPassword: boolean;
  @observable walletNotConnected: boolean;

  constructor(data: WalletProps) {
    Object.assign(this, data);
  }

  @action update(other: Wallet) {
    Object.assign(
      this,
      pick(other, [
        'id',
        'addressPoolGap',
        'name',
        'amount',
        'availableAmount',
        'reward',
        'passwordUpdateDate',
        'syncState',
        'isLegacy',
        'delegatedStakePoolId',
        'delegationStakePoolStatus',
        'lastDelegationStakePoolId',
        'pendingDelegations',
        'discovery',
        'hasPassword',
        'walletNotConnected',
      ])
    );
  }

  @computed get hasFunds(): boolean {
    return this.amount.gt(0);
  }

  @computed get isRestoring(): boolean {
    return (
      get(this, 'syncState.status') === WalletSyncStateStatuses.RESTORING &&
      this.restorationProgress < 100
    );
  }

  @computed get isNotResponding(): boolean {
    return (
      get(this, 'syncState.status') === WalletSyncStateStatuses.NOT_RESPONDING
    );
  }

  @computed get isRandom(): boolean {
    return this.discovery === WalletDiscovery.RANDOM;
  }

  @computed get isSequential(): boolean {
    return this.discovery !== WalletDiscovery.RANDOM;
  }

  @computed get restorationProgress(): number {
    return get(this, 'syncState.progress.quantity', 0);
  }
}
