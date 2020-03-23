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
} from '../api/wallets/types';

export const WalletSyncStateStatuses: {
  RESTORING: SyncStateStatus,
  SYNCING: SyncStateStatus,
  READY: SyncStateStatus,
  NOT_RESPONDING: SyncStateStatus,
} = {
  RESTORING: 'syncing', // @API TODO - calculate if the wallet is restoring!
  SYNCING: 'syncing',
  READY: 'ready',
  NOT_RESPONDING: 'not-responding',
};

export const WalletDelegationStatuses: {
  DELEGATING: DelegationStatus,
  NOT_DELEGATING: DelegationStatus,
} = {
  DELEGATING: 'delegating',
  NOT_DELEGATING: 'not_delegating',
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
      ])
    );
  }

  @computed get hasFunds(): boolean {
    return this.amount.gt(0);
  }

  @computed get isRestoring(): boolean {
    return get(this, 'syncState.status') === WalletSyncStateStatuses.RESTORING;
  }

  @computed get isNotResponding(): boolean {
    return (
      get(this, 'syncState.status') === WalletSyncStateStatuses.NOT_RESPONDING
    );
  }

  @computed get restorationProgress(): number {
    return get(this, 'syncState.progress.quantity', 0);
  }
}
