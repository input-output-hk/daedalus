// @flow
import { pick } from 'lodash';
import { observable, computed, action } from 'mobx';
import BigNumber from 'bignumber.js';
import type {
  WalletSyncState,
  SyncStateStatus,
  DelegationStatus,
  WalletUnit,
} from '../api/wallets/types';

export const WalletSyncStateStatuses: {
  RESTORING: SyncStateStatus,
  SYNCING: SyncStateStatus,
  READY: SyncStateStatus,
} = {
  RESTORING: 'syncing', // @API TODO - calculate if the wallet is restoring!
  SYNCING: 'syncing',
  READY: 'ready',
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
  isDelegated: boolean,
  delegatedStakePoolId?: ?string,
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
  @observable isDelegated: boolean;
  @observable delegatedStakePoolId: ?string;

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
        'isDelegated',
        'delegatedStakePoolId',
      ])
    );
  }

  @computed get hasFunds(): boolean {
    return this.amount.gt(0);
  }
}
