// @flow
import { pick } from 'lodash';
import { observable, computed, action } from 'mobx';
import BigNumber from 'bignumber.js';
import type {
  WalletAssuranceLevel,
  WalletAssuranceMode,
  WalletSyncState,
  SyncStateTag,
} from '../api/wallets/types';
import type { StakePool } from '../api/staking/types';

export const WalletAssuranceModeOptions: {
  NORMAL: WalletAssuranceLevel,
  STRICT: WalletAssuranceLevel,
} = {
  NORMAL: 'normal',
  STRICT: 'strict',
};

export const WalletSyncStateTags: {
  RESTORING: SyncStateTag,
  SYNCED: SyncStateTag,
} = {
  RESTORING: 'restoring',
  SYNCED: 'synced',
};

const WalletAssuranceModes: {
  NORMAL: WalletAssuranceMode,
  STRICT: WalletAssuranceMode,
} = {
  NORMAL: {
    low: 3,
    medium: 9,
  },
  STRICT: {
    low: 5,
    medium: 15,
  },
};

export type WalletProps = {
  id: string,
  name: string,
  amount: BigNumber,
  assurance: WalletAssuranceLevel,
  hasPassword: boolean,
  passwordUpdateDate: ?Date,
  syncState?: WalletSyncState,
  isLegacy: boolean,
  inactiveStakePercentage?: number,
  isDelegated?: boolean,
  delegatedStakePool?: StakePool,
  createdAt: Date,
};

export default class Wallet {
  id: string = '';
  @observable name: string = '';
  @observable amount: BigNumber;
  @observable assurance: WalletAssuranceLevel;
  @observable hasPassword: boolean;
  @observable passwordUpdateDate: ?Date;
  @observable syncState: ?WalletSyncState;
  @observable isLegacy: boolean;
  @observable inactiveStakePercentage: ?number;
  @observable isDelegated: ?boolean;
  @observable delegatedStakePool: ?StakePool;
  @observable createdAt: Date;

  constructor(data: WalletProps) {
    Object.assign(this, data);
  }

  @action update(other: Wallet) {
    Object.assign(
      this,
      pick(other, [
        'id',
        'name',
        'amount',
        'assurance',
        'hasPassword',
        'passwordUpdateDate',
        'syncState',
        'isLegacy',
        'inactiveStakePercentage',
        'isDelegated',
        'delegatedStakePool',
        'createdAt',
      ])
    );
  }

  @computed get hasFunds(): boolean {
    return this.amount > 0;
  }

  @computed get assuranceMode(): WalletAssuranceMode {
    switch (this.assurance) {
      case WalletAssuranceModeOptions.NORMAL:
        return WalletAssuranceModes.NORMAL;
      case WalletAssuranceModeOptions.STRICT:
        return WalletAssuranceModes.STRICT;
      default:
        return WalletAssuranceModes.NORMAL;
    }
  }
}
