// @flow
import { observable, computed } from 'mobx';
import BigNumber from 'bignumber.js';
import type {
  WalletAssuranceLevel,
  WalletAssuranceMode,
  WalletSyncState,
  SyncStateTag
} from '../api/ada/types';

export const WalletAssuranceModeOptions: {
  NORMAL: WalletAssuranceLevel, STRICT: WalletAssuranceLevel,
} = {
  NORMAL: 'normal', STRICT: 'strict',
};

export const WalletSyncStateTags: {
  RESTORING: SyncStateTag, SYNCED: SyncStateTag,
} = {
  RESTORING: 'restoring', SYNCED: 'synced',
};

const WalletAssuranceModes: { NORMAL: WalletAssuranceMode, STRICT: WalletAssuranceMode } = {
  NORMAL: {
    low: 3,
    medium: 9,
  },
  STRICT: {
    low: 5,
    medium: 15,
  }
};

export default class Wallet {

  id: string = '';
  @observable name: string = '';
  @observable amount: BigNumber;
  @observable assurance: WalletAssuranceLevel;
  @observable hasPassword: boolean;
  @observable passwordUpdateDate: ?Date;
  @observable syncState: ?WalletSyncState;

  constructor(data: {
    id: string,
    name: string,
    amount: BigNumber,
    assurance: WalletAssuranceLevel,
    hasPassword: boolean,
    passwordUpdateDate: ?Date,
    syncState?: WalletSyncState,
  }) {
    Object.assign(this, data);
  }

  @computed get hasFunds(): boolean {
    return this.amount > 0;
  }

  @computed get assuranceMode(): WalletAssuranceMode {
    switch (this.assurance) {
      case WalletAssuranceModeOptions.NORMAL: return WalletAssuranceModes.NORMAL;
      case WalletAssuranceModeOptions.STRICT: return WalletAssuranceModes.STRICT;
      default: return WalletAssuranceModes.NORMAL;
    }
  }

}
