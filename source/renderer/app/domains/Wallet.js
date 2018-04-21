// @flow
import { observable, computed } from 'mobx';
import BigNumber from 'bignumber.js';
import type { AssuranceMode, AssuranceModeOption } from '../types/transactionAssuranceTypes';
import type { AdaV1WalletSyncState, AdaV1WalletSyncStateTag } from '../api/ada/types';
import { assuranceModes, assuranceModeOptions } from '../types/transactionAssuranceTypes';

export const syncStateTags: {
  RESTORING: AdaV1WalletSyncStateTag, SYNCED: AdaV1WalletSyncStateTag,
} = {
  RESTORING: 'restoring', SYNCED: 'synced',
};

export default class Wallet {

  id: string = '';
  @observable name: string = '';
  @observable amount: BigNumber;
  @observable assurance: AssuranceModeOption;
  @observable hasPassword: boolean;
  @observable passwordUpdateDate: ?Date;
  @observable syncState: ?AdaV1WalletSyncState;

  constructor(data: {
    id: string,
    name: string,
    amount: BigNumber,
    assurance: AssuranceModeOption,
    hasPassword: boolean,
    passwordUpdateDate: ?Date,
    syncState?: AdaV1WalletSyncState,
  }) {
    Object.assign(this, data);
  }

  @computed get hasFunds(): boolean {
    return this.amount > 0;
  }

  @computed get assuranceMode(): AssuranceMode {
    switch (this.assurance) {
      case assuranceModeOptions.NORMAL: return assuranceModes.NORMAL;
      case assuranceModeOptions.STRICT: return assuranceModes.STRICT;
      default: return assuranceModes.NORMAL;
    }
  }

}
