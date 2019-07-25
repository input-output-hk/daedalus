// @flow
import { observable } from 'mobx';
import type { Address } from '../api/addresses/types';

export default class WalletAddress {
  @observable id: string = '';
  @observable state: 'used' | 'unused' = 'unused';

  constructor(data: Address) {
    Object.assign(this, data);
  }
}
