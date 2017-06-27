// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';

export default class WalletAddress {

  @observable id: string = '';
  @observable amount: BigNumber;
  @observable isUsed: boolean = false;

  constructor(data: {
    id: string,
    amount: BigNumber,
    isUsed: boolean,
  }) {
    Object.assign(this, data);
  }

}
