// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';
import WalletAddress from './WalletAddress';

export default class WalletAccount {

  id: string = '';
  @observable name: string = '';
  @observable amount: BigNumber;
  @observable addresses: Array<WalletAddress> = [];

  constructor(data: {
    id: string,
    name: string,
    amount: BigNumber,
    addresses: Array<WalletAddress>,
  }) {
    Object.assign(this, data);
  }

}
