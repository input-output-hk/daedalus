// @flow
import { observable } from 'mobx';

export default class WalletAddress {

  @observable id: string = '';
  @observable used: boolean = false;
  @observable changeAddress: boolean = false;

  constructor(data: {
    id: string,
    used: boolean,
    changeAddress: boolean,
  }) {
    Object.assign(this, data);
  }

}
