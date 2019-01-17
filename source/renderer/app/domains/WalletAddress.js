// @flow
import { observable } from 'mobx';

type Params = {
  id: string,
  used: boolean,
  changeAddress: boolean,
};

export default class WalletAddress {

  @observable id: string = '';
  @observable used: boolean = false;
  @observable changeAddress: boolean = false;

  constructor(data: Params) {
    Object.assign(this, data);
  }

}
