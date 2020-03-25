// @flow
import { observable } from 'mobx';

type WalletAddressProps = {
  id: string,
  used: boolean,
  accountIndex?: number,
};

export default class WalletAddress {
  @observable id: string = '';
  @observable used: boolean = false;
  @observable accountIndex: ?number = 0;

  constructor(data: WalletAddressProps) {
    Object.assign(this, data);
  }
}
