// @flow
import { observable } from 'mobx';

type WalletAddressProps = {
  id: string,
  used: boolean,
};

export default class WalletAddress {
  @observable id: string = '';
  @observable used: boolean = false;

  constructor(data: WalletAddressProps) {
    Object.assign(this, data);
  }
}
