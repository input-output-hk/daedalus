// @flow
import { observable } from 'mobx';

type WalletAddressProps = {
  id: string,
  used: boolean,
  spendingPath: string,
};

export default class WalletAddress {
  @observable id: string = '';
  @observable used: boolean = false;
  @observable spendingPath: string = "1852'/1815'/0'";

  constructor(data: WalletAddressProps) {
    Object.assign(this, data);
  }
}
