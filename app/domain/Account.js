import { observable, action } from 'mobx';
import { Wallet } from './Wallet';

export default class Account {

  @observable wallets: Array<Wallet> = [];

  @action addWallet(wallet:Wallet) {
    this.wallets.push(wallet);
  }
}
