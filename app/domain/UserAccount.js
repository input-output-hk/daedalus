// @flow
import { observable, action } from 'mobx';
import Profile from './Profile';
import Wallet from './Wallet';

export default class UserAccount {

  @observable profile: ?Profile;
  @observable wallets: Array<Wallet> = [];

  @action addWallet(wallet: Wallet) {
    this.wallets.push(wallet);
  }
}
