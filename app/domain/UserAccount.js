// @flow
import { observable, action } from 'mobx';
import UserProfile from './UserProfile';
import Wallet from './Wallet';

export default class UserAccount {

  @observable profile: UserProfile;
  @observable wallets: Array<Wallet> = [];

  @action addWallet(wallet: Wallet) {
    this.wallets.push(wallet);
  }
}
