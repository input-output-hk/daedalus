// @flow
import { observable, action } from 'mobx';
import Profile from './Profile';
import Wallet from './Wallet';

export default class User {

  @observable id: string;
  @observable profile: Profile;
  @observable wallets: Array<Wallet> = [];

  constructor(id:string, profile: Profile) {
    this.id = id;
    this.profile = profile;
  }

  @action addWallet(wallet: Wallet) {
    this.wallets.push(wallet);
  }
}
