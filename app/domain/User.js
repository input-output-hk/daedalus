// @flow
import { observable, action } from 'mobx';
import Profile from './Profile';
import Wallet from './Wallet';

export default class User {

  @observable id: string;
  @observable profile: Profile;

  constructor(id:string, profile: Profile) {
    this.id = id;
    this.profile = profile;
  }
}
