import { observable, makeObservable } from 'mobx';
import Profile from './Profile';

export default class User {
  id: string;
  profile: Profile;

  constructor(id: string, profile: Profile) {
    makeObservable(this, {
      id: observable,
      profile: observable,
    });

    this.id = id;
    this.profile = profile;
  }
}
