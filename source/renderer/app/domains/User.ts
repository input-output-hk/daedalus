import { observable } from 'mobx';
import Profile from './Profile';

export default class User {
  @observable
  id: string;
  @observable
  profile: Profile;

  constructor(id: string, profile: Profile) {
    this.id = id;
    this.profile = profile;
  }
}
