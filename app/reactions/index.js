// @flow
import LoginReaction from './LoginReaction';
import LogoutReaction from './LogoutReaction';
import storesType from '../stores';

export default class Reactions {

  loginReaction: LoginReaction;
  logoutReaction: LogoutReaction;

  constructor(stores: storesType) {
    this.loginReaction = new LoginReaction(stores);
    this.logoutReaction = new LogoutReaction(stores);
    // Start reactions that need to be started upon application launch
    this.loginReaction.start();
    this.logoutReaction.start();
  }

}
