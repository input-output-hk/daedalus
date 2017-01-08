// @flow
import AppController from '../controllers/AppController';
import LoginReaction from './LoginReaction';
import LogoutReaction from './LogoutReaction';
import storesType from '../stores';

export default class Reactions {

  loginReaction: LoginReaction;
  logoutReaction: LogoutReaction;

  constructor(appController: AppController, stores: storesType) {
    this.loginReaction = new LoginReaction(appController, stores);
    this.logoutReaction = new LogoutReaction(appController, stores);
    // Start reactions that need to be started upon application launch
    this.loginReaction.start();
    this.logoutReaction.start();
  }

}
