// @flow
import type { appState } from '../state';
import AppController from '../controllers/AppController';
import LoginReaction from './LoginReaction';
import LogoutReaction from './LogoutReaction';
import storesType from '../stores';

export default class Reactions {

  loginReaction: LoginReaction;
  logoutReaction: LogoutReaction;

  constructor(state: appState, appController: AppController, stores: storesType) {
    this.loginReaction = new LoginReaction(state, appController, stores);
    this.logoutReaction = new LogoutReaction(state, appController, stores);
    // Start reactions that need to be started upon application launch
    this.loginReaction.start();
    this.logoutReaction.start();
  }

}
