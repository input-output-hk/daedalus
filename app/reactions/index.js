// @flow
import type { appState } from '../state';
import AppController from '../controllers/AppController';
import LoginReaction from './LoginReaction';
import LogoutReaction from './LogoutReaction';

export default class Reactions {

  state: appState;
  appController: AppController;
  loginReaction: LoginReaction;
  logoutReaction: LogoutReaction;

  constructor(state: appState, appController: AppController) {
    this.state = state;
    this.appController = appController;
    this.loginReaction = new LoginReaction(state, appController);
    this.logoutReaction = new LogoutReaction(state, appController);
    // Start reactions that need to be started upon application launch
    this.loginReaction.start();
    this.logoutReaction.start();
  }

}
