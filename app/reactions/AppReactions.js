// @flow
import type { appState } from '../state';
import AppController from '../controllers/AppController';
import LoginReaction from './LoginReaction';

export default class AppReactions {

  state: appState;
  appController: AppController;
  loginReaction: LoginReaction;

  constructor(state: appState, appController: AppController) {
    this.state = state;
    this.appController = appController;
    this.loginReaction = new LoginReaction(state, appController).start();
  }

}
