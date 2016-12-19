// @flow
import { autorun } from 'mobx';
import type { appState } from '../state';
import AppController from '../controllers/AppController';

export default class Reaction {
  state: appState;
  appController: AppController;
  hasBeenStarted: boolean;
  dispose: () => void;

  constructor(state: appState, appController: AppController) {
    this.state = state;
    this.appController = appController;
    this.hasBeenStarted = false;
  }

  reaction() {

  }

  start() {
    this.dispose = autorun(() => {
      this.reaction();
    });
    this.hasBeenStarted = true;
  }

  stop() {
    if (this.hasBeenStarted) {
      this.dispose();
    }
  }
}
