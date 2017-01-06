// @flow
import { autorun } from 'mobx';
import type { appState } from '../state';
import AppController from '../controllers/AppController';
import storesType from '../stores';

export default class Reaction {

  state: appState;
  appController: AppController;
  stores: storesType;
  hasBeenStarted: boolean;
  dispose: () => void;

  constructor(state: appState, appController: AppController, stores: storesType) {
    this.state = state;
    this.appController = appController;
    this.stores = stores;
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
