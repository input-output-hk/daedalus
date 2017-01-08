// @flow
import { autorun } from 'mobx';
import AppController from '../controllers/AppController';
import storesType from '../stores';

export default class Reaction {

  stores: storesType;
  hasBeenStarted: boolean;
  dispose: () => void;

  constructor(appController: AppController, stores: storesType) {
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
