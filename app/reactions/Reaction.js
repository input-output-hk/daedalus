// @flow
import { autorun } from 'mobx';
import storesType from '../stores';

export default class Reaction {

  stores: storesType;
  hasBeenStarted: boolean;
  dispose: () => void;

  constructor(stores: storesType) {
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
