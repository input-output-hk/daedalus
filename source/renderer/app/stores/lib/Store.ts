import Reaction from './Reaction';
import type { StoresMap } from '../index';
import type { Environment } from '../../../../common/types/environment.types';

export default class Store {
  stores: StoresMap;
  environment: Environment = global.environment;
  _reactions: Array<Reaction> = [];

  registerReactions(reactions: Array<(...args: Array<any>) => any>) {
    reactions.forEach((reaction) =>
      this._reactions.push(new Reaction(reaction))
    );
  }

  configure(stores: StoresMap) {
    this.stores = stores;
  }

  setup() {}

  initialize() {
    this.setup();

    this._reactions.forEach((reaction) => reaction.start());
  }

  teardown() {
    this._reactions.forEach((reaction) => reaction.stop());
  }
}
