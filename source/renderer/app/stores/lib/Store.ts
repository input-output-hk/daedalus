import Reaction from './Reaction';
import type { ActionsMap } from '../../actions/index';
import type { StoresMap } from '../index';
import type { Api } from '../../api/index';
import type { Environment } from '../../../../common/types/environment.types';

export default class Store {
  stores: StoresMap;
  api: Api;
  actions: ActionsMap;
  environment: Environment = global.environment;
  _reactions: Array<Reaction> = [];

  constructor(api: Api, actions: ActionsMap) {
    this.api = api;
    this.actions = actions;
  }

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
