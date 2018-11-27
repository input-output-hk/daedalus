// @flow
import Reaction from './Reaction';
import type { ActionsMap } from '../../actions/index';
import type { StoresMap } from '../../stores/index';
import type { Api } from '../../api/index';
import type { Environment } from '../../../../common/types/environment.types';

export default class Store {

  stores: StoresMap;
  api: Api;
  actions: ActionsMap;
  environment: Environment = global.environment;

  _reactions: Array<Reaction> = [];

  constructor(stores: StoresMap, api: Api, actions: ActionsMap) {
    this.stores = stores;
    this.api = api;
    this.actions = actions;
  }

  registerReactions(reactions: Array<Function>) {
    reactions.forEach(reaction => this._reactions.push(new Reaction(reaction)));
  }

  setup() {}

  initialize() {
    this.setup();
    this._reactions.forEach(reaction => reaction.start());
  }

  teardown() {
    this._reactions.forEach(reaction => reaction.stop());
  }
}
