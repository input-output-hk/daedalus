// @flow
import Reaction from './Reaction';
import type { ActionsMap } from '../../actions/index';
import type { StoresMap } from '../../stores/index';
import type { UniversalApi } from '../../api/index';

export default class Store {

  stores: StoresMap;
  api: UniversalApi;
  actions: ActionsMap;

  _reactions: Array<Reaction> = [];

  constructor(stores: StoresMap, api: UniversalApi, actions: ActionsMap) {
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
