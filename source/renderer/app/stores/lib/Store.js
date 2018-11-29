// @flow
import Reaction from './Reaction';
import type { ActionsMap } from '../../actions/index';
import type { StoresMap } from '../../stores/index';
import type { Api } from '../../api/index';

export default class Store {

  stores: StoresMap;
  api: Api;
  actions: ActionsMap;

  _reactions: Array<Reaction> = [];

  constructor(api: Api, actions: ActionsMap) {
    this.api = api;
    this.actions = actions;
  }

  registerReactions(reactions: Array<Function>) {
    reactions.forEach(reaction => this._reactions.push(new Reaction(reaction)));
  }

  configure(stores: StoresMap) {
    this.stores = stores;
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
