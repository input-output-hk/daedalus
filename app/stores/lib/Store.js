// @flow
import Reaction from './Reaction';

export default class Store {

  stores: Object = {};
  api: Object = {};
  actions: Object = {};

  _reactions = [];

  constructor(stores: Object, api: Object, actions: Object) {
    this.stores = stores;
    this.api = api;
    this.actions = actions;
  }

  registerReactions(reactions: Array<Function>) {
    reactions.forEach(reaction => this._reactions.push(new Reaction(reaction)));
  }

  initialize() {
    this._reactions.forEach(reaction => reaction.start());
  }

  teardown() {
    this._reactions.forEach(reaction => reaction.stop());
  }
}
