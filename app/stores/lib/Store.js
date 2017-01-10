import Reaction from './Reaction';

// @flow
export default class Store {

  _actionMappings = [];
  _reactions = [];

  constructor(stores, api, actions) {
    this.stores = stores;
    this.api = api;
    this.actions = actions;
  }

  mapActions(mappings) {
    mappings.forEach((mapping) => {
      mapping.action.listen(mapping.listener);
      this._actionMappings.push(mapping);
    });
  }

  registerReactions(reactions) {
    reactions.forEach(reaction => this._reactions.push(new Reaction(reaction)));
  }

  initialize() {
    this._reactions.forEach(reaction => reaction.start());
    this._actionMappings.forEach(mapping => mapping.action.listen(mapping.listener))
  }

  teardown() {
    this._actionMappings.forEach(mapping => mapping.action.remove(mapping.listener));
    this._reactions.forEach(reaction => reaction.stop());
  }
}
