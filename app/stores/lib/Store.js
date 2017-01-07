// @flow
export default class Store {
  constructor(stores, api, actions) {
    this.stores = stores;
    this.api = api;
    this.actions = actions;
  }
}
