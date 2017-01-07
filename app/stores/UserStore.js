// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';
import CachedRequest from './lib/CachedRequest';

export default class UserStore extends Store {

  @observable loginRequest = new Request(this.api, 'login');
  @observable userRequest = new CachedRequest(this.api, 'getUser');

  constructor(...args) {
    super(...args);
    this.actions.login.listen(this._login.bind(this));
  }

  @computed get isLoggedIn() {
    return this.loginRequest.result === true && this.active !== null;
  }

  @computed get active() {
    return this.userRequest.execute().result;
  }

  @action _login(params) {
    this.loginRequest.execute(params);
  }
}
