// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';
import CachedRequest from './lib/CachedRequest';

export default class UserStore extends Store {

  @observable loginRequest = new Request(this.api, 'login');
  @observable userRequest = new CachedRequest(this.api, 'getUser');
  @observable updateProfileRequest = new Request(this.api, 'updateProfileField');

  constructor(...args) {
    super(...args);
    this.actions.login.listen(this._login);
    this.actions.updateProfileField.listen(this._updateProfileField.bind(this));
  }

  _login = (params) => this.loginRequest.execute(params);

  @action _updateProfileField = async ({ field, value }) => {
    if (!this.active) return;
    await this.updateProfileRequest.execute({ field, value });
    this.active.profile[field] = value;
    if (field === 'languageLocale') this.stores.app.currentLocale = value;
  };

  @computed get isLoggedIn() {
    return this.loginRequest.result === true && this.active !== null;
  }

  @computed get active() {
    return this.userRequest.execute().result;
  }


}
