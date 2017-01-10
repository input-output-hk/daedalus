// @flow
import { observable, action, computed, runInAction } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';
import CachedRequest from './lib/CachedRequest';
import environment from '../environment';

export default class UserStore extends Store {

  @observable loginRequest = new Request(this.api, 'login');
  @observable userRequest = new CachedRequest(this.api, 'getUser');
  @observable updateProfileRequest = new Request(this.api, 'updateProfileField');

  constructor(...args) {
    super(...args);
    const { login, updateProfileField } = this.actions;
    this.mapActions([
      { action: login, listener: this._login },
      { action: updateProfileField, listener: this._updateProfileField },
    ]);
    this.registerReactions([
      this._resizeWindowOnAuthChange,
      this._autoLogin,
    ]);
  }

  @computed get isLoggedIn() {
    return this.loginRequest.result === true && this.active !== null;
  }

  @computed get active() {
    return this.userRequest.execute().result;
  }

  // PRIVATE

  _login = (params) => this.loginRequest.execute(params);

  _updateProfileField = async ({ field, value }) => {
    if (!this.active) return;
    await this.updateProfileRequest.execute({ field, value });
    runInAction(() => {
      this.active.profile[field] = value;
      if (field === 'languageLocale') this.stores.app.currentLocale = value;
    });
  };

  _resizeWindowOnAuthChange = () => {
    const { router, wallets } = this.stores;
    if (this.isLoggedIn && this.active && wallets.active) {
      if (router.location.pathname === '/login') {
        router.push(wallets.getWalletRoute(wallets.active.id));
      }
      this.actions.resizeWindow({ width: 1024, height: 768 });
    } else {
      if (router.location.pathname !== '/login') router.push('/login');
      this.actions.resizeWindow({ width: 480, height: 575 });
    }
  };

  _autoLogin = () => {
    if (environment.AUTO_LOGIN && !this.isLoggedIn) {
      if (environment.WITH_CARDANO_API) {
        this._login({ email: '', passwordHash: '' });
      } else {
        const user = this.api.repository.findUser();
        if (user) {
          const { email, passwordHash } = user.profile;
          this._login({ email, passwordHash });
        }
      }
    }
  };

}
