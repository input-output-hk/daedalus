// @flow
import { observable, computed } from 'mobx';
import Store from './lib/Store';
import CachedRequest from './lib/CachedRequest';

export default class SettingsStore extends Store {

  @observable termsOfUseRequest = new CachedRequest(this.api, 'getTermsOfUse');

  @computed get termsOfUse(): string {
    return this.termsOfUseRequest.execute().result;
  }
}
