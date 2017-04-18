// @flow
import { observable, computed } from 'mobx';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';
import CachedRequest from './lib/CachedRequest';

export default class SettingsStore extends Store {

  @observable termsOfUseRequest = new CachedRequest(this.api, 'getTermsOfUse');

  @observable bigNumberDecimalFormat = {
    decimalSeparator: '.',
    groupSeparator: ',',
    groupSize: 3,
    secondaryGroupSize: 0,
    fractionGroupSeparator: ' ',
    fractionGroupSize: 0
  };

  setup() {
    this.registerReactions([
      this._setBigNumberFormat,
    ]);
  }

  @computed get termsOfUse(): string {
    return this.termsOfUseRequest.execute().result;
  }

  _setBigNumberFormat = () => {
    BigNumber.config({ FORMAT: this.bigNumberDecimalFormat });
  }
}
