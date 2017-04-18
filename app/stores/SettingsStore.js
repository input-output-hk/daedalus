// @flow
import { observable, computed } from 'mobx';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';

export default class SettingsStore extends Store {

  @observable bigNumberDecimalFormat = {
    decimalSeparator: ',',
    groupSeparator: '.',
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

  _setBigNumberFormat = () => {
    BigNumber.config({ FORMAT: this.bigNumberDecimalFormat });
  }
}
