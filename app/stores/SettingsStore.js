// @flow
import { observable, computed, action } from 'mobx';
import isLength from 'validator/lib/isLength';
import isEmail from 'validator/lib/isEmail';
import Store from './lib/Store';
import CachedRequest from './lib/CachedRequest';

export default class SettingsStore extends Store {

  @observable termsOfUseRequest = new CachedRequest(this.api, 'getTermsOfUse');
  @observable settingsFieldBeingEdited = null;

  constructor(...args) {
    super(...args);
    this.actions.startEditingSettingsField.listen(this._startEditingSettingsField.bind(this));
    this.actions.stopEditingSettingsField.listen(this._stopEditingSettingsField.bind(this));
  }

  @computed get termsOfUse() {
    return this.termsOfUseRequest.execute().result;
  }

  @action _startEditingSettingsField(params) {
    this.settingsFieldBeingEdited = params.field;
  }

  @action _stopEditingSettingsField() {
    this.settingsFieldBeingEdited = null;
  }

  isValidName(name: string) {
    return isLength(name, { min: 2, max: 30 });
    // TODO: Add API validation
  }

  isValidEmail(email: string) {
    return isEmail(email);
  }
}
