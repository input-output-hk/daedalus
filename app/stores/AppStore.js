import { observable, action } from 'mobx';
import Store from './lib/Store';

export default class AppStore extends Store {
  @observable isInitialized = false;

  @action initialize() {
    this.isInitialized = true;
  }
}
