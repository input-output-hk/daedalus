import type { appState } from '../state';
import type { Api } from '../api';
import AppController from './AppController';

export default class BaseController {
  state: appState;
  api: Api;
  appController: AppController;

  constructor(appController: AppController, state: appState, api: Api, stores) {
    this.state = state;
    this.api = api;
    this.appController = appController;
    this.stores = stores;
  }
}
