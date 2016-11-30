import type { appState } from '../state/index';
import AppController from './AppController';

export default class BaseController {
  state: appState;
  appController: AppController;

  constructor(appController: AppController, state: appState) {
    this.state = state;
    this.appController = appController;
  }
}
