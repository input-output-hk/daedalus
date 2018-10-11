// @flow
import { ipcRenderer } from 'electron';
import { action } from 'mobx';
import Store from './lib/Store';
import { GET_APP_ENVIRONMENT } from '../../../common/ipc-api';

export default class WindowStore extends Store {

  _isTest: boolean = false;

  setup() {
    this.actions.window.resizeWindow.listen(this._resizeWindow);
    this.actions.app.initAppEnvironment.listen(() => {});
    ipcRenderer.on(GET_APP_ENVIRONMENT.SUCCESS, this._onGetAppEnvironmentSuccess);
  }

  _onGetAppEnvironmentSuccess = action((event, { isTest }) => {
    this._isTest = isTest;
  });

  // PRIVATE

  _resizeWindow = ({ width, height }: { width: number, height: number }) => {
    ipcRenderer.send('resize-window', { width, height, animate: !this._isTest });
  };

}
