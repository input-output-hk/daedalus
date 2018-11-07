// @flow
import { ipcRenderer } from 'electron';
import { action } from 'mobx';
import Store from './lib/Store';
import { GET_APP_ENVIRONMENT } from '../../../common/ipc-api';

export default class WindowStore extends Store {

  _isTest: boolean = false;

  setup() {
    this.actions.window.resizeWindow.listen(this._resizeWindow);
    this.actions.window.closeWindow.listen(this.closeWindow);
    this.actions.app.initAppEnvironment.listen(() => {});
    ipcRenderer.on(GET_APP_ENVIRONMENT.SUCCESS, this._onGetAppEnvironmentSuccess);
  }

  closeWindow = () => ipcRenderer.send('close-window');

  // PRIVATE

  _onGetAppEnvironmentSuccess = action((event, { isTest }) => {
    this._isTest = isTest;
  });

  _resizeWindow = ({ width, height }: { width: number, height: number }) => {
    ipcRenderer.send('resize-window', { width, height, animate: !this._isTest });
  };


}
