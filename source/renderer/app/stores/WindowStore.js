// @flow
import { ipcRenderer } from 'electron';
import { computed, action } from 'mobx';
import Store from './lib/Store';
import { GET_APP_ENVIRONMENT } from '../../../common/ipc-api';

export default class WindowStore extends Store {

  _network: ?string = null;

  setup() {
    this.actions.window.resizeWindow.listen(this._resizeWindow);
    this.actions.app.initAppEnvironment.listen(() => {});
    ipcRenderer.on(GET_APP_ENVIRONMENT.SUCCESS, this._onGetAppEnvironmentSuccess);
  }

  @computed get isTestnet(): boolean {
    return (this._network === 'testnet');
  }

  _onGetAppEnvironmentSuccess = action((event, environment) => {
    this._network = environment.NETWORK;
  });

  // PRIVATE

  _resizeWindow = ({ width, height }: { width: number, height: number }) => {
    ipcRenderer.send('resize-window', { width, height, animate: !this.isTestnet });
  };

}
