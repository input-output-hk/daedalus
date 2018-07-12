// @flow
import { ipcRenderer } from 'electron';
import Store from './lib/Store';
import environment from '../../../common/environment';
import { ASSETS_LOADED } from '../../../common/ipc-api/load-asset';

export default class WindowStore extends Store {

  setup() {
    this.actions.window.resizeWindow.listen(this._resizeWindow);
    ipcRenderer.send(ASSETS_LOADED.SUCCESS);
  }

  // PRIVATE

  _resizeWindow = ({ width, height }: { width: number, height: number }) => {
    ipcRenderer.send('resize-window', { width, height, animate: !environment.isTest() });
  };

}
