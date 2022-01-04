import { action } from 'mobx';
import Store from './lib/Store';
// TODO: refactor all parts that rely on this to ipc channels!
// @ts-ignore ts-migrate(2339) FIXME: Property 'ipcRenderer' does not exist on type 'typ... Remove this comment to see the full error message
const { ipcRenderer } = global;
export default class WindowStore extends Store {
  _isTest = false;

  setup() {
    this.actions.window.resizeWindow.listen(this._resizeWindow);
    this.actions.window.closeWindow.listen(this.closeWindow);
    this.actions.app.initAppEnvironment.listen(() => {});
  }

  closeWindow = () => {
    // TODO: refactor to ipc channel
    ipcRenderer.send('close-window');
  };
  // PRIVATE
  _onGetAppEnvironmentSuccess = action((event, { isTest }) => {
    this._isTest = isTest;
  });
  _resizeWindow = ({ width, height }: { width: number; height: number }) => {
    // TODO: refactor to ipc channel
    ipcRenderer.send('resize-window', {
      width,
      height,
      animate: !this._isTest,
    });
  };
}
