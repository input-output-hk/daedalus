import { action } from 'mobx';
import Store from './lib/Store';
import {
  closeWindowChannel,
  resizeWindowChannel,
} from '../ipc/windowControlChannels';

export default class WindowStore extends Store {
  _isTest = false;

  setup() {
    this.actions.window.resizeWindow.listen(this._resizeWindow);
    this.actions.window.closeWindow.listen(this.closeWindow);
    this.actions.app.initAppEnvironment.listen(() => {});
  }

  closeWindow = () => {
    closeWindowChannel.send().catch(() => undefined);
  };
  // PRIVATE
  _onGetAppEnvironmentSuccess = action((event, { isTest }) => {
    this._isTest = isTest;
  });
  _resizeWindow = ({ width, height }: { width: number; height: number }) => {
    resizeWindowChannel
      .send({ width, height, animate: !this._isTest })
      .catch(() => undefined);
  };
}
