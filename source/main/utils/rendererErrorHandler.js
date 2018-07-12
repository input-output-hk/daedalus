// @flow
import { BrowserWindow } from 'electron';
import { Logger } from '../../common/logging';

export default class RendererErrorHandler {
  count: number = 0;
  maxReloads: number = 1;
  window: ?BrowserWindow = null;
  createMainWindow: ?Function = null;

  setup(window: BrowserWindow, createMainWindow: Function) {
    this.window = window;
    this.createMainWindow = createMainWindow;
  }

  onError(errorType: string) {
    Logger.error(`RendererError::${errorType}`);

    if (this.count < this.maxReloads) {
      this.count++;
      this.createMainWindow && this.createMainWindow();
      // this.window.destroy();
    } else {
      this.count = 0;
    }
  }
}
