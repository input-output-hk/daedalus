// @flow
import { BrowserWindow } from 'electron';
import unhandled from 'electron-unhandled';
import { Logger } from './logging';

unhandled({
  logger: (error: any) => Logger.error('unhandledException::renderer', { error }),
  showDialog: false
});

export default class RendererErrorHandler {
  count: number = 0;
  maxReloads: number = 1;
  window: ?BrowserWindow = null;
  createMainWindow: ?Function = null;

  setup(window: BrowserWindow, createMainWindow: Function) {
    this.window = window;
    this.createMainWindow = createMainWindow;
  }

  onError(errorType: string, error: any) {
    Logger.error(`RendererError::${errorType}`, { error });

    if (this.count < this.maxReloads) {
      this.count++;
      this.createMainWindow && this.createMainWindow();
    } else {
      this.count = 0;
    }
  }
}
