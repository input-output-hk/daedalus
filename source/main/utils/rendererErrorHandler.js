// @flow
import { BrowserWindow } from 'electron';
import { Logger } from './logging';

export default class RendererErrorHandler {
  count: number = 0;
  maxReloads: number = 1;
  window: ?BrowserWindow = null;
  createMainWindow: ?Function = null;

  setup(window: BrowserWindow, createMainWindow: Function) {
    this.window = window;
    this.createMainWindow = createMainWindow;
    Logger.info('Renderer Error Handler started');
  }

  onError(errorType: string, error: any) {
    Logger.error(`RendererError::${errorType}`, { error });

    if (this.count < this.maxReloads) {
      this.count++;
      // eslint-disable-next-line no-unused-expressions
      this.createMainWindow && this.createMainWindow();
    } else {
      this.count = 0;
    }
  }
}
