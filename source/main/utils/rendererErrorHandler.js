// @flow
import { BrowserWindow } from 'electron';
import { logger } from './logging';

export default class RendererErrorHandler {
  count: number = 0;
  maxReloads: number = 1;
  window: ?BrowserWindow = null;
  createMainWindow: ?Function = null;

  setup(window: BrowserWindow, createMainWindow: Function) {
    this.window = window;
    this.createMainWindow = createMainWindow;
    logger.info('Renderer Error Handler started');
  }

  onError(errorType: string, error: any) {
    logger.error(`RendererError::${errorType}`, { error });

    if (this.count < this.maxReloads) {
      this.count++;
      // eslint-disable-next-line no-unused-expressions
      this.createMainWindow && this.createMainWindow();
    } else {
      this.count = 0;
    }
  }
}
