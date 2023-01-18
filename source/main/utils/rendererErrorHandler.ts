import { BrowserWindow } from 'electron';
import { logger } from './logging';

export default class RendererErrorHandler {
  count = 0;
  maxReloads = 1;
  window: BrowserWindow | null | undefined = null;
  createMainWindow: ((...args: Array<any>) => any) | null | undefined = null;

  setup(window: BrowserWindow, createMainWindow: (...args: Array<any>) => any) {
    this.window = window;
    this.createMainWindow = createMainWindow;
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('Renderer Error Handler started');
  }

  onError(errorType: string, error: any) {
    logger.error(`RendererError::${errorType}`, {
      error,
    });

    if (this.count < this.maxReloads) {
      this.count++;
      // eslint-disable-next-line no-unused-expressions
      this.createMainWindow && this.createMainWindow();
    } else {
      this.count = 0;
    }
  }
}
