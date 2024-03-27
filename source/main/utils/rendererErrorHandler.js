'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const logging_1 = require('./logging');
class RendererErrorHandler {
  count = 0;
  maxReloads = 1;
  window = null;
  createMainWindow = null;
  setup(window, createMainWindow) {
    this.window = window;
    this.createMainWindow = createMainWindow;
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('Renderer Error Handler started');
  }
  onError(errorType, error) {
    logging_1.logger.error(`RendererError::${errorType}`, {
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
exports.default = RendererErrorHandler;
//# sourceMappingURL=rendererErrorHandler.js.map
