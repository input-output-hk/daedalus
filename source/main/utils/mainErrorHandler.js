'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const electron_1 = require('electron');
const logging_1 = require('./logging');
const logging_2 = require('../../common/utils/logging');
exports.default = (onError) => {
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logging_1.logger.info('Main Error Handler started');
  const handleError = (title, error) => {
    const err = `${(0, logging_2.stringifyError)(error)}`;
    logging_1.logger.error(title, {
      error,
    });
    if (typeof onError === 'function') onError(err);
  };
  process.on('uncaughtException', (error) => {
    handleError('uncaughtException', error);
  });
  process.on('unhandledRejection', (error) => {
    handleError('unhandledRejection', {
      error,
      stack: error.stack,
      message: error.message,
    });
  });
  electron_1.app.on('gpu-process-crashed', (event, killed) => {
    logging_1.logger.error(
      `uncaughtException::gpu-process-crashed: ${
        killed ? 'killed' : 'not-killed'
      }`,
      {
        error: event,
      }
    );
  });
};
//# sourceMappingURL=mainErrorHandler.js.map
