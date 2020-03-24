// @flow
import { app } from 'electron';
import { logger } from './logging';
import { stringifyError } from '../../common/utils/logging';

export default (onError?: Function) => {
  logger.info('Main Error Handler started');

  const handleError = (title: string, error: any) => {
    const err = `${stringifyError(error)}`;
    logger.error(title, { error });
    if (typeof onError === 'function') onError(err);
  };

  process.on('uncaughtException', (error: any) => {
    handleError('uncaughtException', error);
  });

  process.on('unhandledRejection', (error: any) => {
    handleError('unhandledRejection', error);
  });

  app.on('gpu-process-crashed', (event: any, killed: boolean) => {
    logger.error(
      `uncaughtException::gpu-process-crashed: ${
        killed ? 'killed' : 'not-killed'
      }`,
      { error: event }
    );
  });
};
