// @flow
import { app } from 'electron';
import unhandled from 'electron-unhandled';
import { Logger } from './logging';
import { stringifyError } from '../../common/utils/logging';

export default (onError?: Function) => {
  Logger.info('========== Main Error Handler started ==========');

  unhandled({
    logger: (error: any) => Logger.error('unhandledException::main', { error }),
    showDialog: false,
  });

  process.on('uncaughtException', (error: any) => {
    const err = `${stringifyError(error)}`;
    Logger.error('uncaughtException', { error });
    if (typeof onError === 'function') onError(err);
  });

  app.on('gpu-process-crashed', (event: any, killed: boolean) => {
    Logger.error(
      `uncaughtException::gpu-process-crashed: ${
        killed ? 'killed' : 'not-killed'
      }`,
      { error: event }
    );
  });
};
