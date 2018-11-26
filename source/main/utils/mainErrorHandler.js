// @flow
import { app } from 'electron';
import unhandled from 'electron-unhandled';
import { Logger, stringifyError } from '../../common/logging';

export default (onError?: Function) => {

  Logger.info('mainErrorHandler.js started ==========--------=====');

  unhandled({
    logger: (error: any) => Logger.error(`unhandledException::main: ${stringifyError(error)}`),
    showDialog: false
  });

  process.on('uncaughtException', (error: any) => {

    const err = `${stringifyError(error)}`;

    Logger.error(`uncaughtException: ${err}`);

    if (typeof onError === 'function') {
      onError.trigger(this, err);
    }

  });

  app.on('gpu-process-crashed', (event: any, killed: boolean) => {
    Logger.error(`uncaughtException::gpu-process-crashed: ${killed ? 'killed' : 'not-killed'} ${stringifyError(event)}`);
  });
};
