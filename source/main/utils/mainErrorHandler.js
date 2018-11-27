// @flow
import { app } from 'electron';
import unhandled from 'electron-unhandled';
import { Logger } from './logging';
import { stringifyError } from '../../common/utils/logging';

export default () => {
  unhandled({
    logger: (error: any) => Logger.error(`unhandledException::main: ${stringifyError(error)}`),
    showDialog: false
  });

  process.on('uncaughtException', (error: any) => {
    Logger.error(`uncaughtException: ${stringifyError(error)}`);
  });

  app.on('gpu-process-crashed', (event: any, killed: boolean) => {
    Logger.error(`uncaughtException::gpu-process-crashed: ${killed ? 'killed' : 'not-killed'} ${stringifyError(event)}`);
  });
};
