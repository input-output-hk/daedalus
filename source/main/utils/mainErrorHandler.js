// @flow
import { app, BrowserWindow } from 'electron';
import unhandled from 'electron-unhandled';
import { Logger, stringifyError } from '../../common/logging';
import { onNoDiskSpaceError } from '../ipc/no-disk-space';

let dummyTrigger = false;

export default (mainWindow: BrowserWindow) => {

  Logger.info('mainErrorHandler.js started ==========--------=====');

  unhandled({
    logger: (error: any) => Logger.error(`unhandledException::main: ${stringifyError(error)}`),
    showDialog: false
  });

  process.on('uncaughtException', (error: any) => {

    const err = `${stringifyError(error)}`;

    Logger.error(`uncaughtException: ${err}`);

    if (err.indexOf('ENOSPC') > -1) {
      onNoDiskSpaceError(mainWindow);
      return false;
    }

  });

  app.on('gpu-process-crashed', (event: any, killed: boolean) => {
    Logger.error(`uncaughtException::gpu-process-crashed: ${killed ? 'killed' : 'not-killed'} ${stringifyError(event)}`);
  });
};
