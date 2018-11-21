// @flow
import { app, BrowserWindow } from 'electron';
import unhandled from 'electron-unhandled';
import { Logger, stringifyError } from '../../common/logging';
import { handleNoDiskSpace } from '../ipc/no-disk-space';

// let dummyTrigger = false;

export default (mainWindow: BrowserWindow) => {

  Logger.info('mainErrorHandler.js started ==========--------=====');

  unhandled({
    logger: (error: any) => Logger.error(`unhandledException::main: ${stringifyError(error)}`),
    showDialog: false
  });

  // if (!dummyTrigger) {
  //   dummyTrigger = true;
  //   // setTimeout(() => handleNoDiskSpace(mainWindow), 15000);
  // }

  process.on('uncaughtException', (error: any) => {

    const err = `${stringifyError(error)}`;

    Logger.error(`uncaughtException: ${err}`);

    if (err.indexOf('ENOSPC') > -1 || err.indexOf('notEnoughDiskSpace') > -1) {
      handleNoDiskSpace(mainWindow);
      return false;
    }

  });

  app.on('gpu-process-crashed', (event: any, killed: boolean) => {
    Logger.error(`uncaughtException::gpu-process-crashed: ${killed ? 'killed' : 'not-killed'} ${stringifyError(event)}`);
  });
};
