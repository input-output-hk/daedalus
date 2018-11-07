// @flow
import { app /* , BrowserWindow */ } from 'electron';
import unhandled from 'electron-unhandled';
import { Logger, stringifyError } from '../../common/logging';
// import { handleNoDiskSpace } from '../ipc/no-disk-space';

export default (/* mainWindow: BrowserWindow */) => {
  unhandled({
    logger: (error: any) => Logger.error(`unhandledException::main: ${stringifyError(error)}`),
    showDialog: false
  });

  // setTimeout(() => {
  //   handleNoDiskSpace(mainWindow);
  // }, 5000);

  process.on('uncaughtException', (error: any) => {
    // if (error === 'noEnoughDiskSpace') {
    //   handleNoDiskSpace(mainWindow);
    // }
    Logger.error(`uncaughtException: ${stringifyError(error)}`);
  });

  app.on('gpu-process-crashed', (event: any, killed: boolean) => {
    Logger.error(`uncaughtException::gpu-process-crashed: ${killed ? 'killed' : 'not-killed'} ${stringifyError(event)}`);
  });
};
