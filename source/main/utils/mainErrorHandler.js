// @flow
import { Logger, stringifyError } from '../../common/logging';
import unhandled from 'electron-unhandled';

export default () => {
  unhandled({
    logger: (error: any) => Logger.error(`unhandledException::main: ${stringifyError(error)}`),
    showDialog: false
  });

  process.on('uncaughtException', (error: any) => {
    Logger.error(`uncaughtException: ${stringifyError(error)}`);
  });
};
