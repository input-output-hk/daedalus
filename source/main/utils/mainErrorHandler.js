// @flow
import unhandled from 'electron-unhandled';
import { Logger, stringifyError } from '../../common/logging';

export default () => {
  unhandled({
    logger: (error: any) => Logger.error(`unhandledException::main: ${stringifyError(error)}`),
    showDialog: false
  });

  process.on('uncaughtException', (error: any) => {
    Logger.error(`uncaughtException: ${stringifyError(error)}`);
  });
};
