import { includes, sortBy } from 'lodash';
import fs from 'fs';
import path from 'path';
import {
  pubLogsFolderPath,
  ALLOWED_LOGS,
  ALLOWED_NODE_LOGS,
  ALLOWED_WALLET_LOGS,
  ALLOWED_LAUNCHER_LOGS,
  MAX_NODE_LOGS_ALLOWED,
  MAX_WALLET_LOGS_ALLOWED,
  MAX_LAUNCHER_LOGS_ALLOWED,
} from '../config';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_LOGS_CHANNEL } from '../../common/ipc/api';
import type {
  GetLogsRendererRequest,
  GetLogsMainResponse,
} from '../../common/ipc/api';
import type { LogFiles } from '../../renderer/app/types/LogTypes';
// IpcChannel<Incoming, Outgoing>
export const getLogsChannel: MainIpcChannel<
  GetLogsRendererRequest,
  GetLogsMainResponse
> = new MainIpcChannel(GET_LOGS_CHANNEL);

const isOldDaedalusLog = (fileName: string) => {
  const appLogNamePosition = fileName.indexOf(ALLOWED_LOGS[0]);

  // if fileName doesn't start with AppLog filename, it's not old daedalus log
  if (appLogNamePosition !== 0) {
    return false;
  }

  // extract subsequent string attached to log file name
  const fileNamePostfix = fileName.substring(ALLOWED_LOGS[0].length);

  // if extracted string is in -{datetimestamp} format, it's old daedalus log
  if (/^-\d{14}$/.test(fileNamePostfix)) {
    return true;
  }

  return false;
};

const isFileAllowed = (fileName: string) =>
  includes(ALLOWED_LOGS, fileName) || isOldDaedalusLog(fileName);

const isFileNodeLog = (fileName: string, nodeLogsIncluded: number) =>
  ALLOWED_NODE_LOGS.test(fileName) && nodeLogsIncluded < MAX_NODE_LOGS_ALLOWED;

const isFileWalletLog = (fileName: string, walletLogsIncluded: number) =>
  ALLOWED_WALLET_LOGS.test(fileName) &&
  walletLogsIncluded < MAX_WALLET_LOGS_ALLOWED;

const isFileLauncherLog = (fileName: string, nodeLogsIncluded: number) =>
  ALLOWED_LAUNCHER_LOGS.test(fileName) &&
  nodeLogsIncluded < MAX_LAUNCHER_LOGS_ALLOWED;

export default () => {
  getLogsChannel.onRequest(() => {
    // check if pub folder exists and create array of log file names
    const logFiles: Array<string> = [];

    if (fs.existsSync(pubLogsFolderPath)) {
      const files = fs.readdirSync(pubLogsFolderPath).sort().reverse();
      let nodeLogsIncluded = 0;
      let walletLogsIncluded = 0;
      let launcherLogsIncluded = 0;

      for (let i = 0; i < files.length; i++) {
        const currentFile = path.join(pubLogsFolderPath, files[i]);

        if (fs.statSync(currentFile).isFile()) {
          const fileName = path.basename(currentFile);

          if (isFileAllowed(fileName)) {
            logFiles.push(fileName);
          } else if (isFileNodeLog(fileName, nodeLogsIncluded)) {
            logFiles.push(fileName);
            nodeLogsIncluded++;
          } else if (isFileWalletLog(fileName, walletLogsIncluded)) {
            logFiles.push(fileName);
            walletLogsIncluded++;
          } else if (isFileLauncherLog(fileName, launcherLogsIncluded)) {
            logFiles.push(fileName);
            launcherLogsIncluded++;
          }
        }
      }
    }

    const logs: LogFiles = {
      path: pubLogsFolderPath,
      files: sortBy(logFiles, (log: string): string => {
        // custom file sorting which enforces correct ordering (like in ALLOWED_LOGS)
        const nameSegments = log.split('.');
        return nameSegments.shift() + nameSegments.join('').length;
      }),
    };
    return Promise.resolve(logs);
  });
};
