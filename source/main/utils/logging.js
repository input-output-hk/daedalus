// @flow
import fs from 'fs';
import path from 'path';
import log from 'electron-log-daedalus';
import { environment } from '../environment';
import { formatContext } from '../../common/utils/logging';
import type { FormatMessageContextParams } from '../../common/types/logging.types';
import { pubLogsFolderPath, APP_NAME } from '../config';

const appName = 'daedalus';
const electronProcess = 'ipcMain';
const { network, os, platformVersion, version } = environment;

const messageContext: FormatMessageContextParams = {
  appName,
  electronProcess,
  network,
  level: '',
};

const environmentData = {
  network,
  os,
  platformVersion,
  version,
};

const logToLevel = (level: string) => (message: string, data: ?Object) =>
  log[level](formatContext({ ...messageContext, level }), {
    message,
    data,
    environmentData,
  });

const composeRotatedOldLogFiles = (compareLogFilePath: string) => {
  let index = 0;
  const rotatedLogFilePathes = [
    path.join(pubLogsFolderPath, `${APP_NAME}.old.1.json`),
    path.join(pubLogsFolderPath, `${APP_NAME}.old.2.json`),
  ];

  for (index = 0; index < 2; index++) {
    if (!fs.existsSync(rotatedLogFilePathes[index])) {
      break;
    }
  }
  if (index < 2) {
    fs.copyFileSync(compareLogFilePath, rotatedLogFilePathes[index]);
    fs.unlinkSync(compareLogFilePath);
    return;
  }

  fs.unlinkSync(rotatedLogFilePathes[0]);
  fs.copyFileSync(rotatedLogFilePathes[1], rotatedLogFilePathes[0]);
  fs.unlinkSync(rotatedLogFilePathes[1]);
  fs.copyFileSync(compareLogFilePath, rotatedLogFilePathes[1]);
  fs.unlinkSync(compareLogFilePath);
};

export const rotateOldLogFiles = () => {
  const oldLogFilePath = path.join(pubLogsFolderPath, `${APP_NAME}.old.json`);
  const compareLogFilePath = path.join(
    pubLogsFolderPath,
    `${APP_NAME}.compare.json`
  );

  if (!fs.existsSync(oldLogFilePath)) {
    return;
  }

  if (!fs.existsSync(compareLogFilePath)) {
    fs.copyFileSync(oldLogFilePath, compareLogFilePath);
    return;
  }

  const oldLogFileBuf = fs.readFileSync(oldLogFilePath);
  const compareLogFileBuf = fs.readFileSync(compareLogFilePath);

  if (oldLogFileBuf.equals(compareLogFileBuf)) {
    return;
  }

  composeRotatedOldLogFiles(compareLogFilePath);
  fs.copyFileSync(oldLogFilePath, compareLogFilePath);
};

export const Logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
