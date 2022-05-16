import fs from 'fs';
import path from 'path';
import log from 'electron-log-daedalus';
import rimraf from 'rimraf';
import ensureDirectoryExists from './ensureDirectoryExists';
import { pubLogsFolderPath, appLogsFolderPath } from '../config';
import {
  constructMessageBody,
  formatMessage,
  stringifyData,
} from '../../common/utils/logging';
import { isFileNameWithTimestamp } from '../../common/utils/files';
import type {
  ConstructMessageBodyParams,
  MessageBody,
  LogSystemInfoParams,
  StateSnapshotLogParams,
  WalletMigrationReportData,
} from '../../common/types/logging.types';

const isTest = process.env.NODE_ENV === 'test';
const isDev = process.env.NODE_ENV === 'development';
export const setupLogging = () => {
  const logFilePath = path.join(pubLogsFolderPath, 'Daedalus.json');
  ensureDirectoryExists(pubLogsFolderPath);
  rimraf.sync(path.join(pubLogsFolderPath, './Daedalus.*'));
  log.transports.console.level = isTest ? 'error' : 'info';
  log.transports.rendererConsole.level = isDev ? 'info' : 'error';
  log.transports.file.level = 'debug';
  log.transports.file.maxSize = 5 * 1024 * 1024; // 5MB, unit bytes

  // @ts-ignore ts-migrate(2339) FIXME: Property 'maxItems' does not exist on type 'IFileT... Remove this comment to see the full error message
  log.transports.file.maxItems = 4;
  // @ts-ignore ts-migrate(2339) FIXME: Property 'timeStampPostfixFormat' does not exist o... Remove this comment to see the full error message
  log.transports.file.timeStampPostfixFormat = '{y}{m}{d}{h}{i}{s}';
  log.transports.file.file = logFilePath;

  log.transports.console.format = (message: Record<string, any>): string =>
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'Record<string, any>' is not assi... Remove this comment to see the full error message
    formatMessage(message);

  log.transports.file.format = (message: Record<string, any>): string => {
    // Debug level logging is recorded as "info" in Daedalus log files
    // but at the same time we do not want to output it to console or terminal window
    const level = message.level === 'debug' ? 'info' : message.level;
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ level: any; }' is not assignab... Remove this comment to see the full error message
    return formatMessage({ ...message, level });
  };

  log.transports.rendererConsole.format = (
    message: Record<string, any>
  ): string => {
    // deconstruct message data
    const date = message.date.toISOString();
    const [year, time] = date.split('T');
    const [context, messageData] = message.data;
    const { message: msg, data = {} } = messageData;
    // log minimal message body in the renderer console
    let messageBody = {
      msg,
      data,
    };

    if (typeof data === 'string') {
      messageBody = {
        ...messageBody,
        data: {
          response: data,
        },
      };
    }

    return `[${year}T${time.slice(0, -1)}Z] ${context} ${stringifyData(
      messageBody
    )}`;
  };

  // Removes existing compressed logs
  fs.readdir(appLogsFolderPath, (err, files) => {
    files.filter(isFileNameWithTimestamp()).forEach((fileName) => {
      const filePath = path.join(appLogsFolderPath, fileName);

      try {
        fs.unlinkSync(filePath);
      } catch (error) {
        // eslint-disable-next-line no-console
        console.error(
          `Compressed log file "${filePath}" deletion failed: ${error}`
        );
      }
    });
  });
};
export const logSystemInfo = (props: LogSystemInfoParams): MessageBody => {
  const { ...data } = props;
  const {
    network,
    osName,
    platformVersion,
    daedalusVersion,
    startTime: at,
  } = data;
  const env = `${network}:${osName}:${platformVersion}`;
  const messageBodyParams: ConstructMessageBodyParams = {
    at,
    env,
    ns: ['daedalus', `v${daedalusVersion}`, `*${network}*`],
    // @ts-ignore ts-migrate(2559) FIXME: Type '{ cardanoNodeVersion: string; cardanoWalletV... Remove this comment to see the full error message
    data,
    msg: 'Updating System-info.json file',
    pid: '',
    sev: 'info',
    thread: '',
  };
  const messageBody: MessageBody = constructMessageBody(messageBodyParams);
  const systemInfoFilePath = path.join(pubLogsFolderPath, 'System-info.json');
  fs.writeFileSync(systemInfoFilePath, JSON.stringify(messageBody));
  return messageBody;
};
export const logStateSnapshot = (
  props: StateSnapshotLogParams
): MessageBody => {
  const { ...data } = props;
  const { currentTime: at, systemInfo, coreInfo } = data;
  const {
    platform,
    platformVersion,
    cpu,
    ram,
    availableDiskSpace,
  } = systemInfo;
  const {
    daedalusVersion,
    daedalusProcessID,
    daedalusMainProcessID,
    isBlankScreenFixActive,
    cardanoNodeVersion,
    cardanoNetwork,
    cardanoNodePID,
    cardanoWalletVersion,
    cardanoWalletPID,
    cardanoWalletApiPort,
    daedalusStateDirectoryPath,
  } = coreInfo;
  const env = `${cardanoNetwork}:${platform}:${platformVersion}`;
  const messageBodyParams: ConstructMessageBodyParams = {
    at,
    env,
    msg: 'Updating State-snapshot.json file',
    pid: '',
    sev: 'info',
    thread: '',
    ns: ['daedalus', `v${daedalusVersion}`, `*${cardanoNetwork}*`],
    platform,
    platformVersion,
    cpu,
    ram,
    availableDiskSpace,
    daedalusVersion,
    daedalusProcessID,
    daedalusMainProcessID,
    isBlankScreenFixActive,
    cardanoNetwork,
    cardanoNodeVersion,
    cardanoNodePID,
    cardanoWalletVersion,
    cardanoWalletPID,
    cardanoWalletApiPort,
    daedalusStateDirectoryPath,
    // @ts-ignore ts-migrate(2559) FIXME: Type '{ systemInfo: SystemInfo; coreInfo: CoreSyst... Remove this comment to see the full error message
    data,
  };
  const messageBody: MessageBody = constructMessageBody(messageBodyParams);
  const stateSnapshotFilePath = path.join(
    pubLogsFolderPath,
    'State-snapshot.json'
  );
  fs.writeFileSync(stateSnapshotFilePath, JSON.stringify(messageBody));
  return messageBody;
};
export const generateWalletMigrationReport = (
  data: WalletMigrationReportData
) => {
  const walletMigrationrReportFilePath = path.join(
    pubLogsFolderPath,
    'Wallet-migration-report.json'
  );
  const generatedAt = new Date().toISOString();
  fs.writeFileSync(
    walletMigrationrReportFilePath,
    JSON.stringify({ ...data, generatedAt })
  );
};
