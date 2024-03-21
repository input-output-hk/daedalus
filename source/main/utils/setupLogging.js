'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.generateWalletMigrationReport = exports.logStateSnapshot = exports.logSystemInfo = exports.setupLogging = void 0;
const fs_1 = __importDefault(require('fs'));
const path_1 = __importDefault(require('path'));
const electron_log_daedalus_1 = __importDefault(
  require('electron-log-daedalus')
);
const rimraf_1 = __importDefault(require('rimraf'));
const ensureDirectoryExists_1 = __importDefault(
  require('./ensureDirectoryExists')
);
const config_1 = require('../config');
const logging_1 = require('../../common/utils/logging');
const files_1 = require('../../common/utils/files');
const isTest = process.env.NODE_ENV === 'test';
const isDev = process.env.NODE_ENV === 'development';
const setupLogging = () => {
  const logFilePath = path_1.default.join(
    config_1.pubLogsFolderPath,
    'Daedalus.json'
  );
  (0, ensureDirectoryExists_1.default)(config_1.pubLogsFolderPath);
  rimraf_1.default.sync(
    path_1.default.join(config_1.pubLogsFolderPath, './Daedalus.*')
  );
  electron_log_daedalus_1.default.transports.console.level = isTest
    ? 'error'
    : 'info';
  electron_log_daedalus_1.default.transports.rendererConsole.level = isDev
    ? 'info'
    : 'error';
  electron_log_daedalus_1.default.transports.file.level = 'debug';
  electron_log_daedalus_1.default.transports.file.maxSize = 5 * 1024 * 1024; // 5MB, unit bytes
  // @ts-ignore ts-migrate(2339) FIXME: Property 'maxItems' does not exist on type 'IFileT... Remove this comment to see the full error message
  electron_log_daedalus_1.default.transports.file.maxItems = 4;
  // @ts-ignore ts-migrate(2339) FIXME: Property 'timeStampPostfixFormat' does not exist o... Remove this comment to see the full error message
  electron_log_daedalus_1.default.transports.file.timeStampPostfixFormat =
    '{y}{m}{d}{h}{i}{s}';
  electron_log_daedalus_1.default.transports.file.file = logFilePath;
  electron_log_daedalus_1.default.transports.console.format = (message) =>
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'Record<string, any>' is not assi... Remove this comment to see the full error message
    (0, logging_1.formatMessage)(message);
  electron_log_daedalus_1.default.transports.file.format = (message) => {
    // Debug level logging is recorded as "info" in Daedalus log files
    // but at the same time we do not want to output it to console or terminal window
    const level = message.level === 'debug' ? 'info' : message.level;
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ level: any; }' is not assignab... Remove this comment to see the full error message
    return (0, logging_1.formatMessage)({ ...message, level });
  };
  electron_log_daedalus_1.default.transports.rendererConsole.format = (
    message
  ) => {
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
    return `[${year}T${time.slice(0, -1)}Z] ${context} ${(0,
    logging_1.stringifyData)(messageBody)}`;
  };
  // Removes existing compressed logs
  fs_1.default.readdir(config_1.appLogsFolderPath, (err, files) => {
    files.filter((0, files_1.isFileNameWithTimestamp)()).forEach((fileName) => {
      const filePath = path_1.default.join(
        config_1.appLogsFolderPath,
        fileName
      );
      try {
        fs_1.default.unlinkSync(filePath);
      } catch (error) {
        // eslint-disable-next-line no-console
        console.error(
          `Compressed log file "${filePath}" deletion failed: ${error}`
        );
      }
    });
  });
};
exports.setupLogging = setupLogging;
const logSystemInfo = (props) => {
  const { ...data } = props;
  const {
    network,
    osName,
    platformVersion,
    daedalusVersion,
    startTime: at,
  } = data;
  const env = `${network}:${osName}:${platformVersion}`;
  const messageBodyParams = {
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
  const messageBody = (0, logging_1.constructMessageBody)(messageBodyParams);
  const systemInfoFilePath = path_1.default.join(
    config_1.pubLogsFolderPath,
    'System-info.json'
  );
  fs_1.default.writeFileSync(systemInfoFilePath, JSON.stringify(messageBody));
  return messageBody;
};
exports.logSystemInfo = logSystemInfo;
const logStateSnapshot = (props) => {
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
  const messageBodyParams = {
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
  const messageBody = (0, logging_1.constructMessageBody)(messageBodyParams);
  const stateSnapshotFilePath = path_1.default.join(
    config_1.pubLogsFolderPath,
    'State-snapshot.json'
  );
  fs_1.default.writeFileSync(
    stateSnapshotFilePath,
    JSON.stringify(messageBody)
  );
  return messageBody;
};
exports.logStateSnapshot = logStateSnapshot;
const generateWalletMigrationReport = (data) => {
  const walletMigrationrReportFilePath = path_1.default.join(
    config_1.pubLogsFolderPath,
    'Wallet-migration-report.json'
  );
  const generatedAt = new Date().toISOString();
  fs_1.default.writeFileSync(
    walletMigrationrReportFilePath,
    JSON.stringify({ ...data, generatedAt })
  );
};
exports.generateWalletMigrationReport = generateWalletMigrationReport;
//# sourceMappingURL=setupLogging.js.map
