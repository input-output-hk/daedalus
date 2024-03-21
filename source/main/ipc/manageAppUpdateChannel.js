'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleManageAppUpdateRequests = void 0;
const electron_1 = require('electron');
const fs_1 = __importDefault(require('fs'));
const shasum_1 = __importDefault(require('shasum'));
const child_process_1 = require('child_process');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
const appUpdateConfig_1 = require('../../common/config/appUpdateConfig');
const environment_1 = require('../environment');
const safeExitWithCode_1 = require('../utils/safeExitWithCode');
const logging_1 = require('../utils/logging');
const config_1 = require('../config');
// IpcChannel<Incoming, Outgoing>
const manageAppUpdateChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.MANAGE_APP_UPDATE
);
const logPrefix = 'appUpdateInstall';
const getMessage = (functionPrefix, message) => {
  let formattedMessage = `${logPrefix}:${functionPrefix}`;
  if (message) formattedMessage += `: ${message}`;
  return formattedMessage;
};
const handleManageAppUpdateRequests = (window) => {
  const response = (success, functionPrefix, messageText = '', _data = {}) => {
    let status = appUpdateConfig_1.UPDATE_INSTALLATION_STATUSES.PROGRESS;
    if (success === true)
      status = appUpdateConfig_1.UPDATE_INSTALLATION_STATUSES.SUCCESS;
    else if (success === false)
      status = appUpdateConfig_1.UPDATE_INSTALLATION_STATUSES.ERROR;
    const log =
      success === false ? logging_1.logger.error : logging_1.logger.info;
    const message = getMessage(functionPrefix, messageText);
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    log(getMessage(functionPrefix, message));
    const data = { ..._data, message };
    manageAppUpdateChannel.send(
      {
        status,
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ status: UpdateInstallationStat... Remove this comment to see the full error message
        message,
        data,
      },
      window.webContents
    );
    return {
      status,
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ status: UpdateInstallationStatus; message:... Remove this comment to see the full error message
      message,
      data,
    };
  };
  const checkInstallerHash = (filePath, expectedHash) => {
    const { name: functionPrefix } = checkInstallerHash;
    const fileBuffer = fs_1.default.readFileSync(filePath);
    if (!fileBuffer) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.error(
        getMessage(functionPrefix, 'Unable to read the installer:')
      );
      return false;
    }
    const fileHash = (0, shasum_1.default)(fileBuffer, 'sha256');
    if (fileHash !== expectedHash) {
      logging_1.logger.error(
        getMessage(functionPrefix, 'Hash does not match'),
        {
          filePath,
        }
      );
      return false;
    }
    return true;
  };
  const installUpdate = async (filePath) => {
    return new Promise((resolve, reject) => {
      const { name: functionPrefix } = installUpdate;
      response(null, functionPrefix, 'installation begin.');
      const { updateRunnerBin } = config_1.launcherConfig;
      fs_1.default.chmodSync(filePath, 0o777);
      const updater = (0, child_process_1.spawn)(updateRunnerBin, [filePath]);
      let success = true;
      updater.stdout.on('data', (progressData) => {
        const info = progressData.toString().split(/\n/);
        const progress = info.reduce((prog, infoItem) => {
          const [, progressStr] = infoItem.split('PROG ');
          if (progressStr) {
            const [item, total] = `${progressStr}`.trim().split('/');
            return parseInt(
              // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
              (parseInt(item, 10) * 100) / parseInt(total, 10),
              10
            );
          }
          return prog;
        }, 0);
        response(null, functionPrefix, 'installation progress.', {
          info,
          progress,
        });
      });
      updater.stderr.on('data', (progressData) => {
        response(null, functionPrefix, 'installation progress.', {
          info: progressData.toString(),
        });
      });
      updater.on('close', (code) => {
        if (code !== 0) {
          success = false;
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info(`updater closed with ${code}`);
          reject(
            response(
              false,
              functionPrefix,
              `updater closed with code ${code}`,
              {
                code,
              }
            )
          );
        }
        response(null, functionPrefix, 'installation progress.', {
          info: 'stdio closed',
        });
      });
      updater.on('error', (error) => {
        success = false;
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.error(`on error with ${error}`);
        reject(
          response(false, functionPrefix, 'installation failed', {
            error,
          })
        );
      });
      updater.on('exit', (code) => {
        if (code !== 0) {
          success = false;
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info(`updater exited with ${code}`);
          reject(
            response(
              false,
              functionPrefix,
              `updater exited with code ${code}`,
              {
                code,
              }
            )
          );
        }
        if (!success) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.error('exit without success');
          return reject();
        }
        (0, safeExitWithCode_1.safeExitWithCode)(20);
        return resolve(response(true, functionPrefix));
      });
    });
  };
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ filePath, hash: expectedHash ... Remove this comment to see the full error message
  manageAppUpdateChannel.onRequest(async ({ filePath, hash: expectedHash }) => {
    const functionPrefix = 'onRequest';
    const fileExists = fs_1.default.existsSync(filePath);
    if (!fileExists)
      return response(false, functionPrefix, 'Installer not found:', {
        info: {
          filePath,
        },
      });
    const installerHash = checkInstallerHash(filePath, expectedHash);
    if (!installerHash) return response(false, functionPrefix);
    // For linux we execute the installer file
    if (environment_1.environment.isLinux) return installUpdate(filePath);
    // For other OS we launch the installer file after the app was closed
    electron_1.app.on('quit', () => {
      electron_1.shell.openPath(filePath);
    });
    electron_1.app.quit();
    return response(true, functionPrefix);
  });
};
exports.handleManageAppUpdateRequests = handleManageAppUpdateRequests;
//# sourceMappingURL=manageAppUpdateChannel.js.map
