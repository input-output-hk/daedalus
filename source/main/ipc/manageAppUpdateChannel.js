// @flow
import { app, shell } from 'electron';
import fs from 'fs';
import shasum from 'shasum';
import { spawn } from 'child_process';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { MANAGE_APP_UPDATE } from '../../common/ipc/api';
import type {
  ManageAppUpdateRendererRequest as Request,
  ManageAppUpdateMainResponse as Response,
} from '../../common/ipc/api';
import { UPDATE_INSTALLATION_STATUSES as statuses } from '../../common/config/appUpdateConfig';
import { environment } from '../environment';
import { safeExitWithCode } from '../utils/safeExitWithCode';
import { logger } from '../utils/logging';
import { launcherConfig } from '../config';

// IpcChannel<Incoming, Outgoing>

const manageAppUpdateChannel: MainIpcChannel<
  Request,
  Response
> = new MainIpcChannel(MANAGE_APP_UPDATE);

const logPrefix = 'appUpdateInstall';

const getMessage = (functionPrefix: string, message?: string): string => {
  let formattedMessage = `${logPrefix}:${functionPrefix}`;
  if (message) formattedMessage += `: ${message}`;
  return formattedMessage;
};

export const handleManageAppUpdateRequests = (window: BrowserWindow) => {
  const response = (
    success: ?boolean,
    functionPrefix: string,
    messageText?: string = '',
    _data?: Object
  ): Response => {
    let status = statuses.PROGRESS;
    if (success === true) status = statuses.SUCCESS;
    else if (success === false) status = statuses.ERROR;
    const log = success === false ? logger.error : logger.info;
    const message = getMessage(functionPrefix, messageText);
    log(getMessage(functionPrefix, message));
    const data = {
      ..._data,
      message,
    };
    manageAppUpdateChannel.send(
      {
        status,
        message,
        data,
      },
      window.webContents
    );
    return {
      status,
      message,
      data,
    };
  };

  const checkInstallerHash = (filePath, expectedHash): boolean => {
    const { name: functionPrefix } = checkInstallerHash;
    const fileBuffer = fs.readFileSync(filePath);
    if (!fileBuffer) {
      logger.error(getMessage(functionPrefix, 'Unable to read the installer:'));
      return false;
    }
    const fileHash = shasum(fileBuffer, 'sha256');
    if (fileHash !== expectedHash) {
      logger.error(getMessage(functionPrefix, 'Hash does not match'), {
        filePath,
      });
      return false;
    }
    return true;
  };

  const installUpdate = async filePath => {
    return new Promise((resolve, reject) => {
      const { name: functionPrefix } = installUpdate;
      response(null, functionPrefix, 'installation begin.');
      const { updateRunnerBin } = launcherConfig;
      fs.chmodSync(filePath, 0o777);
      const updater = spawn(updateRunnerBin, [filePath]);
      let success = true;
      updater.stdout.on('data', progressData => {
        const info = progressData.toString().split(/\n/);
        const progress = info.reduce((prog, infoItem) => {
          const [, progressStr] = infoItem.split('PROG ');
          if (progressStr) {
            const [item, total] = `${progressStr}`.trim().split('/');
            return parseInt(
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
      updater.stderr.on('data', progressData => {
        response(null, functionPrefix, 'installation progress.', {
          info: progressData.toString(),
        });
      });
      updater.on('close', code => {
        if (code !== 0) {
          success = false;
          logger.info(`updater closed with ${code}`);
          reject(
            response(
              false,
              functionPrefix,
              `updater closed with code ${code}`,
              { code }
            )
          );
        }
        response(null, functionPrefix, 'installation progress.', {
          info: 'stdio closed',
        });
      });
      updater.on('error', error => {
        success = false;
        logger.error(`on error with ${error}`);
        reject(
          response(false, functionPrefix, 'installation failed', { error })
        );
      });
      updater.on('exit', code => {
        if (code !== 0) {
          success = false;
          logger.info(`updater exited with ${code}`);
          reject(
            response(
              false,
              functionPrefix,
              `updater exited with code ${code}`,
              { code }
            )
          );
        }
        if (!success) {
          logger.error('exit without success');
          return reject();
        }
        safeExitWithCode(20);
        return resolve(response(true, functionPrefix));
      });
    });
  };

  manageAppUpdateChannel.onRequest(async ({ filePath, hash: expectedHash }) => {
    const functionPrefix = 'onRequest';

    const fileExists = fs.existsSync(filePath);
    if (!fileExists)
      return response(false, functionPrefix, 'Installer not found:', {
        info: { filePath },
      });

    const installerHash = checkInstallerHash(filePath, expectedHash);
    if (!installerHash) return response(false, functionPrefix);

    // For linux we execute the installer file
    if (environment.isLinux) return installUpdate(filePath);

    // For other OS we launch the installer file after the app was closed
    app.on('quit', () => {
      shell.openItem(filePath);
    });
    app.quit();
    return response(true, functionPrefix);
  });
};
