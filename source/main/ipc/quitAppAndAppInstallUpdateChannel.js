// @flow
import { app, shell } from 'electron';
import fs from 'fs';
import shasum from 'shasum';
import { spawnSync } from 'child_process';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { QUIT_APP_AND_INSTALL_UPDATE } from '../../common/ipc/api';
import type {
  QuitAppAndAppInstallUpdateRendererRequest as Request,
  QuitAppAndAppInstallUpdateMainResponse as Response,
} from '../../common/ipc/api';
import { environment } from '../environment';
import { logger } from '../utils/logging';
import { launcherConfig } from '../config';

// IpcChannel<Incoming, Outgoing>

const quitAppAndAppInstallUpdateChannel: MainIpcChannel<
  Request,
  Response
> = new MainIpcChannel(QUIT_APP_AND_INSTALL_UPDATE);

const logPrefix = 'appUpdateInstall';

const getMessage = (functionPrefix: string, message?: string): string => {
  let formattedMessage = `${logPrefix}:${functionPrefix}`;
  if (message) formattedMessage += `: ${message}`;
  return formattedMessage;
};

export const handleQuitAppAndAppInstallUpdateRequests = (
  window: BrowserWindow
) => {
  const response = (
    success: ?boolean,
    functionPrefix: string,
    messageText?: string = '',
    data?: any
  ): Response => {
    let status = 'progress';
    if (success === true) status = 'success';
    else if (success === false) status = 'error';
    const log = success === false ? logger.error : logger.info;
    const message = getMessage(functionPrefix, messageText);
    log(getMessage(functionPrefix, message));
    quitAppAndAppInstallUpdateChannel.send(
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
    const { name: functionPrefix } = installUpdate;
    response(null, functionPrefix, 'installation begin.');
    // logger.info(getMessage(functionPrefix, 'installation begin.'));
    const { updateRunnerBin } = launcherConfig;
    fs.chmodSync(filePath, 0o777);
    const { error } = await spawnSync(updateRunnerBin, [filePath]);
    if (error)
      return response(false, functionPrefix, 'Unable to install the update', {
        error: error.toString(),
      });
    return response(true, functionPrefix, 'installation success.');
  };

  quitAppAndAppInstallUpdateChannel.onRequest(
    async ({ filePath, hash: expectedHash }) => {
      const functionPrefix = 'onRequest';

      const fileExists = fs.existsSync(filePath);
      if (!fileExists)
        return response(false, functionPrefix, 'Installer not found:', {
          filePath,
        });

      const installerHash = checkInstallerHash(filePath, expectedHash);
      if (!installerHash) return response(false, functionPrefix);

      // For linux we execute the installer file
      if (environment.isLinux) return installUpdate(filePath);

      // For other OS we launch the installer file
      const openInstaller: boolean = shell.openItem(filePath);
      if (!openInstaller)
        return response(
          false,
          functionPrefix,
          'Not able to launch the installer'
        );

      app.quit();
      return response(true, functionPrefix);
    }
  );
};
