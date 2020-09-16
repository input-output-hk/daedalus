// @flow
import { app, shell } from 'electron';
import fs from 'fs';
import shasum from 'shasum';
import { spawnSync } from 'child_process';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { QUIT_APP_AND_INSTALL_UPDATE } from '../../common/ipc/api';
import type {
  QuitAppAndAppInstallUpdateRendererRequest as Request,
  QuitAppAndAppInstallUpdateMainResponse as Response,
} from '../../common/ipc/api';
import { environment } from '../environment';
import { logger } from '../utils/logging';

// IpcChannel<Incoming, Outgoing>

const quitAppAndAppInstallUpdateChannel: MainIpcChannel<
  Request,
  Response
> = new MainIpcChannel(QUIT_APP_AND_INSTALL_UPDATE);

const returnError = (message: string, data?: Object): Response => {
  logger.error(message, data);
  return {
    success: false,
    message,
    data,
  };
};

const checkInstallerHash = (filePath, expectedHash): Response => {
  const fileBuffer = fs.readFileSync(filePath);
  if (!fileBuffer)
    return returnError(
      'appUpdateInstall:checkInstallerHash: Unable to read the installer:',
      { filePath }
    );
  const fileHash = shasum(fileBuffer, 'sha256');
  if (fileHash !== expectedHash)
    return returnError(
      'appUpdateInstall:checkInstallerHash: Hash does not match'
    );
  return { success: true };
};

const installUpdate = (filePath): Response => {
  try {
    logger.info('appUpdateInstall: Installing the update', { filePath });
    const { stdout, stderr } = spawnSync(filePath);
    const status = JSON.parse(stdout.toString());
    logger.info('appUpdateInstall:installing:', { status });
    const errors = stderr.toString();
    if (errors) throw new Error(errors);
    return { success: true };
  } catch (errors) {
    return returnError(
      'appUpdateInstall:installing: Error when trying to install the update:',
      { errors }
    );
  }
};

export const handleQuitAppAndAppInstallUpdateRequests = () => {
  quitAppAndAppInstallUpdateChannel.onRequest(
    async ({ filePath, hash: expectedHash }) => {
      const fileExists = fs.existsSync(filePath);
      if (!fileExists)
        return returnError('appUpdateInstall: Installer not found:', {
          filePath,
        });

      const installerHash = checkInstallerHash(filePath, expectedHash);

      if (!installerHash.success)
        return returnError('appUpdateInstall: Invalid hash');

      // For linux we execute the installer file
      if (environment.isLinux) {
        return installUpdate(filePath);
      }

      // For other OS we launch the installer file
      const openInstaller: boolean = shell.openItem(filePath);
      if (!openInstaller) {
        return returnError(
          'appUpdateInstall: Not able to launch the installer'
        );
      }

      app.quit();
      return { success: true };
    }
  );
};
