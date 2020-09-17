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
import { launcherConfig } from '../config';

const { updateRunnerBin } = launcherConfig;

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

const response = (
  success: boolean,
  functionPrefix: string,
  message?: string = '',
  data?: any
): Response => {
  const log = success ? logger.info : logger.error;
  log(getMessage(functionPrefix, message));
  return {
    success,
    message,
    data,
  };
};

const checkInstallerHash = (filePath, expectedHash): Response => {
  const { name: functionPrefix } = checkInstallerHash;
  const fileBuffer = fs.readFileSync(filePath);
  if (!fileBuffer)
    return response(false, functionPrefix, 'Unable to read the installer:', {
      filePath,
    });
  const fileHash = shasum(fileBuffer, 'sha256');
  if (fileHash !== expectedHash)
    return response(false, functionPrefix, 'Hash does not match', { filePath });
  return response(true, functionPrefix);
};

const installUpdate = async (filePath): Promise<Response> => {
  const { name: functionPrefix } = installUpdate;
  logger.info(getMessage(functionPrefix, 'installation begin.'));
  fs.chmodSync(filePath, 0o777);
  const { error } = await spawnSync(updateRunnerBin, [filePath]);
  if (error)
    return response(false, functionPrefix, 'Unable to install the update', {
      error,
    });
  return response(true, functionPrefix, 'installation success.');
};

export const handleQuitAppAndAppInstallUpdateRequests = () => {
  quitAppAndAppInstallUpdateChannel.onRequest(
    async ({ filePath, hash: expectedHash }) => {
      const functionPrefix = 'onRequest';
      const fileExists = fs.existsSync(filePath);
      if (!fileExists)
        return response(false, functionPrefix, 'Installer not found:', {
          filePath,
        });

      const installerHash = checkInstallerHash(filePath, expectedHash);

      if (!installerHash.success) return installerHash;

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
      return { success: true };
    }
  );
};
