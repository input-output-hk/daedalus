// @flow
import { app, shell } from 'electron';
import fs from 'fs';
import shasum from 'shasum';
import { spawnSync } from 'child_process';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { QUIT_APP_AND_INSTALL_UPDATE } from '../../common/ipc/api';
import type {
  QuitAppAndAppInstallUpdateRendererRequest,
  QuitAppAndAppInstallUpdateMainResponse,
} from '../../common/ipc/api';
import { environment } from '../environment';
import { logger } from '../utils/logging';

// IpcChannel<Incoming, Outgoing>

const quitAppAndAppInstallUpdateChannel: MainIpcChannel<
  QuitAppAndAppInstallUpdateRendererRequest,
  QuitAppAndAppInstallUpdateMainResponse
> = new MainIpcChannel(QUIT_APP_AND_INSTALL_UPDATE);

const checkInstallerHash = (filePath, expectedHash): boolean => {
  const fileExists = fs.existsSync(filePath);
  if (!fileExists) return false;
  const fileBuffer = fs.readFileSync(filePath);
  if (!fileBuffer) return false;
  const fileHash = shasum(fileBuffer, 'sha256');
  if (fileHash !== expectedHash) return false;
  return true;
};

const installUpdate = (filePath): boolean => {
  logger.info('quitAppInstallUpdate: Installing the update', { filePath });
  const { stdout, stderr } = spawnSync(filePath);
  const status = JSON.parse(stdout.toString());
  logger.info('quitAppInstallUpdate:installing:', { status });
  const errors = stderr.toString();
  if (errors) {
    logger.error(
      'quitAppInstallUpdate:installing: Error when trying to install the update:',
      { errors }
    );
  }
  return !errors;
};

export const handleQuitAppAndAppInstallUpdateRequests = () => {
  quitAppAndAppInstallUpdateChannel.onRequest(
    async ({ filePath, hash: expectedHash }) => {
      if (checkInstallerHash(filePath, expectedHash)) return false;
      const fileExists = fs.existsSync(filePath);
      if (!fileExists) {
        logger.error('quitAppInstallUpdate: Installer not found:', {
          filePath,
        });
        return false;
      }

      // For linux we execute the installer file
      if (environment.isLinux) {
        return installUpdate(filePath);
      }

      // For other OS we launch the installer file
      const openInstaller: boolean = shell.openItem(filePath);
      if (!openInstaller) {
        logger.error('quitAppInstallUpdate: Installer not found:', {
          filePath,
        });
      }
      return openInstaller;
    }
  );
};
