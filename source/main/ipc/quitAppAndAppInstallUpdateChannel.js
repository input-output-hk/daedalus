// @flow
import { app, shell } from 'electron';
import fs from 'fs';
import shasum from 'shasum';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { QUIT_APP_AND_INSTALL_UPDATE } from '../../common/ipc/api';
import type {
  QuitAppAndAppInstallUpdateRendererRequest,
  QuitAppAndAppInstallUpdateMainResponse,
} from '../../common/ipc/api';
import { environment } from '../environment';

// IpcChannel<Incoming, Outgoing>

const quitAppAndAppInstallUpdateChannel: MainIpcChannel<
  QuitAppAndAppInstallUpdateRendererRequest,
  QuitAppAndAppInstallUpdateMainResponse
> = new MainIpcChannel(QUIT_APP_AND_INSTALL_UPDATE);

export const handleQuitAppAndAppInstallUpdateRequests = () => {
  quitAppAndAppInstallUpdateChannel.onRequest(
    async ({ filePath, directoryPath, hash: expectedHash }) => {
      // For Linux we simply open the installer directory
      if (environment.isLinux) {
        const directoryExists = fs.existsSync(directoryPath);
        if (!directoryExists) return false;
        const openDirectory: boolean = shell.openItem(directoryPath);
        if (openDirectory) app.quit();
        return openDirectory;
      }
      const fileExists = fs.existsSync(filePath);
      if (!fileExists) return false;
      const fileBuffer = fs.readFileSync(filePath);
      if (!fileBuffer) return false;
      const fileHash = shasum(fileBuffer, 'sha256');
      if (fileHash !== expectedHash) return false;
      const openInstaller: boolean = shell.openItem(filePath);
      if (openInstaller) app.quit();
      return openInstaller;
    }
  );
};
