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

// IpcChannel<Incoming, Outgoing>

const quitAppAndAppInstallUpdateChannel: MainIpcChannel<
  QuitAppAndAppInstallUpdateRendererRequest,
  QuitAppAndAppInstallUpdateMainResponse
> = new MainIpcChannel(QUIT_APP_AND_INSTALL_UPDATE);

export const handleQuitAppAndAppInstallUpdateRequests = () => {
  quitAppAndAppInstallUpdateChannel.onRequest(async () => {
    // const { hash } = this.getUpdateInfo(this.availableUpdate);
    // const filePath = `${destinationPath}/${originalFilename}`;
    const { filePath, hash: expectedHash } = {};
    if (!filePath || !expectedHash) return false;
    const fileBuffer = fs.readFileSync(filePath);
    if (!fileBuffer) return false;
    const fileHash = shasum(fileBuffer, 'sha256');
    if (fileHash !== expectedHash) return false;
    const openInstaller: boolean = shell.openItem(filePath);
    if (openInstaller) app.quit();
    return openInstaller;
  });
};
