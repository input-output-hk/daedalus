// @flow
import fs from 'fs';
import cbor from 'cbor';
import path from 'path';
import rimraf from 'rimraf';
import ensureDirectoryExists from '../utils/ensureDirectoryExists';
import { appFolderPath } from '../config';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { DownloadKeyFileChannelName } from '../../common/ipc/api';
import type {
  DownloadKeyFileRendererRequest,
  DownloadKeyFileMainResponse,
} from '../../common/ipc/api';
import type { ExtractedWallet } from '../../common/types/wallet-importer.types';

export const downloadKeyFileChannel: (
  // IpcChannel<Incoming, Outgoing>
  MainIpcChannel<DownloadKeyFileRendererRequest, DownloadKeyFileMainResponse>
) = (
  new MainIpcChannel(DownloadKeyFileChannelName)
);

export const handleDownloadKeyFileRequests = () => {
  const tempKeysFolder = path.join(appFolderPath, 'temp-keys');
  ensureDirectoryExists(tempKeysFolder);

  const extractKey = (wallet: ExtractedWallet) => {
    const wus = []; // WalletUserSecret
    wus[0] = wallet.raw;
    wus[1] = 'Imported Wallet';
    wus[2] = []; // Accounts
    wus[3] = []; // Addresses
    const newSecrets = cbor.encode([[], [], [], [wus]]);
    return newSecrets; // UserSecret
  };

  downloadKeyFileChannel.onReceive((request: DownloadKeyFileRendererRequest) => (
    new Promise((resolve, reject) => {
      const { wallet } = request;
      const fileContent = extractKey(wallet);
      const filePath = request.filePath || path.join(tempKeysFolder, `wallet-${wallet.index}.key`);
      if (filePath !== request.filePath) {
        rimraf.sync(tempKeysFolder);
        ensureDirectoryExists(tempKeysFolder);
      }
      const output = fs.createWriteStream(filePath);
      output.on('close', () => resolve(filePath));
      output.on('error', (error) => reject(error));
      output.write(fileContent);
      output.close();
    })
  ));
};
