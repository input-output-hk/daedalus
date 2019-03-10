// @flow
import fs from 'fs';
import cbor from 'cbor';
import path from 'path';
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
      const filePath = request.filePath || path.join(appFolderPath, `wallet-${wallet.index}.key`);
      const output = fs.createWriteStream(filePath);
      output.on('close', () => resolve(filePath));
      output.on('error', (error) => reject(error));
      output.write(fileContent);
      output.close();
    })
  ));
};
