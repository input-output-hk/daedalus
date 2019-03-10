// @flow
import fs from 'fs';
import cbor from 'cbor';
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
    const wus = []; // the WalletUserSecret
    wus[0] = wallet.raw;
    wus[1] = 'Imported Wallet';
    wus[2] = []; // accounts
    wus[3] = []; // addresses
    const newSecrets = cbor.encode([[], [], [], [wus]]);
    return newSecrets; // the UserSecret
  };

  downloadKeyFileChannel.onReceive((request: DownloadKeyFileRendererRequest) => (
    new Promise((resolve, reject) => {
      const { wallet, filePath } = request;
      const fileContent = extractKey(wallet);
      const output = fs.createWriteStream(filePath);
      output.on('close', () => resolve());
      output.on('error', (error) => reject(error));
      output.write(fileContent);
      output.close();
    })
  ));
};
