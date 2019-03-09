// @flow
import fs from 'fs';
import cbor from 'cbor';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { ExtractWalletsChannelName } from '../../common/ipc/api';
import type {
  ExtractWalletsRendererRequest,
  ExtractWalletsMainResponse
} from '../../common/ipc/api';

export const extractWalletsChannel: (
  // IpcChannel<Incoming, Outgoing>
  MainIpcChannel<ExtractWalletsRendererRequest, ExtractWalletsMainResponse>
) = (
  new MainIpcChannel(ExtractWalletsChannelName)
);

export const handleExtractWalletsRequests = () => {
  extractWalletsChannel.onReceive((request: ExtractWalletsRendererRequest) => (
    new Promise((resolve, reject) => (
      fs.readFile(request.secretKeyFilePath, (error, data) => {
        if (error) reject(error);

        const binaryString = Buffer.from(data);
        const decodedSecrets = cbor.decode(binaryString);
        const keys = decodedSecrets[2];
        const wallets = [];
        keys.forEach((key) => {
          wallets.push({
            raw: key,
            passwordHash: key[1].toString('ascii'),
            password: null,
            balance: null,
          });
        });

        resolve(wallets);
      })
    ))
  ));
};
