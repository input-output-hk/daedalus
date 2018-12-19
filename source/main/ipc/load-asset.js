// @flow
import fs from 'fs';
import path from 'path';
import { LOAD_ASSET_CHANNEL } from '../../common/ipc/load-asset';
import type { LoadAssetRequest, LoadAssetResponse } from '../../common/ipc/load-asset';
import { MainIpcChannel } from './lib/MainIpcChannel';

// IpcChannel<Incoming, Outgoing>

export default () => {
  const loadAssetChannel: MainIpcChannel<LoadAssetRequest, LoadAssetResponse> = (
    new MainIpcChannel(LOAD_ASSET_CHANNEL)
  );
  loadAssetChannel.onReceive((request: LoadAssetRequest) => {
    const asset = path.resolve(__dirname, `../renderer/${request.fileName}`);
    return new Promise((resolve, reject) => (
      fs.readFile(asset, 'base64', (error, data) => { error ? reject(error) : resolve(data); })
    ));
  });
};
