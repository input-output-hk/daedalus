// @flow
import fs from 'fs';
import path from 'path';
import { LOAD_ASSET_CHANNEL } from '../../common/ipc/channels';
import type { LoadAssetRequest} from '../../common/ipc/contracts';
import { MainIpcChannel } from './lib/MainIpcChannel';
import type { LoadAssetResponse } from '../../common/ipc/contracts';

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
