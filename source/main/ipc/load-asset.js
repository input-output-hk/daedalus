// @flow
import fs from 'fs';
import path from 'path';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { LoadAssetChannelName } from '../../common/ipc/api';
import type {
  LoadAssetRendererRequest,
  LoadAssetMainResponse
} from '../../common/ipc/api';

export default () => {
  // IpcChannel<Incoming, Outgoing>
  const loadAssetChannel: (
    MainIpcChannel<LoadAssetRendererRequest, LoadAssetMainResponse>
  ) = (new MainIpcChannel(LoadAssetChannelName));

  loadAssetChannel.onReceive((request: LoadAssetRendererRequest) => {
    const asset = path.resolve(__dirname, `../renderer/${request.fileName}`);
    return new Promise((resolve, reject) => (
      fs.readFile(asset, 'base64', (error, data) => { error ? reject(error) : resolve(data); })
    ));
  });
};
