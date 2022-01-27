import fs from 'fs';
import path from 'path';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { LOAD_ASSET_CHANNEL } from '../../common/ipc/api';
import type {
  LoadAssetRendererRequest,
  LoadAssetMainResponse,
} from '../../common/ipc/api';

const loadAssetChannel: MainIpcChannel<
  LoadAssetRendererRequest,
  LoadAssetMainResponse
> = new MainIpcChannel(LOAD_ASSET_CHANNEL);
export default () => {
  loadAssetChannel.onRequest((request: LoadAssetRendererRequest) => {
    const asset = path.resolve(__dirname, `../renderer/${request.fileName}`);
    return new Promise((resolve, reject) =>
      fs.readFile(asset, 'base64', (error, data) => {
        if (error) {
          reject(error);
        } else {
          resolve(data);
        }
      })
    );
  });
};
