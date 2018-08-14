// @flow
import { ipcMain } from 'electron';
import fs from 'fs';
import path from 'path';
import { LOAD_ASSET_CHANNEL } from '../../common/ipc-api/load-asset';
import type { LoadAssetRequest } from '../../common/ipc-api/load-asset';

export default () => {
  ipcMain.on(LOAD_ASSET_CHANNEL, ({ sender }, request: LoadAssetRequest) => {
    const assetPath = path.resolve(__dirname, `../renderer/${request.fileName}`);
    fs.readFile(assetPath, 'base64', (error, data) => {
      if (error) {
        sender.send(LOAD_ASSET_CHANNEL, false, error);
      } else {
        sender.send(LOAD_ASSET_CHANNEL, true, data);
      }
    });
  });
};
