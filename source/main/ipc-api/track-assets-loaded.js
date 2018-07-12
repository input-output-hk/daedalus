// @flow
import { ipcMain } from 'electron';
import { ASSETS_LOADED } from '../../common/ipc-api/load-asset';

export default ({ window }: { window: any }) => {
  ipcMain.on(ASSETS_LOADED.SUCCESS, () => {
    clearTimeout(window.checkAssetsFilesWereLoaded);
  });
};
