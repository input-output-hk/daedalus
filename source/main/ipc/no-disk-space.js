// @flow
import { ipcMain, BrowserWindow } from 'electron';
import checkDiskSpace from 'check-disk-space';
import { CHECK_DISK_SPACE } from '../../common/ipc-api';
import environment from '../../common/environment';

const cardanoDiskSpaceRequired = 2000000000; // 2Gb

const path = environment.isWindows() ? 'c:' : '/';

const check = async () => {

  const { free: diskSpaceAvailable } = await checkDiskSpace(path);
  const diskSpaceRequired = cardanoDiskSpaceRequired - diskSpaceAvailable;

  return {
    diskSpaceAvailable,
    diskSpaceRequired,
  };
};

export const handleNoDiskSpaceFromMain = async (mainWindow: BrowserWindow) => {
  const response = await check();
  mainWindow.webContents.send(CHECK_DISK_SPACE.SUCCESS, response);
};

export const handleNoDiskSpaceFromRenderer = () => {
  ipcMain.on(CHECK_DISK_SPACE.REQUEST, async (event) => {
    const response = await check();
    setTimeout(() => {
      event.sender.send(CHECK_DISK_SPACE.SUCCESS, response);
    }, 2000);
  });
};
