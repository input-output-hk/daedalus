// @flow
import { ipcMain, BrowserWindow } from 'electron';
import checkDiskSpace from 'check-disk-space';
import { CHECK_DISK_SPACE } from '../../common/ipc-api';
import environment from '../../common/environment';

const cardanoDiskSpaceRequired = 10123456;

const DUMMY_FREE_DISK_SPACE = [50, 3454, 1123456, 3123456, 11123456];

const path = environment.isWindows() ? 'c:' : '/';
let DUMMY_CHECKING_INDEX = 0;

const check = async () => {

  // const { free:diskSpaceAvailable /* , size */ } = await checkDiskSpace(path);
  const { free } = await checkDiskSpace(path);
  console.log('free', free);
  const diskSpaceAvailable = DUMMY_FREE_DISK_SPACE[DUMMY_CHECKING_INDEX++];
  const diskSpaceRequired = cardanoDiskSpaceRequired - diskSpaceAvailable;

  return {
    diskSpaceAvailable,
    diskSpaceRequired,
  };
};

export const handleNoDiskSpace = async (mainWindow: BrowserWindow) => {
  const response = await check();
  mainWindow.webContents.send(CHECK_DISK_SPACE.SUCCESS, response);
};

export default () => {
  ipcMain.on(CHECK_DISK_SPACE.REQUEST, async (event) => {
    const response = await check();
    setTimeout(() => {
      event.sender.send(CHECK_DISK_SPACE.SUCCESS, response);
    }, 2000);
  });
};
