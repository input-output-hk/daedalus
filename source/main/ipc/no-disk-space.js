// @flow
import { ipcMain, BrowserWindow } from 'electron';
import { CHECK_DISK_SPACE } from '../../common/ipc-api';


const DUMMY_CHECKING = [
  {
    diskSpaceRequired: 10123456,
    noDiskSpace: true
  },
  {
    diskSpaceRequired: 3123456,
    noDiskSpace: true
  },
  {
    diskSpaceRequired: 1123456,
    noDiskSpace: true
  },
  {
    diskSpaceRequired: 50,
    noDiskSpace: true
  },
  {
    diskSpaceRequired: 0,
    noDiskSpace: false
  },

];

let DUMMY_CHECKING_INDEX = 0;
const checkDiskSpace = async () => {
  const response = DUMMY_CHECKING[DUMMY_CHECKING_INDEX++];
  return response;
};

export const handleNoDiskSpace = async (mainWindow: BrowserWindow) => {
  const response = await checkDiskSpace();
  mainWindow.webContents.send(CHECK_DISK_SPACE.SUCCESS, response);
};

export default () => {
  ipcMain.on(CHECK_DISK_SPACE.REQUEST, async (event) => {
    const response = await checkDiskSpace();
    setTimeout(() => {
      event.sender.send(CHECK_DISK_SPACE.SUCCESS, response);
    }, 2000);
  });
};
