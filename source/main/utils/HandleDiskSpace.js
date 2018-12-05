// @flow
import { BrowserWindow, ipcMain } from 'electron';
import checkDiskSpace from 'check-disk-space';
import prettysize from 'prettysize';
import { GET_DISK_SPACE_STATUS } from '../../common/ipc-api';
import environment from '../../common/environment';
import { Logger } from '../../common/logging';

export type CheckDiskSpaceResponse = {
  notEnoughSpace: boolean,
  diskSpaceRequired: string,
  diskSpaceMissing: string,
  diskSpaceRecommended: string,
};

export default (
  mainWindow: BrowserWindow,
  onCheckDiskSpace?: Function,
) => {

  const DISK_SPACE_REQUIRED = 2147483648; // 2Gb
  const DISK_SPACE_REQUIRED_MARGIN =
    DISK_SPACE_REQUIRED - (DISK_SPACE_REQUIRED * 10 / 100); // 2Gb - 10%
  const DISK_SPACE_CHECK_LONG_INTERVAL = 600000; // 10 minutes
  const DISK_SPACE_CHECK_SHORT_INTERVAL = 10000; // 10 seconds
  const DISK_SPACE_RECOMMENDED_PERCENTAGE = 15; // 15% of the total disk space

  const path = environment.isWindows() ? 'C:' : '/';
  let diskSpaceCheckInterval;
  let notEnoughSpace = false;

  const handleCheckDiskSpace = async () => {

    const diskSpaceRequired = prettysize(DISK_SPACE_REQUIRED);
    const { free: diskSpaceAvailable, size: diskTotalSpace } = await checkDiskSpace(path);
    let diskSpaceMissing = DISK_SPACE_REQUIRED - diskSpaceAvailable;
    diskSpaceMissing = diskSpaceMissing > -1 ? prettysize(diskSpaceMissing) : '0';
    const diskSpaceRecommended =
      prettysize(diskTotalSpace * DISK_SPACE_RECOMMENDED_PERCENTAGE / 100);

    if (diskSpaceAvailable <= DISK_SPACE_REQUIRED_MARGIN) {
      if (!notEnoughSpace) {
        setDiskSpaceIntervalChecking(DISK_SPACE_CHECK_SHORT_INTERVAL);
      }
      notEnoughSpace = true;
    } else if (diskSpaceAvailable >= DISK_SPACE_REQUIRED) {
      if (notEnoughSpace) {
        setDiskSpaceIntervalChecking(DISK_SPACE_CHECK_LONG_INTERVAL);
      }
      notEnoughSpace = false;
    }
    const response = {
      notEnoughSpace,
      diskSpaceRequired,
      diskSpaceMissing,
      diskSpaceRecommended,
    };

    Logger.info(JSON.stringify(response, null, 2));

    if (typeof onCheckDiskSpace === 'function') {
      onCheckDiskSpace(response);
    }
    mainWindow.webContents.send(GET_DISK_SPACE_STATUS.SUCCESS, response);
  };

  const setDiskSpaceIntervalChecking = (interval) => {
    clearInterval(diskSpaceCheckInterval);
    diskSpaceCheckInterval =
      setInterval(async () => {
        handleCheckDiskSpace();
      }, interval);
  };
  setDiskSpaceIntervalChecking(DISK_SPACE_CHECK_LONG_INTERVAL);

  ipcMain.on(GET_DISK_SPACE_STATUS.REQUEST, handleCheckDiskSpace);

  return handleCheckDiskSpace;

};
