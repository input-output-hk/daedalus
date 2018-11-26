// @flow
import { BrowserWindow } from 'electron';
import checkDiskSpace from 'check-disk-space';
import { DISK_SPACE_STATUS_CHANNEL } from '../../common/ipc/check-disk-space';
import environment from '../../common/environment';

const DISK_SPACE_REQUIRED = 2000000000; // 2Gb
const DISK_SPACE_REQUIRED_MARGIN =
  DISK_SPACE_REQUIRED - (DISK_SPACE_REQUIRED * 10 / 100); // 2Gb - 10%
const DISK_SPACE_CHECK_LONG_INTERVAL = 600000; // 10 minutes
const DISK_SPACE_CHECK_SHORT_INTERVAL = 10000; // 10 seconds

const path = environment.isWindows() ? 'c:' : '/';
let diskSpaceCheckInterval;
let notEnoughSpace = false;

const handleCheckDiskSpace = async () => {

  const { free: diskSpaceAvailable } = await checkDiskSpace(path);
  const diskSpaceMissing = DISK_SPACE_REQUIRED - diskSpaceAvailable;

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

  return {
    diskSpaceAvailable,
    diskSpaceRequired: DISK_SPACE_REQUIRED,
    notEnoughSpace,
    diskSpaceMissing,
  };
};

const setDiskSpaceIntervalChecking = (interval) => {
  clearInterval(diskSpaceCheckInterval);
  diskSpaceCheckInterval =
    setInterval(() => handleCheckDiskSpace(), interval);
};
setDiskSpaceIntervalChecking(DISK_SPACE_CHECK_LONG_INTERVAL);
handleCheckDiskSpace();

export const onNoDiskSpaceError = async (mainWindow: BrowserWindow) => {
  const response = await handleCheckDiskSpace();
  mainWindow.webContents.send(DISK_SPACE_STATUS_CHANNEL, response);
};
