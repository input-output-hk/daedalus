// @flow
import { BrowserWindow } from 'electron';
import checkDiskSpace from 'check-disk-space';
import { DISK_SPACE_STATUS_CHANNEL } from '../../common/ipc/check-disk-space';
import environment from '../../common/environment';
import { Logger } from '../../common/logging';

export default (
  mainWindow: BrowserWindow,
  onCheckDiskSpace?: Function,
) => {

  const DISK_SPACE_REQUIRED = 2000000000; // 2Gb
  const DISK_SPACE_REQUIRED_MARGIN =
    DISK_SPACE_REQUIRED - (DISK_SPACE_REQUIRED * 10 / 100); // 2Gb - 10%
  const DISK_SPACE_CHECK_LONG_INTERVAL = 600000; // 10 minutes
  const DISK_SPACE_CHECK_SHORT_INTERVAL = 10000; // 10 seconds

  const path = environment.isWindows() ? 'c:' : '/';
  let diskSpaceCheckInterval;
  let notEnoughSpace = false;

  const handleCheckDiskSpace = async (action: string) => {

    const { free: diskSpaceAvailable } = await checkDiskSpace(path);
    let diskSpaceMissing = DISK_SPACE_REQUIRED - diskSpaceAvailable;
    diskSpaceMissing = diskSpaceMissing > -1 ? diskSpaceMissing : 0;

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
      action,
      diskSpaceAvailable,
      diskSpaceRequired: DISK_SPACE_REQUIRED,
      notEnoughSpace,
      diskSpaceMissing,
    };

    const isNotEnoughSpace = notEnoughSpace ? 'yes' : 'no';

    Logger.info(`Action: ${action}. n.e.s: ${isNotEnoughSpace}. d.s.a.: ${diskSpaceAvailable}`);

    if (typeof onCheckDiskSpace === 'function') {
      onCheckDiskSpace(response);
    }
    mainWindow.webContents.send(DISK_SPACE_STATUS_CHANNEL, response);
  };

  const setDiskSpaceIntervalChecking = (interval) => {
    clearInterval(diskSpaceCheckInterval);
    diskSpaceCheckInterval =
      setInterval(async () => {
        let action;
        if (interval === DISK_SPACE_CHECK_LONG_INTERVAL) {
          action = 'Long interval disk space check';
        } else {
          action = 'Short interval disk space check';
        }
        await handleCheckDiskSpace(action);
      }, interval);
  };
  setDiskSpaceIntervalChecking(DISK_SPACE_CHECK_LONG_INTERVAL);

  setTimeout(async () => {
    const action = 'Initial disk space check';
    await handleCheckDiskSpace(action);
  }, 5000);

  return handleCheckDiskSpace;

};

