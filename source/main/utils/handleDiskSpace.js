// @flow
import { BrowserWindow } from 'electron';
import checkDiskSpace from 'check-disk-space';
import prettysize from 'prettysize';
import { getDiskSpaceStatusChannel } from '../ipc/get-disk-space-status';
import { environment } from '../environment';
import { Logger } from './logging';
import {
  DISK_SPACE_REQUIRED,
  DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE,
  DISK_SPACE_CHECK_LONG_INTERVAL,
  DISK_SPACE_CHECK_SHORT_INTERVAL,
  DISK_SPACE_RECOMMENDED_PERCENTAGE
} from '../config';

export const handleDiskSpace = (
  mainWindow: BrowserWindow,
  onCheckDiskSpace?: Function,
) => {
  const { isWindows, isTest } = environment;
  const path = isWindows ? 'C:' : '/';
  let diskSpaceCheckInterval;

  const handleCheckDiskSpace = async (forceDiskSpaceRequired?: number) => {
    const diskSpaceRequired = forceDiskSpaceRequired || DISK_SPACE_REQUIRED;
    const { free: diskSpaceAvailable, size: diskTotalSpace } = await checkDiskSpace(path);
    const diskSpaceMissing = Math.max((diskSpaceRequired - diskSpaceAvailable), 0);
    const diskSpaceRecommended =
      diskTotalSpace * DISK_SPACE_RECOMMENDED_PERCENTAGE / 100;
    const diskSpaceRequiredMargin =
      diskSpaceRequired - (diskSpaceRequired * DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE / 100);

    let isNotEnoughDiskSpace = false;
    if (diskSpaceAvailable <= diskSpaceRequiredMargin) {
      if (!isNotEnoughDiskSpace) {
        setDiskSpaceIntervalChecking(
          isTest ? 1000 : DISK_SPACE_CHECK_SHORT_INTERVAL
        );
      }
      isNotEnoughDiskSpace = true;
    } else if (diskSpaceAvailable >= diskSpaceRequired) {
      if (isNotEnoughDiskSpace) setDiskSpaceIntervalChecking(DISK_SPACE_CHECK_LONG_INTERVAL);
      isNotEnoughDiskSpace = false;
    }

    const response = {
      isNotEnoughDiskSpace,
      diskSpaceRequired: prettysize(diskSpaceRequired),
      diskSpaceMissing: prettysize(diskSpaceMissing),
      diskSpaceRecommended: prettysize(diskSpaceRecommended),
    };

    if (isNotEnoughDiskSpace) Logger.info(JSON.stringify(response, null, 2));

    if (typeof onCheckDiskSpace === 'function') onCheckDiskSpace(response);

    getDiskSpaceStatusChannel.send(response, mainWindow.webContents);
    return response;
  };

  const setDiskSpaceIntervalChecking = (interval) => {
    clearInterval(diskSpaceCheckInterval);
    diskSpaceCheckInterval =
      setInterval(async () => {
        handleCheckDiskSpace();
      }, interval);
  };
  setDiskSpaceIntervalChecking(DISK_SPACE_CHECK_LONG_INTERVAL);

  getDiskSpaceStatusChannel.onReceive(diskSpaceRequired => handleCheckDiskSpace(diskSpaceRequired));

  return handleCheckDiskSpace;
};
