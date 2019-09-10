// @flow
import { BrowserWindow } from 'electron';
import checkDiskSpace from 'check-disk-space';
import prettysize from 'prettysize';
import { getDiskSpaceStatusChannel } from '../ipc/get-disk-space-status';
import { Logger } from './logging';
import {
  DISK_SPACE_REQUIRED,
  DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE,
  DISK_SPACE_CHECK_LONG_INTERVAL,
  DISK_SPACE_CHECK_MEDIUM_INTERVAL,
  DISK_SPACE_CHECK_SHORT_INTERVAL,
  DISK_SPACE_RECOMMENDED_PERCENTAGE,
  stateDrive,
} from '../config';

export const handleDiskSpace = (
  mainWindow: BrowserWindow,
  onCheckDiskSpace?: Function
) => {
  let diskSpaceCheckInterval;
  let diskSpaceCheckIntervalLength = DISK_SPACE_CHECK_LONG_INTERVAL; // Default check interval
  let isNotEnoughDiskSpace = false; // Default check state

  const handleCheckDiskSpace = async (forceDiskSpaceRequired?: number) => {
    const diskSpaceRequired = forceDiskSpaceRequired || DISK_SPACE_REQUIRED;
    try {
      const {
        free: diskSpaceAvailable,
        size: diskTotalSpace,
      } = await checkDiskSpace(stateDrive);
      const diskSpaceMissing = Math.max(
        diskSpaceRequired - diskSpaceAvailable,
        0
      );
      const diskSpaceRecommended =
        (diskTotalSpace * DISK_SPACE_RECOMMENDED_PERCENTAGE) / 100;
      const diskSpaceRequiredMargin =
        diskSpaceRequired -
        (diskSpaceRequired * DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE) / 100;

      if (diskSpaceAvailable <= diskSpaceRequiredMargin) {
        if (!isNotEnoughDiskSpace) {
          // State change: transitioning from enough to not-enough disk space
          setDiskSpaceIntervalChecking(DISK_SPACE_CHECK_SHORT_INTERVAL);
          isNotEnoughDiskSpace = true;
        }
      } else if (diskSpaceAvailable >= diskSpaceRequired) {
        const newDiskSpaceCheckIntervalLength =
          diskSpaceAvailable >= diskSpaceRequired * 2
            ? DISK_SPACE_CHECK_LONG_INTERVAL
            : DISK_SPACE_CHECK_MEDIUM_INTERVAL;
        if (isNotEnoughDiskSpace) {
          // State change: transitioning from not-enough to enough disk space
          setDiskSpaceIntervalChecking(newDiskSpaceCheckIntervalLength);
          isNotEnoughDiskSpace = false;
        } else if (
          newDiskSpaceCheckIntervalLength !== diskSpaceCheckIntervalLength
        ) {
          // Interval change: transitioning from medium to long interval (or vice versa)
          // This is a special case in which we adjust the disk space check polling interval:
          // - more than 2x of available space than required: LONG interval
          // - less than 2x of available space than required: MEDIUM interval
          setDiskSpaceIntervalChecking(newDiskSpaceCheckIntervalLength);
        }
      }

      const response = {
        isNotEnoughDiskSpace,
        diskSpaceRequired: prettysize(diskSpaceRequired),
        diskSpaceMissing: prettysize(diskSpaceMissing),
        diskSpaceRecommended: prettysize(diskSpaceRecommended),
        diskSpaceAvailable: prettysize(diskSpaceAvailable),
      };
      if (isNotEnoughDiskSpace)
        Logger.info('Not enough disk space', { response });
      if (typeof onCheckDiskSpace === 'function') onCheckDiskSpace(response);
      getDiskSpaceStatusChannel.send(response, mainWindow.webContents);
      return response;
    } catch (error) {
      // Remove diskSpaceCheckInterval if set
      if (diskSpaceCheckInterval) {
        clearInterval(diskSpaceCheckInterval);
        // Reset to default check interval
        diskSpaceCheckIntervalLength = DISK_SPACE_CHECK_LONG_INTERVAL;
      }
      const response = {
        isNotEnoughDiskSpace: false,
        diskSpaceRequired: '? GB',
        diskSpaceMissing: '? Bytes',
        diskSpaceRecommended: '? GB',
        diskSpaceAvailable: '? GB',
      };
      getDiskSpaceStatusChannel.send(response, mainWindow.webContents);
      return response;
    }
  };

  const setDiskSpaceIntervalChecking = interval => {
    clearInterval(diskSpaceCheckInterval);
    diskSpaceCheckInterval = setInterval(async () => {
      handleCheckDiskSpace();
    }, interval);
    diskSpaceCheckIntervalLength = interval;
  };

  // Start default interval
  setDiskSpaceIntervalChecking(diskSpaceCheckIntervalLength);

  getDiskSpaceStatusChannel.onReceive(diskSpaceRequired =>
    handleCheckDiskSpace(diskSpaceRequired)
  );

  return handleCheckDiskSpace;
};
