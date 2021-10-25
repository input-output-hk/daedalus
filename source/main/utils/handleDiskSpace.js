// @flow
import { BrowserWindow } from 'electron';
import checkDiskSpace from 'check-disk-space';
import prettysize from 'prettysize';
import { getDiskSpaceStatusChannel } from '../ipc/get-disk-space-status';
import { logger } from './logging';
import {
  DISK_SPACE_REQUIRED,
  DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE,
  DISK_SPACE_CHECK_LONG_INTERVAL,
  DISK_SPACE_CHECK_MEDIUM_INTERVAL,
  DISK_SPACE_CHECK_SHORT_INTERVAL,
  DISK_SPACE_RECOMMENDED_PERCENTAGE,
  stateDirectoryPath,
} from '../config';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import { CardanoNode } from '../cardano/CardanoNode';

const startStopCardanoNode = async (
  cardanoNode: CardanoNode,
  isNotEnoughDiskSpace: boolean
) => {
  if (isNotEnoughDiskSpace) {
    if (
      cardanoNode.state !== CardanoNodeStates.STOPPING &&
      cardanoNode.state !== CardanoNodeStates.STOPPED
    ) {
      try {
        logger.info('[DISK-SPACE-DEBUG] Stopping cardano node');
        await cardanoNode.stop();
      } catch (error) {
        logger.error('[DISK-SPACE-DEBUG] Cannot stop cardano node', error);
      }
    }
  } else if (
    // Happens after the user made more disk space
    cardanoNode.state !== CardanoNodeStates.STARTING &&
    cardanoNode.state !== CardanoNodeStates.RUNNING
  ) {
    try {
      logger.info(
        '[DISK-SPACE-DEBUG] restart cardano node after freeing up disk space'
      );
      if (cardanoNode._startupTries > 0) await cardanoNode.restart();
      else await cardanoNode.start();
    } catch (error) {
      logger.error(
        '[DISK-SPACE-DEBUG] Daedalus tried to restart, but failed',
        error
      );
    }
  }
};

export const handleDiskSpace = (
  mainWindow: BrowserWindow,
  cardanoNode: CardanoNode
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
      } = await checkDiskSpace(stateDirectoryPath);
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
        logger.info('Not enough disk space', { response });
      await startStopCardanoNode(cardanoNode, isNotEnoughDiskSpace);
      await getDiskSpaceStatusChannel.send(response, mainWindow.webContents);
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
        diskSpaceRequired: '',
        diskSpaceMissing: '',
        diskSpaceRecommended: '',
        diskSpaceAvailable: '',
      };
      await getDiskSpaceStatusChannel.send(response, mainWindow.webContents);
      return response;
    }
  };

  const setDiskSpaceIntervalChecking = (interval) => {
    clearInterval(diskSpaceCheckInterval);
    diskSpaceCheckInterval = setInterval(async () => {
      handleCheckDiskSpace();
    }, interval);
    diskSpaceCheckIntervalLength = interval;
  };

  // Start default interval
  setDiskSpaceIntervalChecking(diskSpaceCheckIntervalLength);

  getDiskSpaceStatusChannel.onReceive((diskSpaceRequired) =>
    handleCheckDiskSpace(diskSpaceRequired)
  );

  return handleCheckDiskSpace;
};
