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
import type { CheckDiskSpaceResponse } from '../../common/types/no-disk-space.types';

export const handleDiskSpace = (
  mainWindow: BrowserWindow,
  cardanoNode: CardanoNode
): Function => {
  let diskSpaceCheckInterval;
  let diskSpaceCheckIntervalLength = DISK_SPACE_CHECK_LONG_INTERVAL; // Default check interval
  let isNotEnoughDiskSpace = false; // Default check state

  const handleCheckDiskSpace = async (
    hadNotEnoughSpaceLeft: boolean,
    forceDiskSpaceRequired?: number
  ): Promise<CheckDiskSpaceResponse> => {
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
        hadNotEnoughSpaceLeft,
      };

      const NO_SPACE_AND_CARDANO_NODE_CAN_BE_STOPPED =
        isNotEnoughDiskSpace &&
        cardanoNode.state !== CardanoNodeStates.STOPPING &&
        cardanoNode.state !== CardanoNodeStates.STOPPED;

      const CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME =
        !isNotEnoughDiskSpace &&
        cardanoNode.state === CardanoNodeStates.STOPPED &&
        cardanoNode._startupTries === 0;

      const CARDANO_NODE_CAN_BE_STARTED_AFTER_FREEING_SPACE =
        !isNotEnoughDiskSpace &&
        cardanoNode.state !== CardanoNodeStates.STOPPED &&
        cardanoNode.state !== CardanoNodeStates.STOPPING &&
        hadNotEnoughSpaceLeft;

      switch (true) {
        case NO_SPACE_AND_CARDANO_NODE_CAN_BE_STOPPED:
          try {
            logger.info('[DISK-SPACE-DEBUG] Stopping cardano node');
            await cardanoNode.stop();
          } catch (error) {
            logger.error('[DISK-SPACE-DEBUG] Cannot stop cardano node', error);
          }
          break;
        case CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME:
          await cardanoNode.start();
          break;
        case CARDANO_NODE_CAN_BE_STARTED_AFTER_FREEING_SPACE:
          try {
            logger.info(
              '[DISK-SPACE-DEBUG] restart cardano node after freeing up disk space'
            );
            if (cardanoNode._startupTries > 0) await cardanoNode.restart();
            else await cardanoNode.start();
            response.hadNotEnoughSpaceLeft = false;
          } catch (error) {
            logger.error(
              '[DISK-SPACE-DEBUG] Daedalus tried to restart, but failed',
              error
            );
          }
          break;
        default:
      }
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
        hadNotEnoughSpaceLeft: false,
      };
      await getDiskSpaceStatusChannel.send(response, mainWindow.webContents);
      return response;
    }
  };

  let hadNotEnoughSpaceLeft: boolean = false;

  const setDiskSpaceIntervalChecking = (interval) => {
    clearInterval(diskSpaceCheckInterval);
    diskSpaceCheckInterval = setInterval(async () => {
      const response = await handleCheckDiskSpace(hadNotEnoughSpaceLeft);
      hadNotEnoughSpaceLeft = response.hadNotEnoughSpaceLeft;
    }, interval);
    diskSpaceCheckIntervalLength = interval;
  };

  // Start default interval
  setDiskSpaceIntervalChecking(diskSpaceCheckIntervalLength);

  getDiskSpaceStatusChannel.onReceive((diskSpaceRequired) =>
    handleCheckDiskSpace(hadNotEnoughSpaceLeft, diskSpaceRequired)
  );

  return handleCheckDiskSpace;
};
