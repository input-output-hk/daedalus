import { BrowserWindow } from 'electron';
import checkDiskSpace from 'check-disk-space';
import prettysize from 'prettysize';
import { getDiskSpaceStatusChannel } from '../ipc/get-disk-space-status';
import { logger } from './logging';
import {
  DISK_SPACE_CHECK_DONT_BOTHER_ME_INTERVAL,
  DISK_SPACE_CHECK_LONG_INTERVAL,
  DISK_SPACE_CHECK_MEDIUM_INTERVAL,
  DISK_SPACE_CHECK_SHORT_INTERVAL,
  DISK_SPACE_RECOMMENDED_PERCENTAGE,
  DISK_SPACE_REQUIRED,
  DISK_SPACE_CHECK_TIMEOUT,
  DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE,
  stateDirectoryPath,
} from '../config';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import { CardanoNode } from '../cardano/CardanoNode';
import type { CheckDiskSpaceResponse } from '../../common/types/no-disk-space.types';

const getDiskCheckReport = async (
  path: string,
  timeout: number = DISK_SPACE_CHECK_TIMEOUT
): Promise<CheckDiskSpaceResponse> => {
  const initialReport: CheckDiskSpaceResponse = {
    isNotEnoughDiskSpace: false,
    diskSpaceRequired: '',
    diskSpaceMissing: '',
    diskSpaceRecommended: '',
    diskSpaceAvailable: '',
    hadNotEnoughSpaceLeft: false,
    diskSpaceAvailableRaw: 0,
    diskTotalSpaceRaw: 0,
    isError: false,
  };
  // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
  return Promise.race([
    // Disk space check promise
    new Promise((resolve) => {
      checkDiskSpace(path)
        .then(({ free, size }) => {
          logger.info('[DISK-SPACE-DEBUG] Disk space check completed', {
            free,
            size,
          });
          resolve({
            ...initialReport,
            diskSpaceAvailableRaw: free,
            diskSpaceAvailable: prettysize(free),
            diskTotalSpace: size,
          });
        })
        .catch((error) => {
          logger.error(
            '[DISK-SPACE-DEBUG] Error getting diskCheckReport',
            error
          );
          resolve({ ...initialReport, isError: true });
        });
    }), // Timeout promise
    new Promise((resolve) => {
      setTimeout(() => {
        resolve({ ...initialReport, isError: true });
      }, timeout);
    }),
  ]);
};

export const handleDiskSpace = (
  mainWindow: BrowserWindow,
  cardanoNode: CardanoNode
): ((...args: Array<any>) => any) => {
  let diskSpaceCheckInterval;
  let diskSpaceCheckIntervalLength = DISK_SPACE_CHECK_LONG_INTERVAL; // Default check interval

  let isNotEnoughDiskSpace = false; // Default check state

  const handleCheckDiskSpace = async (
    hadNotEnoughSpaceLeft: boolean,
    forceDiskSpaceRequired?: number
  ): Promise<CheckDiskSpaceResponse> => {
    const diskSpaceRequired = forceDiskSpaceRequired || DISK_SPACE_REQUIRED;
    const response = await getDiskCheckReport(stateDirectoryPath);

    if (response.isError) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.info(
        '[DISK-SPACE-DEBUG] We could not check disk space, but we will try to start cardano-node anyway'
      );
      resetInterval(DISK_SPACE_CHECK_DONT_BOTHER_ME_INTERVAL);
    } else {
      const diskSpaceMissing = Math.max(
        diskSpaceRequired - response.diskSpaceAvailableRaw,
        0
      );
      const diskSpaceRecommended =
        (response.diskTotalSpaceRaw * DISK_SPACE_RECOMMENDED_PERCENTAGE) / 100;
      const diskSpaceRequiredMargin =
        diskSpaceRequired -
        (diskSpaceRequired * DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE) / 100;

      if (response.diskSpaceAvailableRaw <= diskSpaceRequiredMargin) {
        if (!isNotEnoughDiskSpace) {
          // State change: transitioning from enough to not-enough disk space
          setDiskSpaceIntervalChecking(DISK_SPACE_CHECK_SHORT_INTERVAL);
          isNotEnoughDiskSpace = true;
        }
      } else if (response.diskSpaceAvailableRaw >= diskSpaceRequired) {
        const newDiskSpaceCheckIntervalLength =
          response.diskSpaceAvailableRaw >= diskSpaceRequired * 2
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

      response.isNotEnoughDiskSpace = isNotEnoughDiskSpace;
      response.diskSpaceRequired = prettysize(diskSpaceRequired);
      response.diskSpaceMissing = prettysize(diskSpaceMissing);
      response.diskSpaceRecommended = prettysize(diskSpaceRecommended);
      response.hadNotEnoughSpaceLeft = hadNotEnoughSpaceLeft;
    }

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

    try {
      switch (true) {
        case NO_SPACE_AND_CARDANO_NODE_CAN_BE_STOPPED:
          try {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
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
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
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
    } catch (error) {
      logger.error('[DISK-SPACE-DEBUG] Unknown error', error);
      resetInterval(DISK_SPACE_CHECK_MEDIUM_INTERVAL);
    }

    await getDiskSpaceStatusChannel.send(response, mainWindow.webContents);
    return response;
  };

  const resetInterval = (interval: number) => {
    // Remove diskSpaceCheckInterval if set
    if (diskSpaceCheckInterval) {
      clearInterval(diskSpaceCheckInterval);
      // Reset to default check interval
      diskSpaceCheckIntervalLength = interval;
    }
  };

  let hadNotEnoughSpaceLeft = false;

  const setDiskSpaceIntervalChecking = (interval) => {
    clearInterval(diskSpaceCheckInterval);
    diskSpaceCheckInterval = setInterval(async () => {
      const response = await handleCheckDiskSpace(hadNotEnoughSpaceLeft);
      hadNotEnoughSpaceLeft = response?.hadNotEnoughSpaceLeft;
    }, interval);
    diskSpaceCheckIntervalLength = interval;
  };

  // Start default interval
  setDiskSpaceIntervalChecking(diskSpaceCheckIntervalLength);
  getDiskSpaceStatusChannel.onReceive(async () => {
    const diskReport = await getDiskCheckReport(stateDirectoryPath);
    await getDiskSpaceStatusChannel.send(diskReport, mainWindow.webContents);
    return diskReport;
  });
  return handleCheckDiskSpace;
};
