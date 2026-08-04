import path from 'path';
import fs from 'fs-extra';
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
  launcherConfig,
} from '../config';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import { CardanoNode } from '../cardano/CardanoNode';
import type { CheckDiskSpaceResponse } from '../../common/types/no-disk-space.types';
import {
  chainStorageCoordinator,
  getChainStorageManager,
} from './chainStorageCoordinator';
import { getMithrilController } from '../mithril/MithrilController';
import { isMithrilPartialSyncSuppressingDiskSpaceCheck } from '../../common/types/mithril-partial-sync.types';

const getDiskCheckReport = async (
  targetPath: string,
  timeout: number = DISK_SPACE_CHECK_TIMEOUT
): Promise<CheckDiskSpaceResponse> => {
  // On Windows, checkDiskSpace uses GetDiskFreeSpaceEx which may report the
  // host drive's (C:) free space when given a junction path rather than the
  // junction target's (D:). Resolve the real path first so we always check
  // the volume that actually holds the chain data.
  let resolvedPath = targetPath;
  try {
    resolvedPath = await fs.realpath(targetPath);
  } catch {
    // Path does not exist yet (fresh install). Fall back to the original so
    // checkDiskSpace can still run against the nearest existing ancestor.
  }

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
  const diskCheckPromise = new Promise<CheckDiskSpaceResponse>((resolve) => {
    checkDiskSpace(resolvedPath)
      .then(({ free, size }) => {
        logger.info('[DISK-SPACE-DEBUG] Disk space check completed', {
          free,
          size,
        });
        resolve({
          ...initialReport,
          diskSpaceAvailableRaw: free,
          diskSpaceAvailable: prettysize(free),
          diskTotalSpaceRaw: size,
        });
      })
      .catch((error) => {
        logger.error('[DISK-SPACE-DEBUG] Error getting diskCheckReport', {
          error,
        });
        resolve({ ...initialReport, isError: true });
      });
  });

  const timeoutPromise = new Promise<CheckDiskSpaceResponse>((resolve) => {
    setTimeout(() => {
      resolve({ ...initialReport, isError: true });
    }, timeout);
  });

  return Promise.race([diskCheckPromise, timeoutPromise]);
};

export const handleDiskSpace = (
  mainWindow: BrowserWindow,
  cardanoNode: CardanoNode
) => {
  let diskSpaceCheckInterval;
  let diskSpaceCheckIntervalLength = DISK_SPACE_CHECK_LONG_INTERVAL; // Default check interval

  let isNotEnoughDiskSpace = false; // Default check state
  let directoryChangeGeneration = 0;
  let activeDiskSpaceCheckPromise: Promise<CheckDiskSpaceResponse> | null =
    null;
  let pendingDiskSpaceCheckArgs: {
    hadNotEnoughSpaceLeft?: boolean;
    forceDiskSpaceRequired?: number;
  } | null = null;
  let pendingDiskSpaceCheckWaiters: Array<{
    resolve: (response: CheckDiskSpaceResponse) => void;
    reject: (error: unknown) => void;
  }> = [];
  const chainStorageManager = getChainStorageManager();
  const mithrilController = getMithrilController();

  const runHandleCheckDiskSpace = async (
    hadNotEnoughSpaceLeft?: boolean,
    forceDiskSpaceRequired?: number
  ): Promise<CheckDiskSpaceResponse> => {
    const hadNotEnoughSpaceFlag = hadNotEnoughSpaceLeft ?? false;
    const currentGeneration = directoryChangeGeneration;
    const diskSpaceRequired = forceDiskSpaceRequired || DISK_SPACE_REQUIRED;
    const getStaleResponse = (): CheckDiskSpaceResponse => ({
      isNotEnoughDiskSpace,
      diskSpaceRequired: '',
      diskSpaceMissing: '',
      diskSpaceRecommended: '',
      diskSpaceAvailable: '',
      hadNotEnoughSpaceLeft: hadNotEnoughSpaceFlag,
      diskSpaceAvailableRaw: 0,
      diskTotalSpaceRaw: 0,
      isError: false,
    });

    const partialSyncStatus = mithrilController.getPartialSyncStatus().status;
    if (isMithrilPartialSyncSuppressingDiskSpaceCheck(partialSyncStatus)) {
      return getStaleResponse();
    }

    // Resolve chain storage layout to determine disk check path
    let diskCheckPath: string;
    try {
      const layoutResult =
        await chainStorageCoordinator.ensureManagedChainLayout();
      if (currentGeneration !== directoryChangeGeneration) {
        return getStaleResponse();
      }
      diskCheckPath = layoutResult.managedChainPath;
    } catch (error) {
      logger.error('[DISK-SPACE-DEBUG] Failed to resolve chain layout', {
        error,
      });
      if (currentGeneration !== directoryChangeGeneration) {
        return getStaleResponse();
      }
      // Fall back to state directory path
      diskCheckPath = stateDirectoryPath;
    }

    const response = await getDiskCheckReport(diskCheckPath);
    if (currentGeneration !== directoryChangeGeneration) {
      return getStaleResponse();
    }

    if (response.isError) {
      logger.info(
        '[DISK-SPACE-DEBUG] We could not check disk space, but we will try to start cardano-node anyway',
        null
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
          setDiskSpaceIntervalChecking(newDiskSpaceCheckIntervalLength);
        }
      }

      response.isNotEnoughDiskSpace = isNotEnoughDiskSpace;
      response.diskSpaceRequired = prettysize(diskSpaceRequired);
      response.diskSpaceMissing = prettysize(diskSpaceMissing);
      response.diskSpaceRecommended = prettysize(diskSpaceRecommended);
      response.hadNotEnoughSpaceLeft = hadNotEnoughSpaceFlag;
    }

    const NO_SPACE_AND_CARDANO_NODE_CAN_BE_STOPPED =
      isNotEnoughDiskSpace &&
      cardanoNode.state !== CardanoNodeStates.STOPPING &&
      cardanoNode.state !== CardanoNodeStates.STOPPED;
    const CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME =
      !isNotEnoughDiskSpace && cardanoNode.state === CardanoNodeStates.STOPPED;
    const CARDANO_NODE_CAN_BE_STARTED_AFTER_FREEING_SPACE =
      !isNotEnoughDiskSpace &&
      cardanoNode.state !== CardanoNodeStates.STOPPED &&
      cardanoNode.state !== CardanoNodeStates.STOPPING &&
      hadNotEnoughSpaceFlag;

    try {
      switch (true) {
        case NO_SPACE_AND_CARDANO_NODE_CAN_BE_STOPPED:
          try {
            logger.info('[DISK-SPACE-DEBUG] Stopping cardano node', null);
            await cardanoNode.stop();
          } catch (error) {
            logger.error('[DISK-SPACE-DEBUG] Cannot stop cardano node', error);
          }

          break;

        case CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME:
          try {
            await cardanoNode.start();
          } catch (startError) {
            logger.error('[DISK-SPACE-DEBUG] cardano-node start failed', {
              error: startError,
            });
          }
          break;

        case CARDANO_NODE_CAN_BE_STARTED_AFTER_FREEING_SPACE:
          try {
            logger.info(
              '[DISK-SPACE-DEBUG] restart cardano node after freeing up disk space',
              null
            );
            if (cardanoNode._startupTries > 0) await cardanoNode.restart();
            else {
              await cardanoNode.start();
              if (currentGeneration !== directoryChangeGeneration) {
                return getStaleResponse();
              }
            }
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

  const flushPendingDiskSpaceCheck = () => {
    if (activeDiskSpaceCheckPromise || !pendingDiskSpaceCheckArgs) return;

    const nextArgs = pendingDiskSpaceCheckArgs;
    const nextWaiters = pendingDiskSpaceCheckWaiters;

    pendingDiskSpaceCheckArgs = null;
    pendingDiskSpaceCheckWaiters = [];

    const nextPromise = startDiskSpaceCheckRun(nextArgs);

    nextPromise.then(
      (response) => {
        nextWaiters.forEach(({ resolve }) => resolve(response));
      },
      (error) => {
        nextWaiters.forEach(({ reject }) => reject(error));
      }
    );
  };

  const startDiskSpaceCheckRun = ({
    hadNotEnoughSpaceLeft,
    forceDiskSpaceRequired,
  }: {
    hadNotEnoughSpaceLeft?: boolean;
    forceDiskSpaceRequired?: number;
  }): Promise<CheckDiskSpaceResponse> => {
    const diskSpaceCheckPromise = runHandleCheckDiskSpace(
      hadNotEnoughSpaceLeft,
      forceDiskSpaceRequired
    );

    activeDiskSpaceCheckPromise = diskSpaceCheckPromise;

    diskSpaceCheckPromise.then(
      () => {
        if (activeDiskSpaceCheckPromise === diskSpaceCheckPromise) {
          activeDiskSpaceCheckPromise = null;
        }
        flushPendingDiskSpaceCheck();
      },
      () => {
        if (activeDiskSpaceCheckPromise === diskSpaceCheckPromise) {
          activeDiskSpaceCheckPromise = null;
        }
        flushPendingDiskSpaceCheck();
      }
    );

    return diskSpaceCheckPromise;
  };

  const mergePendingDiskSpaceCheckArgs = (nextArgs: {
    hadNotEnoughSpaceLeft?: boolean;
    forceDiskSpaceRequired?: number;
  }) => {
    if (!pendingDiskSpaceCheckArgs) {
      pendingDiskSpaceCheckArgs = nextArgs;
      return;
    }

    pendingDiskSpaceCheckArgs = {
      hadNotEnoughSpaceLeft:
        pendingDiskSpaceCheckArgs.hadNotEnoughSpaceLeft === false ||
        nextArgs.hadNotEnoughSpaceLeft === false
          ? false
          : (nextArgs.hadNotEnoughSpaceLeft ??
            pendingDiskSpaceCheckArgs.hadNotEnoughSpaceLeft),
      forceDiskSpaceRequired:
        nextArgs.forceDiskSpaceRequired ??
        pendingDiskSpaceCheckArgs.forceDiskSpaceRequired,
    };
  };

  const launchHandleCheckDiskSpace = (
    hadNotEnoughSpaceLeft?: boolean,
    forceDiskSpaceRequired?: number
  ): Promise<CheckDiskSpaceResponse> => {
    const args = {
      hadNotEnoughSpaceLeft,
      forceDiskSpaceRequired,
    };

    if (!activeDiskSpaceCheckPromise) {
      return startDiskSpaceCheckRun(args);
    }

    mergePendingDiskSpaceCheckArgs(args);

    return new Promise<CheckDiskSpaceResponse>((resolve, reject) => {
      pendingDiskSpaceCheckWaiters.push({ resolve, reject });
    });
  };

  const resetOnDirectoryChange = () => {
    directoryChangeGeneration += 1;
    launchHandleCheckDiskSpace(false).catch((error) => {
      logger.error(
        '[DISK-SPACE] Immediate disk-space recheck after directory change failed',
        {
          error,
        }
      );
    });
  };

  const handleCheckDiskSpace = (
    hadNotEnoughSpaceLeft?: boolean,
    forceDiskSpaceRequired?: number
  ): Promise<CheckDiskSpaceResponse> =>
    launchHandleCheckDiskSpace(hadNotEnoughSpaceLeft, forceDiskSpaceRequired);

  chainStorageCoordinator.onDirectoryChanged(resetOnDirectoryChange);

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
    diskSpaceCheckInterval = setInterval(() => {
      handleCheckDiskSpace(hadNotEnoughSpaceLeft)
        .then((response) => {
          hadNotEnoughSpaceLeft = response?.hadNotEnoughSpaceLeft;
        })
        .catch((error) => {
          logger.error('[DISK-SPACE] Background disk-space poll failed', {
            error,
          });
        });
    }, interval);
    diskSpaceCheckIntervalLength = interval;
  };

  // Start default interval
  setDiskSpaceIntervalChecking(diskSpaceCheckIntervalLength);
  getDiskSpaceStatusChannel.onReceive(async () => {
    const diskSpacePath = await chainStorageManager.resolveDiskSpaceCheckPath();
    const diskReport = await getDiskCheckReport(diskSpacePath);
    await getDiskSpaceStatusChannel.send(diskReport, mainWindow.webContents);
    return diskReport;
  });
  return handleCheckDiskSpace;
};
