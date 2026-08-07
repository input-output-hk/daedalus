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
} from '../config';
import type { CheckDiskSpaceResponse } from '../../common/types/no-disk-space.types';

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

export const handleDiskSpace = (mainWindow: BrowserWindow) => {
  let diskSpaceCheckInterval;
  let diskSpaceCheckIntervalLength = DISK_SPACE_CHECK_LONG_INTERVAL;

  let isNotEnoughDiskSpace = false;
  let activeDiskSpaceCheckPromise: Promise<
    CheckDiskSpaceResponse
  > | null = null;
  let pendingDiskSpaceCheckArgs: {
    hadNotEnoughSpaceLeft?: boolean;
    forceDiskSpaceRequired?: number;
  } | null = null;
  let pendingDiskSpaceCheckWaiters: Array<{
    resolve: (response: CheckDiskSpaceResponse) => void;
    reject: (error: unknown) => void;
  }> = [];

  const diskCheckPath = path.join(stateDirectoryPath, 'chain');

  const runHandleCheckDiskSpace = async (
    hadNotEnoughSpaceLeft?: boolean,
    forceDiskSpaceRequired?: number
  ): Promise<CheckDiskSpaceResponse> => {
    const hadNotEnoughSpaceFlag = hadNotEnoughSpaceLeft ?? false;
    const diskSpaceRequired = forceDiskSpaceRequired || DISK_SPACE_REQUIRED;

    const response = await getDiskCheckReport(diskCheckPath);

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
          setDiskSpaceIntervalChecking(DISK_SPACE_CHECK_SHORT_INTERVAL);
          isNotEnoughDiskSpace = true;
        }
      } else if (response.diskSpaceAvailableRaw >= diskSpaceRequired) {
        const newDiskSpaceCheckIntervalLength =
          response.diskSpaceAvailableRaw >= diskSpaceRequired * 2
            ? DISK_SPACE_CHECK_LONG_INTERVAL
            : DISK_SPACE_CHECK_MEDIUM_INTERVAL;

        if (isNotEnoughDiskSpace) {
          setDiskSpaceIntervalChecking(newDiskSpaceCheckIntervalLength);
          isNotEnoughDiskSpace = false;
        } else if (
          newDiskSpaceCheckIntervalLength !== diskSpaceCheckIntervalLength
        ) {
          setDiskSpaceIntervalChecking(newDiskSpaceCheckIntervalLength);
        }
      }

      response.isNotEnoughDiskSpace = isNotEnoughDiskSpace;
      response.diskSpaceRequired = prettysize(diskSpaceRequired);
      response.diskSpaceMissing = prettysize(diskSpaceMissing);
      response.diskSpaceRecommended = prettysize(diskSpaceRecommended);
      response.hadNotEnoughSpaceLeft = hadNotEnoughSpaceFlag;
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
          : nextArgs.hadNotEnoughSpaceLeft ??
            pendingDiskSpaceCheckArgs.hadNotEnoughSpaceLeft,
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

  const handleCheckDiskSpace = (
    hadNotEnoughSpaceLeft?: boolean,
    forceDiskSpaceRequired?: number
  ): Promise<CheckDiskSpaceResponse> =>
    launchHandleCheckDiskSpace(hadNotEnoughSpaceLeft, forceDiskSpaceRequired);

  const resetInterval = (interval: number) => {
    if (diskSpaceCheckInterval) {
      clearInterval(diskSpaceCheckInterval);
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

  setDiskSpaceIntervalChecking(diskSpaceCheckIntervalLength);
  getDiskSpaceStatusChannel.onReceive(async () => {
    const diskReport = await getDiskCheckReport(diskCheckPath);
    await getDiskSpaceStatusChannel.send(diskReport, mainWindow.webContents);
    return diskReport;
  });
  return handleCheckDiskSpace;
};
