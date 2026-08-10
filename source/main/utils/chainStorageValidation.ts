import path from 'path';
import fs from 'fs-extra';
import checkDiskSpace from 'check-disk-space';
import { DISK_SPACE_REQUIRED } from '../config';
import {
  ChainStorageConfig,
  ChainStorageValidation,
} from '../../common/types/mithril-bootstrap.types';
import { logger } from './logging';
import {
  CHAIN_DIRECTORY_NAME,
  isPathNotFoundError,
} from './chainStorageManagerShared';

type GetDefaultConfig = () => Promise<
  Pick<
    ChainStorageConfig,
    'defaultPath' | 'availableSpaceBytes' | 'requiredSpaceBytes'
  >
>;

/**
 * How long the free-space probe is given before validation continues without it.
 *
 * `checkDiskSpace` spawns a subprocess on Windows and a cold start there costs
 * seconds. This runs while the user waits on the directory picker, so it is held
 * to a shorter allowance than the background poll in handleDiskSpace, which has
 * nobody waiting on it.
 */
const DISK_SPACE_PROBE_TIMEOUT = 5 * 1000;

/**
 * Reads free space at `targetPath`, or resolves null when it cannot be read in
 * time.
 *
 * A probe that is slow, hung or failing says nothing about whether the directory
 * the user picked is usable, so it must not decide the outcome. handleDiskSpace
 * already takes this position for the periodic check: an unreadable probe is
 * logged and cardano-node is started anyway.
 */
const readFreeSpace = async (
  targetPath: string,
  timeout: number
): Promise<number | null> => {
  let timer: ReturnType<typeof setTimeout> | undefined;

  try {
    return await Promise.race([
      checkDiskSpace(targetPath)
        .then(({ free }) => free)
        .catch((error) => {
          logger.warn('ChainStorageManager: free space probe failed', {
            error,
            targetPath,
          });
          return null;
        }),
      new Promise<null>((resolve) => {
        timer = setTimeout(() => {
          logger.warn('ChainStorageManager: free space probe timed out', {
            targetPath,
            timeout,
          });
          resolve(null);
        }, timeout);
      }),
    ]);
  } finally {
    // Without this the timer holds the event loop open for its full duration
    // after the probe has already answered.
    if (timer) clearTimeout(timer);
  }
};

/**
 * Returns true when `child` is equal to or nested under `parent`.
 */
export function isSubPath(parent: string, child: string): boolean {
  const normalizedParent = path.resolve(parent);
  const normalizedChild = path.resolve(child);
  const comparableParent =
    process.platform === 'win32'
      ? normalizedParent.toLowerCase()
      : normalizedParent;
  const comparableChild =
    process.platform === 'win32'
      ? normalizedChild.toLowerCase()
      : normalizedChild;

  if (comparableChild === comparableParent) {
    return true;
  }

  return comparableChild.startsWith(`${comparableParent}${path.sep}`);
}

/**
 * Returns true when `a` and `b` resolve to the same file-system path,
 * applying case-insensitive comparison on Windows.
 */
export function isSamePath(a: string, b: string): boolean {
  const normalizedA = path.resolve(a);
  const normalizedB = path.resolve(b);

  if (process.platform === 'win32') {
    return normalizedA.toLowerCase() === normalizedB.toLowerCase();
  }

  return normalizedA === normalizedB;
}

/**
 * Validates a candidate chain-storage directory.
 *
 * @param targetDir     The directory path to validate (null → default)
 * @param stateDir      The Daedalus state directory path
 * @param getDefaultConfig  Resolves default storage metadata
 * @param requiredSpace Minimum free-space threshold in bytes
 * @param diskSpaceTimeout How long to wait for the free-space probe, in ms
 */
export async function validateChainStorageDirectory(
  targetDir: string | null,
  stateDir: string,
  getDefaultConfig: GetDefaultConfig,
  requiredSpace: number = DISK_SPACE_REQUIRED,
  diskSpaceTimeout: number = DISK_SPACE_PROBE_TIMEOUT
): Promise<ChainStorageValidation> {
  const chainPath = path.join(stateDir, CHAIN_DIRECTORY_NAME);
  const normalizedPath =
    typeof targetDir === 'string' && targetDir.trim().length > 0
      ? targetDir.trim()
      : null;

  if (normalizedPath == null) {
    return {
      isValid: true,
      path: null,
    };
  }

  const defaultValidation: ChainStorageValidation = {
    isValid: false,
    path: normalizedPath,
  };

  try {
    if (isSamePath(normalizedPath, chainPath)) {
      const defaultStorageConfig = await getDefaultConfig();

      return {
        isValid: true,
        path: null,
        resolvedPath: defaultStorageConfig.defaultPath,
        availableSpaceBytes: defaultStorageConfig.availableSpaceBytes,
        requiredSpaceBytes: defaultStorageConfig.requiredSpaceBytes,
      };
    }

    const exists = await fs.pathExists(normalizedPath);
    if (!exists) {
      return {
        ...defaultValidation,
        reason: 'path-not-found',
        message: 'Selected directory does not exist.',
      };
    }

    let resolvedPath: string;
    try {
      resolvedPath = await fs.realpath(normalizedPath);
    } catch (error) {
      // A link whose target no longer exists reaches this line on Windows. The
      // `pathExists` probe above follows the link on POSIX and has already
      // returned, but on Windows the reparse point itself satisfies the probe
      // and only the resolution fails. Without this the error falls through to
      // the generic catch and the user is told "Unable to validate selected
      // directory" for a condition that has its own message.
      //
      // ELOOP joins the two missing-path codes. A path that loops back on
      // itself names no directory either, and the user needs to hear the same
      // thing about it.
      const code = (error as NodeJS.ErrnoException)?.code;
      if (isPathNotFoundError(error) || code === 'ELOOP') {
        return {
          ...defaultValidation,
          reason: 'path-not-found',
          message: 'Selected directory does not exist.',
        };
      }
      throw error;
    }

    const targetStats = await fs.stat(resolvedPath);
    if (!targetStats.isDirectory()) {
      return {
        ...defaultValidation,
        resolvedPath,
        reason: 'not-writable',
        message: 'Selected path must be a directory.',
      };
    }

    // Treat symlink aliases that resolve to the default chain path as reset-to-default.
    const resolvedDefaultChainPath = await fs
      .realpath(chainPath)
      .catch(() => path.resolve(chainPath));
    if (
      resolvedPath === resolvedDefaultChainPath ||
      isSamePath(resolvedPath, chainPath)
    ) {
      const defaultStorageConfig = await getDefaultConfig();
      return {
        isValid: true,
        path: null,
        resolvedPath: defaultStorageConfig.defaultPath,
        availableSpaceBytes: defaultStorageConfig.availableSpaceBytes,
        requiredSpaceBytes: defaultStorageConfig.requiredSpaceBytes,
      };
    }

    const isDirectChainSelection =
      path.basename(resolvedPath) === CHAIN_DIRECTORY_NAME;
    const validationPath = isDirectChainSelection
      ? path.dirname(resolvedPath)
      : normalizedPath;
    const resolvedValidationPath = isDirectChainSelection
      ? path.dirname(resolvedPath)
      : resolvedPath;

    const stateDirExists = await fs.pathExists(stateDir);
    const resolvedStatePath = stateDirExists
      ? await fs.realpath(stateDir)
      : path.resolve(stateDir);

    // Reject paths that are nested inside the current managed chain directory.
    // Selecting such a path would create a nested chain/ inside existing chain data.
    // For direct chain selections (path ending in /chain), check the parent directory.
    const pathToCheckNesting = isDirectChainSelection
      ? resolvedValidationPath
      : resolvedPath;
    if (
      isSubPath(resolvedDefaultChainPath, pathToCheckNesting) &&
      pathToCheckNesting !== resolvedDefaultChainPath
    ) {
      return {
        ...defaultValidation,
        resolvedPath: pathToCheckNesting,
        reason: 'is-managed-child',
        message:
          'Selected directory is inside the current chain storage location.',
      };
    }

    if (isSubPath(resolvedStatePath, resolvedValidationPath)) {
      return {
        ...defaultValidation,
        resolvedPath: resolvedValidationPath,
        reason: 'inside-state-dir',
        message: 'Selected directory cannot be inside Daedalus state dir.',
      };
    }

    try {
      await fs.access(resolvedValidationPath, fs.constants.W_OK);
    } catch (accessError) {
      // Without this, EACCES falls through to the generic catch below and the
      // user is told only "Unable to validate selected directory", when the
      // actual problem — and the one the renderer already has copy for — is
      // that the location cannot be written to.
      const code = (accessError as NodeJS.ErrnoException)?.code;
      if (code === 'EACCES' || code === 'EPERM' || code === 'EROFS') {
        return {
          ...defaultValidation,
          resolvedPath: resolvedValidationPath,
          reason: 'not-writable',
          message: 'Selected directory is not writable.',
        };
      }
      throw accessError;
    }

    const managedChainPath = isDirectChainSelection
      ? resolvedPath
      : path.join(resolvedValidationPath, CHAIN_DIRECTORY_NAME);
    const managedChainExists = isDirectChainSelection
      ? true
      : await fs.pathExists(managedChainPath);
    let chainSubdirectoryStatus:
      | ChainStorageValidation['chainSubdirectoryStatus']
      | undefined = isDirectChainSelection
      ? 'existing-directory'
      : 'will-create';

    if (managedChainExists) {
      const managedChainStats = isDirectChainSelection
        ? targetStats
        : await fs.lstat(managedChainPath);
      if (managedChainStats.isDirectory()) {
        const existingManagedChainPath = isDirectChainSelection
          ? resolvedPath
          : managedChainPath;
        const managedChainEntries = await fs.readdir(existingManagedChainPath);
        chainSubdirectoryStatus =
          managedChainEntries.length > 0 ? 'existing-directory' : undefined;
      } else {
        return {
          ...defaultValidation,
          resolvedPath: resolvedValidationPath,
          availableSpaceBytes: undefined,
          requiredSpaceBytes: requiredSpace,
          chainSubdirectoryStatus: 'path-is-file',
          reason: 'path-is-file',
          message:
            'Daedalus cannot use this location because <selected directory>/chain already exists as a file.',
        };
      }
    }

    const free = await readFreeSpace(resolvedValidationPath, diskSpaceTimeout);

    // Only a figure that was actually read can reject the directory. When the
    // probe does not answer, the selection is accepted without a free-space
    // figure rather than blocked on a reading nobody has.
    if (free != null && free < requiredSpace) {
      return {
        ...defaultValidation,
        resolvedPath: resolvedValidationPath,
        availableSpaceBytes: free,
        requiredSpaceBytes: requiredSpace,
        chainSubdirectoryStatus,
        reason: 'insufficient-space',
        message: 'Selected directory does not have enough free space.',
      };
    }

    return {
      isValid: true,
      path: validationPath,
      resolvedPath: resolvedValidationPath,
      availableSpaceBytes: free ?? undefined,
      requiredSpaceBytes: requiredSpace,
      chainSubdirectoryStatus,
    };
  } catch (error) {
    logger.warn('ChainStorageManager: validation failed', {
      error,
      targetDir: normalizedPath,
    });
    return {
      ...defaultValidation,
      reason: 'unknown',
      message: 'Unable to validate selected directory.',
    };
  }
}
