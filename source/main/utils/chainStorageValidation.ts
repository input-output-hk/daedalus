import path from 'path';
import fs from 'fs-extra';
import checkDiskSpace from 'check-disk-space';
import { DISK_SPACE_REQUIRED } from '../config';
import {
  ChainStorageConfig,
  ChainStorageValidation,
} from '../../common/types/mithril-bootstrap.types';
import { logger } from './logging';

const CHAIN_DIRECTORY_NAME = 'chain';

type GetDefaultConfig = () => Promise<
  Pick<
    ChainStorageConfig,
    'defaultPath' | 'availableSpaceBytes' | 'requiredSpaceBytes'
  >
>;

/**
 * Returns true when `child` is equal to or nested under `parent`.
 */
export function isSubPath(parent: string, child: string): boolean {
  const normalizedParent = path.resolve(parent);
  const normalizedChild = path.resolve(child);

  if (normalizedChild === normalizedParent) {
    return true;
  }

  return normalizedChild.startsWith(`${normalizedParent}${path.sep}`);
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
 */
export async function validateChainStorageDirectory(
  targetDir: string | null,
  stateDir: string,
  getDefaultConfig: GetDefaultConfig,
  requiredSpace: number = DISK_SPACE_REQUIRED
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

    const resolvedPath = await fs.realpath(normalizedPath);
    const targetStats = await fs.stat(resolvedPath);
    if (!targetStats.isDirectory()) {
      return {
        ...defaultValidation,
        resolvedPath,
        reason: 'not-writable',
        message: 'Selected path must be a directory.',
      };
    }

    const stateDirExists = await fs.pathExists(stateDir);
    const resolvedStatePath = stateDirExists
      ? await fs.realpath(stateDir)
      : path.resolve(stateDir);

    if (isSubPath(resolvedStatePath, resolvedPath)) {
      return {
        ...defaultValidation,
        resolvedPath,
        reason: 'inside-state-dir',
        message: 'Selected directory cannot be inside Daedalus state dir.',
      };
    }

    await fs.access(resolvedPath, fs.constants.W_OK);

    const { free } = await checkDiskSpace(resolvedPath);
    if (free < requiredSpace) {
      return {
        ...defaultValidation,
        resolvedPath,
        availableSpaceBytes: free,
        requiredSpaceBytes: requiredSpace,
        reason: 'insufficient-space',
        message: 'Selected directory does not have enough free space.',
      };
    }

    return {
      isValid: true,
      path: normalizedPath,
      resolvedPath,
      availableSpaceBytes: free,
      requiredSpaceBytes: requiredSpace,
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
