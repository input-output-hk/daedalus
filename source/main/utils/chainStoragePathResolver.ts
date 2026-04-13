import path from 'path';
import fs from 'fs-extra';
import { logger } from './logging';
import {
  CHAIN_DIRECTORY_NAME,
  isPathNotFoundError,
} from './chainStorageManagerShared';

export type ResolvedStateDirectory = {
  exists: boolean;
  resolvedPath: string;
};

export const getManagedChainPath = (
  stateDir: string,
  customPath: string | null
): string =>
  customPath
    ? path.join(path.resolve(customPath), CHAIN_DIRECTORY_NAME)
    : path.join(stateDir, CHAIN_DIRECTORY_NAME);

const resolveLinkTarget = async (chainPath: string): Promise<string | null> => {
  try {
    return await fs.realpath(chainPath);
  } catch (error) {
    logger.warn(
      'ChainStorageManager: failed to resolve chain entry point target',
      {
        error,
        chainPath,
      }
    );
    return null;
  }
};

const resolveManagedChainPathFromEntryPoint = async (
  stateDir: string
): Promise<string> => {
  const chainPath = path.join(stateDir, CHAIN_DIRECTORY_NAME);

  try {
    const chainStats = await fs.lstat(chainPath);

    if (chainStats.isSymbolicLink()) {
      const resolvedLinkTarget = await resolveLinkTarget(chainPath);
      if (resolvedLinkTarget) {
        return resolvedLinkTarget;
      }

      return path.resolve(chainPath);
    }

    if (process.platform === 'win32' && chainStats.isDirectory()) {
      try {
        await fs.readlink(chainPath);

        const resolvedJunctionTarget = await resolveLinkTarget(chainPath);
        if (resolvedJunctionTarget) {
          return resolvedJunctionTarget;
        }
      } catch (error) {
        const code = (error as NodeJS.ErrnoException)?.code;
        if (
          !isPathNotFoundError(error) &&
          code !== 'EINVAL' &&
          code !== 'UNKNOWN'
        ) {
          logger.warn(
            'ChainStorageManager: failed to inspect Windows junction target',
            {
              error,
              chainPath,
            }
          );
        }
      }
    }

    if (chainStats.isDirectory()) {
      return path.resolve(chainPath);
    }
  } catch (error) {
    if (!isPathNotFoundError(error)) {
      logger.warn('ChainStorageManager: failed to inspect chain entry point', {
        error,
        chainPath,
      });
    }
  }

  return path.resolve(chainPath);
};

/**
 * Resolves the Daedalus state directory path, accounting for the directory
 * not existing yet.
 */
export async function resolveStateDirectoryPath(
  stateDir: string
): Promise<ResolvedStateDirectory> {
  const exists = await fs.pathExists(stateDir);

  return {
    exists,
    resolvedPath: exists ? await fs.realpath(stateDir) : path.resolve(stateDir),
  };
}

/**
 * Resolves the actual chain storage path, preferring the symlink target if
 * present, falling back to the custom-path in config, then stateDir.
 */
export async function resolveChainStoragePath(
  stateDir: string
): Promise<string> {
  return resolveManagedChainPathFromEntryPoint(stateDir);
}

/**
 * Resolves the working directory that Mithril should use. Returns the
 * resolved custom path when configured, otherwise the state directory.
 */
export async function resolveMithrilWorkDir(stateDir: string): Promise<string> {
  return resolveManagedChainPathFromEntryPoint(stateDir);
}
