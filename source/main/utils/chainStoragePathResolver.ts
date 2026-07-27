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
      if (resolvedLinkTarget) return resolvedLinkTarget;
      // realpath failed; fall back to readlink (handles Windows NAS/SMB drives
      // where the junction appears as a symlink but realpath fails with ENOENT):
      try {
        return await fs.readlink(chainPath);
      } catch {
        return path.resolve(chainPath);
      }
    }

    if (process.platform === 'win32' && chainStats.isDirectory()) {
      try {
        const junctionTarget = await fs.readlink(chainPath);
        // realpath can fail with ENOENT on mapped network drives (NAS/SMB)
        // even when the junction itself is valid; fall back to the raw
        // readlink target, which is always the absolute Windows path:
        const resolvedJunctionTarget = await resolveLinkTarget(chainPath);
        return resolvedJunctionTarget ?? junctionTarget;
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
 * Resolves the directory Mithril work products live in: the managed chain
 * directory the chain entry point currently targets — the resolved custom
 * target when a symlink/junction is configured, otherwise `<stateDir>/chain`.
 */
export async function resolveMithrilWorkDir(stateDir: string): Promise<string> {
  return resolveManagedChainPathFromEntryPoint(stateDir);
}
