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

/**
 * Resolves a raw `readlink` result against the directory holding the link.
 *
 * `readlink` returns the target as it was recorded, so a relative link yields a
 * relative path. Every consumer of this module expects an absolute one.
 *
 * Absoluteness is tested against both path flavours. A Windows target such as
 * `Z:\DaedalusChain\chain` is not absolute by POSIX rules, and `path` is the
 * POSIX implementation whenever this runs on Linux — including in tests that
 * simulate Windows by overriding `process.platform`. Checking `path.win32` as
 * well keeps a drive-letter or UNC target intact instead of resolving it into
 * nonsense. A genuinely relative target is absolute under neither.
 */
const resolveLinkTargetPath = (linkPath: string, rawTarget: string): string =>
  path.isAbsolute(rawTarget) || path.win32.isAbsolute(rawTarget)
    ? rawTarget
    : path.resolve(path.dirname(linkPath), rawTarget);

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
        const rawLinkTarget = await fs.readlink(chainPath);
        // readlink returns the target exactly as recorded, which for a relative
        // link is a relative path. Callers pass this to setWorkDir() and to the
        // partial sync cutover, where a relative path would resolve against
        // process.cwd() rather than the state directory. Resolve it against the
        // link's own directory, matching captureChainPathState.
        return resolveLinkTargetPath(chainPath, rawLinkTarget);
      } catch {
        return path.resolve(chainPath);
      }
    }

    if (process.platform === 'win32' && chainStats.isDirectory()) {
      try {
        const junctionTarget = await fs.readlink(chainPath);
        // realpath can fail with ENOENT on mapped network drives (NAS/SMB)
        // even when the junction itself is valid; fall back to the raw
        // readlink target, which is always the absolute Windows path.
        //
        // Deliberately NOT passed through resolveLinkTargetPath: `path` is the
        // POSIX implementation in a Linux test process, so path.isAbsolute()
        // returns false for 'Z:\\...' and the target would be mangled. Junctions
        // record absolute paths, so there is nothing to resolve here anyway.
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
