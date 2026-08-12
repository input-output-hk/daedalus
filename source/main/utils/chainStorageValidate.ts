import fs from 'fs';
import path from 'path';
import type { ChainStorageValidation } from '../../common/types/watchdog.types';

export function getAvailableSpaceBytes(targetPath: string): number | undefined {
  try {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-expect-error — fs.statfsSync is available in Node 19+ / Electron 28+
    const stats = fs.statfsSync(targetPath);
    return stats.bfree * stats.bsize;
  } catch {
    return undefined;
  }
}

export function validatePath(
  candidatePath: string,
  stateDir: string
): ChainStorageValidation {
  const resolved = path.resolve(candidatePath);

  // Must not be a file
  try {
    const stat = fs.statSync(resolved);
    if (stat.isFile()) {
      return { isValid: false, path: candidatePath, reason: 'path-is-file' };
    }
  } catch {
    // Path doesn't exist yet — check parent exists
    const parent = path.dirname(resolved);
    try {
      const parentStat = fs.statSync(parent);
      if (!parentStat.isDirectory()) {
        return {
          isValid: false,
          path: candidatePath,
          reason: 'path-not-found',
        };
      }
    } catch {
      return { isValid: false, path: candidatePath, reason: 'path-not-found' };
    }
  }

  // Must not be inside the state dir.
  // On Windows the filesystem is case-insensitive but path.resolve() preserves
  // the case of the input string, so we normalise both sides to lower-case
  // before comparing.  On other platforms the filesystem is case-sensitive and
  // lower-casing is a no-op identity transform, so this is safe everywhere.
  const normalizedState = path.resolve(stateDir).toLowerCase();
  const normalizedCandidate = resolved.toLowerCase();
  if (
    normalizedCandidate === normalizedState ||
    normalizedCandidate.startsWith(normalizedState + path.sep.toLowerCase())
  ) {
    return { isValid: false, path: candidatePath, reason: 'inside-state-dir' };
  }

  // Check writability of the target (or its parent if it doesn't exist)
  const writeCheckTarget = fs.existsSync(resolved)
    ? resolved
    : path.dirname(resolved);
  try {
    fs.accessSync(writeCheckTarget, fs.constants.W_OK);
  } catch {
    return { isValid: false, path: candidatePath, reason: 'not-writable' };
  }

  const chainSubdir = path.join(resolved, 'chain');
  const chainSubdirectoryStatus: ChainStorageValidation['chainSubdirectoryStatus'] =
    fs.existsSync(chainSubdir) ? 'existing-directory' : 'will-create';

  const availableSpaceBytes = getAvailableSpaceBytes(writeCheckTarget);

  return {
    isValid: true,
    path: candidatePath,
    resolvedPath: resolved,
    availableSpaceBytes,
    chainSubdirectoryStatus,
  };
}
