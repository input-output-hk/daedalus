// @flow
import path from 'path';
import { lockSync, unlockSync, checkSync } from './lock-files';
import { launcherConfig } from '../config';
import { getProcessName } from './processes';
import { Logger } from '../../common/utils/logging';

const OPTIONS = {
  onCompromised: (error) => Logger.warn(error),
  realpath: false, // Resolve symlinks (note that if true, the file must exist previously)
};

const getLockFilePath = async (): Promise<string> => {
  const processName = await getProcessName(process.pid);
  return path.join(launcherConfig.statePath, processName);
};

const isLockfileActive = (lockFilePath) => {
  try {
    return checkSync(lockFilePath, OPTIONS);
  } catch (error) {
    return false;
  }
};

export const acquireDaedalusInstanceLock = async () => {
  const lockFilePath = await getLockFilePath();
  const isOtherInstanceActive = isLockfileActive(lockFilePath);
  if (isOtherInstanceActive) {
    return Promise.reject(new Error('Another Daedalus instance is already running.'));
  }
  lockSync(lockFilePath, OPTIONS);
};

export const releaseDaedalusInstanceLock = async () => (
  unlockSync(await getLockFilePath(), OPTIONS)
);

// Map SIGINT & SIGTERM to process exit
// so that lockfile removes the lockfile automatically
// https://www.npmjs.com/package/proper-lockfile
process
  .once('SIGINT', () => process.exit(1))
  .once('SIGTERM', () => process.exit(1));
