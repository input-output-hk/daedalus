// @flow
import path from 'path';
import { lockSync, unlockSync } from 'lockfile';
import { launcherConfig } from '../config';

const lockFilePath = path.join(launcherConfig.statePath, 'Daedalus.lock');

export const acquireDaedalusInstanceLock = () => {
  try {
    lockSync(lockFilePath);
  } catch (error) {
    console.log('Error while trying to acquire lock for Daedalus instance:');
    if (error.code === 'EEXIST') {
      throw new Error('Another Daedalus instance is already running.');
    } else {
      throw error;
    }
  }
}
export const releaseDaedalusInstanceLock = () => unlockSync(lockFilePath);
