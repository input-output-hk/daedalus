// @flow
import path from 'path';
import { lockSync, unlockSync } from 'lockfile';
import { launcherConfig } from '../config';
import { getProcessName } from './processes';

const getLockFilePath = async (): Promise<string> => {
  const processName = await getProcessName(process.pid);
  return path.join(launcherConfig.statePath, `${processName}.lock`);
};

export const acquireDaedalusInstanceLock = async () => {
  try {
    lockSync(await getLockFilePath());
  } catch (error) {
    console.log('Error while trying to acquire lock for Daedalus instance:');
    if (error.code === 'EEXIST') {
      throw new Error('Another Daedalus instance is already running.');
    } else {
      throw error;
    }
  }
};

export const releaseDaedalusInstanceLock = async () => unlockSync(await getLockFilePath());
