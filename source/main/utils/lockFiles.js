// @flow
import fs from 'fs';
import path from 'path';
import { lockSync, unlockSync } from 'lockfile';
import { launcherConfig } from '../config';
import { getProcessName, getProcessesByName } from './processes';

const getLockFilePath = async (): Promise<string> => {
  const processName = await getProcessName(process.pid);
  return path.join(launcherConfig.statePath, `${processName}.lock`);
};

export const acquireDaedalusInstanceLock = async () => {
  const lockFilePath = await getLockFilePath();
  try {
    lockSync(lockFilePath);
  } catch (error) {
    if (error.code === 'EEXIST') {
      console.log('Lockfile already exists. Checking for other Daedalus instance.');
      const processName = await getProcessName(process.pid);
      const processesWithSameName = await getProcessesByName(processName);
      if (processesWithSameName.length > 1) {
        throw new Error('Another Daedalus instance is already running.');
      }
      console.log('No other Daedalus instance running. Recreating the lockfile.');
      fs.unlinkSync(lockFilePath);
      await acquireDaedalusInstanceLock();
    } else {
      throw error;
    }
  }
};

export const releaseDaedalusInstanceLock = async () => unlockSync(await getLockFilePath());
