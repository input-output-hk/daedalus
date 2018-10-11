// @flow
import psList from 'ps-list';
import { isObject } from 'lodash';
import type {
  CardanoNodeStorageKeys,
  NetworkNames,
  PlatformNames,
  ProcessNames
} from '../../common/types/cardanoNode.types';
import {
  CardanoProcessNameOptions,
  NetworkNameOptions
} from '../../common/types/cardanoNode.types';

export type Process = {
  pid: number,
  name: string,
  cmd: string,
  ppid?: number,
  cpu: number,
  memore: number,
};

const checkCondition = async (
  condition: () => boolean,
  resolve: Function,
  reject: Function,
  timeout: number,
  retryEvery: number,
  timeWaited: number = 0
): Promise<void> => {
  const result = await condition();
  if (result) {
    resolve();
  } else if (timeWaited >= timeout) {
    reject(`Condition not met within ${timeout}ms: ${condition.toString()}`);
  } else {
    setTimeout(() => checkCondition(
      condition, resolve, reject, timeout, retryEvery, timeWaited + retryEvery
    ), retryEvery);
  }
};

export const promisedCondition = (
  cond: Function, timeout: number = 5000, retryEvery: number = 1000
): Promise<void> => new Promise((resolve, reject) => {
  checkCondition(cond, resolve, reject, timeout, retryEvery);
});

const getNetworkName = (network: NetworkNames): string => (
  NetworkNameOptions[network] || NetworkNameOptions.development
);

export const deriveStorageKeys = (network: NetworkNames): CardanoNodeStorageKeys => ({
  PREVIOUS_CARDANO_PID: `${getNetworkName(network)}-PREVIOUS-CARDANO-PID`
});

export const deriveProcessNames = (platform: PlatformNames): ProcessNames => ({
  CARDANO_PROCESS_NAME: CardanoProcessNameOptions[platform] || 'cardano-node'
});

export const getProcess = async (processId: number, processName: string): Promise<?Process> => {
  try {
    // retrieves all running processes
    const runningProcesses: Array<Process> = await psList();
    // filters running processes against given pid
    const matchingProcesses: Array<Process> = runningProcesses.filter(({ pid }) => (
      pid === processId
    ));
    // no processes exist with a matching PID
    if (!matchingProcesses.length) return null;
    // Return first matching process if names match
    const previousProcess: Process = matchingProcesses[0];
    if (isObject(previousProcess) && previousProcess.name === processName) {
      return previousProcess;
    }
  } catch (error) {
    return null;
  }
};
