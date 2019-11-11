// @flow
/* eslint-disable */
import { Logger } from '../utils/logging';
import type {
  CardanoNodeStorageKeys,
  NetworkNames,
  PlatformNames,
  ProcessNames,
} from '../../common/types/cardano-node.types';
import {
  CardanoProcessNameOptions,
  NetworkNameOptions,
} from '../../common/types/cardano-node.types';

export type Process = {
  pid: number,
  name: string,
  cmd: string,
  ppid?: number,
  cpu: number,
  memory: number,
};

const checkCondition = async (
  condition: () => boolean,
  resolve: Function,
  reject: Function,
  timeout: number,
  retryEvery: number,
  timeWaited: number = 0
): Promise<void> => {
  Logger.info('>> CALL 1: ', { condition });
  const result = await condition();
  Logger.info('>> CALL 1.1. RESULT: ', { result });
  if (result) {
    Logger.info('>> CALL 1.2. CASE 1');
    resolve();
  } else if (timeWaited >= timeout) {
    Logger.info('>> CALL 1.2. CASE 2');
    reject(`Promised condition not met within ${timeout}ms.`);
  } else {
    Logger.info('>> CALL 1.2. CASE 3: ', {
      timeWaited,
      timeout,
    });
    setTimeout(
      () =>
        checkCondition(
          condition,
          resolve,
          reject,
          timeout,
          retryEvery,
          timeWaited + retryEvery
        ),
      retryEvery
    );
  }
};

export const promisedCondition = (
  cond: Function,
  timeout: number = 5000,
  retryEvery: number = 1000
): Promise<void> =>
  new Promise((resolve, reject) => {
    checkCondition(cond, resolve, reject, timeout, retryEvery);
  });

const getNetworkName = (network: NetworkNames): string =>
  NetworkNameOptions[network] || NetworkNameOptions.development;

export const deriveStorageKeys = (
  network: NetworkNames
): CardanoNodeStorageKeys => ({
  PREVIOUS_CARDANO_PID: `${getNetworkName(network)}-PREVIOUS-CARDANO-PID`,
});

export const deriveProcessNames = (platform: PlatformNames): ProcessNames => ({
  CARDANO_PROCESS_NAME: CardanoProcessNameOptions[platform] || 'cardano-node',
});
