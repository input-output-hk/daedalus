// @flow
import {
  CardanoProcessNameOptions,
  NetworkNameOptions
} from '../../common/types/cardanoNode.types';
import type {
  CardanoNodeStorageKeys,
  ProcessNames,
  NetworkNames,
  PlatformNames
} from '../../common/types/cardanoNode.types';

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
