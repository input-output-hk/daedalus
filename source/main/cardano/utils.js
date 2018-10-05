// @flow
import {
  DaedalusProcessNameOptions,
  CardanoProcessNameOptions,
  NetworkNameOptions
} from '../../common/types/cardanoNode.types';
import type {
  CardanoNodeStorageKeys,
  ProcessNames,
  NetworkNames
} from '../../common/types/cardanoNode.types';

const checkCondition = async (
  condition: () => boolean,
  timeout: number,
  retryEvery: number,
  timeWaited: number = 0
): Promise<void> => {
  const result = await condition();
  if (!result) {
    if (timeWaited >= timeout) {
      throw new Error('Condition not met in time');
    } else {
      setTimeout(() => checkCondition(
        condition, timeout, retryEvery, timeWaited + retryEvery
      ), retryEvery);
    }
  }
};

export const promisedCondition = async (
  cond: Function, timeout: number = 5000, retryEvery: number = 1000
): Promise<void> => await checkCondition(cond, timeout, retryEvery);

const getNetworkName = (network: NetworkNames): string => (
  NetworkNameOptions[network] || NetworkNameOptions.development
);

export const deriveStorageKeys = (network: NetworkNames): CardanoNodeStorageKeys => ({
  PREVIOUS_CARDANO_PID: `${getNetworkName(network)}-PREVIOUS-CARDANO-PID`
});

export const deriveProcessNames = (network: NetworkNames): ProcessNames => ({
  DAEDALUS_PROCESS_NAME: DaedalusProcessNameOptions[network] || 'Electron',
  CARDANO_PROCESS_NAME: CardanoProcessNameOptions[network] || 'cardano-node'
});
