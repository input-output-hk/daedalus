// @flow
import * as fs from 'fs-extra';
import path from 'path';
import { Logger } from '../utils/logging';
import type {
  CardanoNodeStorageKeys,
  CardanoNodeImplementation,
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
  const result = await condition();
  if (result) {
    resolve();
  } else if (timeWaited >= timeout) {
    reject(`Promised condition not met within ${timeout}ms.`);
  } else {
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

export const deriveProcessNames = (
  platform: PlatformNames,
  nodeImplementation: CardanoNodeImplementation
): ProcessNames => ({
  CARDANO_PROCESS_NAME:
    CardanoProcessNameOptions[nodeImplementation][platform] ||
    (nodeImplementation === 'jormungandr'
      ? 'cardano-wallet-jormungandr'
      : 'cardano-wallet-byron'),
});

export const writeGenesisFile = async (stateDir: string) => {
  const installDir = process.env.DAEDALUS_INSTALL_DIRECTORY || '';
  const genesisDefaultPath = 'utils/cardano/selfnode/genesis.json';
  const genesisInstalledPath = path.join(installDir, 'genesis.json');

  // dev path is ./
  const genesisJsonPath =
    installDir.length > 2 ? genesisInstalledPath : genesisDefaultPath;

  Logger.info('Genesis file path', { genesisJsonPath });

  const networkGenesisFileExists = await fs.pathExists(genesisJsonPath);
  if (!networkGenesisFileExists) {
    throw new Error(`No genesis file exists for testnet`);
  }

  const genesisFileRaw = await fs.readFile(genesisJsonPath);
  const startTime = Date.now() + 30000;
  const genesisFile = JSON.stringify({
    ...JSON.parse(genesisFileRaw),
    startTime,
  });
  const genesisPath = path.join(stateDir, 'genesis.json');

  await fs.remove(genesisPath);
  await fs.writeFile(genesisPath, genesisFile);
};
