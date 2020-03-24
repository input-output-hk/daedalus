// @flow
import * as fs from 'fs-extra';
import path from 'path';
import { spawnSync } from 'child_process';
import { logger } from '../utils/logging';
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

export const createSelfnodeConfig = async (
  configFilePath: string,
  genesisFilePath: string,
  stateDir: string,
  cliBin: string
): Promise<{
  configPath: string,
  genesisPath: string,
  genesisHash: string,
}> => {
  const genesisFileExists = await fs.pathExists(genesisFilePath);
  if (!genesisFileExists) {
    throw new Error('No genesis file found');
  }

  const genesisFileContent = await fs.readFile(genesisFilePath);
  const startTime = Date.now() + 30000;
  const genesisFile = JSON.stringify({
    ...JSON.parse(genesisFileContent),
    startTime,
  });
  const genesisPath = path.join(stateDir, 'genesis.json');

  logger.info('Creating selfnode genesis file...', {
    inputPath: genesisFilePath,
    outputPath: genesisPath,
    startTime,
  });

  await fs.remove(genesisPath);
  await fs.writeFile(genesisPath, genesisFile);

  logger.info('Generating selfnode genesis hash...', { cliBin, genesisPath });
  const { stdout: genesisHashBuffer } = spawnSync(cliBin, [
    'print-genesis-hash',
    '--genesis-json',
    genesisPath,
  ]);
  const genesisHash = genesisHashBuffer
    .toString()
    .replace('\r', '')
    .replace('\n', '');
  logger.info('Generated selfnode genesis hash', { genesisHash });

  const configFileExists = await fs.pathExists(configFilePath);
  if (!configFileExists) {
    throw new Error('No config file found');
  }

  const configFileContent = await fs.readFile(configFilePath);
  const configFile = JSON.stringify({
    ...JSON.parse(configFileContent),
    GenesisFile: genesisPath,
  });
  const configPath = path.join(stateDir, 'config.yaml');

  logger.info('Creating selfnode config file...', {
    inputPath: configFilePath,
    outputPath: configPath,
    genesisPath,
  });

  await fs.remove(configPath);
  await fs.writeFile(configPath, configFile);

  return { configPath, genesisPath, genesisHash };
};
