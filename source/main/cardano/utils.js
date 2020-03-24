// @flow
import * as fs from 'fs-extra';
import path from 'path';
import { spawn } from 'child_process';
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

export const createSelfnodeGenesisFile = async (
  genesisFilePath: string,
  stateDir: string,
  cliBin: string
): Promise<{
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
  const genesisHash = await new Promise((resolve, reject) => {
    const genesisHashGenerator = spawn(cliBin, [
      'print-genesis-hash',
      '--genesis-json',
      genesisPath,
    ]);

    let stdoutData = '';
    genesisHashGenerator.stdout.on('data', data => {
      stdoutData += data;
    });

    let stderrData = '';
    genesisHashGenerator.stderr.on('data', data => {
      stderrData += data;
    });

    genesisHashGenerator.on('exit', (exitCode, signal) => {
      logger.info('Generating selfnode genesis SUCCESS', {
        exitCode,
        signal,
        result: stdoutData,
      });
      resolve(stdoutData);
    });

    genesisHashGenerator.on('error', error => {
      logger.error('Generating selfnode genesis ERROR', {
        error,
        data: stderrData,
      });
      reject(new Error(error));
    });
  });
  logger.info('Generated selfnode genesis hash', { genesisHash });

  return { genesisPath, genesisHash };
};
