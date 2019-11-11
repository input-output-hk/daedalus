// @flow
import { spawn } from 'child_process';
import { dirname } from 'path';
import type { ChildProcess } from 'child_process';
import { configureJormungandrDeps } from './nodes';
import { Logger } from '../utils/logging';

export type WalletOpts = {
  path: string,
  walletArgs: string[],
  cliBin: string,
  nodeBin: string,
  nodeImplementation: 'jormungandr' | 'cardano-node',
  stateDir: string,
  logStream: any,
};

export async function CardanoWalletLauncher(
  walletOpts: WalletOpts
): Promise<ChildProcess> {
  const {
    logStream,
    nodeImplementation,
    nodeBin,
    cliBin,
    stateDir,
    path,
    walletArgs,
  } = walletOpts;

  Logger.info('>>> nodeImplementation 999', {
    nodeImplementation,
  });

  // This switch statement handles any node specifc
  // configuration, prior to spawning the child process
  switch (nodeImplementation) {
    case 'cardano-node':
      break;
    case 'jormungandr':
      // This configuration is for the selfnode only
      // The selfnode is identified by the unique genesis-block wallet arg
      if (walletArgs.findIndex(arg => arg === '--genesis-block') > -1) {
        await configureJormungandrDeps(cliBin, stateDir);
      }
      break;
    default:
      break;
  }

  const walletStdio: string[] = ['inherit', logStream, logStream, 'ipc'];
  const nodePath = dirname(nodeBin);
  const PATH: string = (process.env.PATH: any);

  return spawn(path, walletArgs, {
    stdio: walletStdio,
    env: {
      ...process.env,
      PATH: `${nodePath}:${PATH}`,
    },
  });
}
