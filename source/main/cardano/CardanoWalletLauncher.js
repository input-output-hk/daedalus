// @flow
import { spawn } from 'child_process';
import type { ChildProcess } from 'child_process';
import { configureJormungandrDeps } from './nodes';

export type WalletOpts = {
  path: string,
  walletArgs: string[],
  cliBin: string,
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
    cliBin,
    stateDir,
    path,
    walletArgs,
  } = walletOpts;

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
  return spawn(path, walletArgs, { stdio: walletStdio });
}
