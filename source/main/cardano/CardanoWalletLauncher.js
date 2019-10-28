// @flow
import { spawn } from 'child_process';
import type { ChildProcess } from 'child_process';
import { configureJormungandrDeps } from './nodes';

export type WalletOpts = {
  path: string,
  cliPath: string,
  nodeImplementation: 'jormungandr' | 'cardano-node',
  stateDir: string,
  launcherArgs: string[],
  logStream: any,
};

export async function CardanoWalletLauncher(
  walletOpts: WalletOpts
): Promise<ChildProcess> {
  const {
    logStream,
    nodeImplementation,
    cliPath,
    stateDir,
    path,
    launcherArgs,
  } = walletOpts;

  // This switch statement handles any node specifc
  // configuration, prior to spawning the child process
  switch (nodeImplementation) {
    case 'cardano-node':
      break;
    case 'jormungandr':
      await configureJormungandrDeps(cliPath, stateDir);
      break;
    default:
      break;
  }

  const walletStdio: string[] = ['inherit', logStream, logStream, 'ipc'];
  return spawn(path, launcherArgs, { stdio: walletStdio });
}
