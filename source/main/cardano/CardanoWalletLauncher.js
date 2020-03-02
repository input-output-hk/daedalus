// @flow
import { spawn } from 'child_process';
import { dirname } from 'path';
import type { ChildProcess } from 'child_process';
import { configureJormungandrDeps } from './nodes';
import { STAKE_POOL_REGISTRY_URL } from '../config';
import { environment } from '../environment';
import { NIGHTLY, SELFNODE, QA } from '../../common/types/environment.types';

export type WalletOpts = {
  path: string,
  walletArgs: string[],
  cliBin: string,
  nodeBin: string,
  nodeImplementation: 'jormungandr' | 'cardano-node',
  stateDir: string,
  logStream: any,
  cluster: string,
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
    cluster,
  } = walletOpts;
  const walletStdio: string[] = ['pipe', 'pipe', 'pipe', 'ipc'];
  const nodePath = dirname(nodeBin);
  const PATH: string = (process.env.PATH: any);
  const envVariables: $Exact<{
    PATH: string,
    CARDANO_WALLET_STAKE_POOL_REGISTRY_URL?: string,
  }> = {
    PATH: `${nodePath}:${PATH}`,
  };

  // This switch statement handles any node specifc
  // configuration, prior to spawning the child process
  switch (nodeImplementation) {
    case 'cardano-node':
      break;
    case 'jormungandr':
      // This configuration is for the selfnode only
      // The selfnode is identified by the unique genesis-block wallet arg
      if (cluster === 'selfnode') {
        await configureJormungandrDeps(cliBin, stateDir);
        Object.assign(envVariables, {
          CARDANO_WALLET_STAKE_POOL_REGISTRY_URL:
            STAKE_POOL_REGISTRY_URL[SELFNODE],
        });
      }
      if (environment.isIncentivizedTestnetNightly) {
        Object.assign(envVariables, {
          CARDANO_WALLET_STAKE_POOL_REGISTRY_URL:
            STAKE_POOL_REGISTRY_URL[NIGHTLY],
        });
      }
      if (environment.isIncentivizedTestnetQA) {
        Object.assign(envVariables, {
          CARDANO_WALLET_STAKE_POOL_REGISTRY_URL: STAKE_POOL_REGISTRY_URL[QA],
        });
      }
      break;
    default:
      break;
  }

  const childProcess = spawn(path, walletArgs, {
    stdio: walletStdio,
    env: {
      // $FlowFixMe
      ...process.env,
      ...envVariables,
    },
  });

  childProcess.stdout.on('data', data => logStream.write(data));
  childProcess.stderr.on('data', data => logStream.write(data));

  return childProcess;
}
