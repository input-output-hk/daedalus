// @flow
import { spawn } from 'child_process';
import { dirname } from 'path';
import type { ChildProcess } from 'child_process';
import { STAKE_POOL_REGISTRY_URL } from '../config';
import { environment } from '../environment';
import { NIGHTLY, SELFNODE, QA } from '../../common/types/environment.types';
import { Logger } from '../utils/logging';

export type WalletOpts = {
  path: string,
  walletArgs: string[],
  nodeBin: string,
  nodeImplementation: 'jormungandr' | 'cardano-node',
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
  Logger.info('Node implementation', { nodeImplementation });
  switch (nodeImplementation) {
    case 'cardano-node':
      break;
    case 'jormungandr':
      if (cluster === 'selfnode') {
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

  Logger.info('Starting Node now...', { path, walletArgs });
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
