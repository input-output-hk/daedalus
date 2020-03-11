// @flow
// import { spawn } from 'child_process';
import { dirname } from 'path';
import type { ChildProcess } from 'child_process';
import { STAKE_POOL_REGISTRY_URL } from '../config';
import { environment } from '../environment';
import { NIGHTLY, SELFNODE, QA } from '../../common/types/environment.types';
import { Logger } from '../utils/logging';

const cardanoLauncher = require('cardano-launcher');

export type WalletOpts = {
  path: string,
  walletArgs: string[],
  nodeBin: string,
  nodeImplementation: 'jormungandr' | 'cardano-node',
  logStream: any,
  stateDir: string,
  cluster: string,
  block0Path: string,
  block0Hash: string,
  secretPath: string,
  configPath: string,
};

export async function CardanoWalletLauncher(
  walletOpts: WalletOpts
): Promise<ChildProcess> {
  const {
    // logStream,
    nodeImplementation,
    nodeBin,
    path,
    walletArgs,
    stateDir,
    cluster,
    block0Path,
    block0Hash,
    secretPath,
    configPath,
  } = walletOpts;
  // const walletStdio: string[] = ['pipe', 'pipe', 'pipe', 'ipc'];
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

  Logger.info('Starting Node now...', { path, walletArgs, walletOpts });

  const launcher = new cardanoLauncher.Launcher({
    apiPort: 8088, // Remove for auto-port selection
    networkName: cluster,
    stateDir,
    nodeConfig: {
      kind: nodeImplementation,
      restPort: 8888, // Remove for auto-port selection
      configurationDir: '',
      network: {
        configFile: configPath,
        genesisBlock: {
          file: block0Path,
          hash: block0Hash,
        },
        secretFile: [secretPath],
      },
    },
    // TODO: pass in --sync-tolerance 600s
  });

  return launcher;

  /*

    'path': './/cardano-wallet-jormungandr',
    'walletArgs': [
      'launch',
      '--node-port',
      '8888',
      '--port',
      '8088',
      '--state-dir',
      '/Users/nikola/Library/Application Support/Daedalus SelfNode',
      '--genesis-block',
      '/nix/store/hcx534mzjda78jhqb66ch1533jwfky93-selfnode-block0.bin',
      '--sync-tolerance',
      '600s',
      '--',
      '--secret',
      '/nix/store/hbmpys0dkg05plmjr84v7yrf5lvv7cis-secret.yaml',
      '--config',
      '/nix/store/hw302adv0mahkia7xd65ap42v6kz16k3-config.yaml'
    ]

  */

  // Logger.info('Starting Node now...', { path, walletArgs });
  // const childProcess = spawn(path, walletArgs, {
  //   stdio: walletStdio,
  //   env: {
  //     // $FlowFixMe
  //     ...process.env,
  //     ...envVariables,
  //   },
  // });
  //
  // childProcess.stdout.on('data', data => logStream.write(data));
  // childProcess.stderr.on('data', data => logStream.write(data));
  //
  // return childProcess;
}
