// @flow
import { indexOf } from 'lodash';
import * as cardanoLauncher from 'cardano-launcher';
import type { Launcher } from 'cardano-launcher';
import { STAKE_POOL_REGISTRY_URL } from '../config';
import { environment } from '../environment';
import { NIGHTLY, SELFNODE, QA } from '../../common/types/environment.types';
import { Logger } from '../utils/logging';

export type WalletOpts = {
  nodeImplementation: 'jormungandr' | 'cardano-node',
  cluster: string,
  stateDir: string,
  block0Path: string,
  block0Hash: string,
  secretPath: string,
  configPath: string,
  walletArgs: string[],
};

export function CardanoWalletLauncher(walletOpts: WalletOpts): Launcher {
  const {
    nodeImplementation,
    cluster,
    stateDir,
    block0Path,
    block0Hash,
    secretPath,
    configPath,
    walletArgs,
  } = walletOpts;
  let stakePoolRegistryUrl = '';

  // This switch statement handles any node specifc
  // configuration, prior to spawning the child process
  Logger.info('Node implementation', { nodeImplementation });
  switch (nodeImplementation) {
    case 'cardano-node':
      break;
    case 'jormungandr':
      if (cluster === SELFNODE) {
        stakePoolRegistryUrl = STAKE_POOL_REGISTRY_URL[SELFNODE];
      }
      if (environment.isIncentivizedTestnetNightly) {
        stakePoolRegistryUrl = STAKE_POOL_REGISTRY_URL[NIGHTLY];
      }
      if (environment.isIncentivizedTestnetQA) {
        stakePoolRegistryUrl = STAKE_POOL_REGISTRY_URL[QA];
      }
      break;
    default:
      break;
  }

  // Extract '--sync-tolerance' from walletArgs
  const syncToleranceSeconds = parseInt(
    walletArgs[indexOf(walletArgs, '--sync-tolerance') + 1].replace('s', ''),
    10
  );

  Logger.info('Setting up CardanoLauncher now...', {
    walletOpts,
    syncToleranceSeconds,
    stakePoolRegistryUrl,
  });

  const launcher: Launcher = new cardanoLauncher.Launcher({
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
    syncToleranceSeconds,
    stakePoolRegistryUrl,
  });

  return launcher;
}
