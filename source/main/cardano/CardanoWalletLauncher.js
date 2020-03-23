// @flow
import { merge } from 'lodash';
import * as cardanoLauncher from 'cardano-launcher';
import type { Launcher } from 'cardano-launcher';
import type { NodeConfig } from '../config';
import { STAKE_POOL_REGISTRY_URL } from '../config';
import {
  NIGHTLY,
  SELFNODE,
  QA,
  ITN_REWARDS_V1,
  ITN_SELFNODE,
} from '../../common/types/environment.types';
import { writeGenesisFile } from './utils';
import { Logger } from '../utils/logging';
import type { CardanoNodeImplementation } from '../../common/types/cardano-node.types';

export type WalletOpts = {
  nodeImplementation: CardanoNodeImplementation,
  nodeConfig: NodeConfig,
  cluster: string,
  stateDir: string,
  block0Path: string,
  block0Hash: string,
  secretPath: string,
  configPath: string,
  syncTolerance: string,
  logFile: any,
};

export async function CardanoWalletLauncher(walletOpts: WalletOpts): Launcher {
  const {
    nodeImplementation,
    nodeConfig, // For cardano-node / byron only!
    cluster,
    stateDir,
    block0Path,
    block0Hash,
    secretPath,
    configPath,
    syncTolerance,
    logFile,
  } = walletOpts;
  // TODO: Update launcher config to pass number
  const syncToleranceSeconds = parseInt(syncTolerance.replace('s', ''), 10);

  // Shared launcher config (node implementations agnostic)
  const launcherConfig = {
    networkName: cluster,
    stateDir,
    nodeConfig: {
      kind: nodeImplementation,
      configurationDir: '',
      network: {
        configFile: configPath,
      },
    },
    syncToleranceSeconds,
    childProcessLogWriteStream: logFile,
  };

  // This switch statement handles any node specifc
  // configuration, prior to spawning the child process
  Logger.info('Node implementation', { nodeImplementation });
  switch (nodeImplementation) {
    case 'cardano':
      merge(launcherConfig, { nodeConfig });
      if (cluster === SELFNODE) {
        await writeGenesisFile(stateDir);
      }
      break;
    case 'jormungandr':
      if (cluster === ITN_SELFNODE) {
        merge(launcherConfig, {
          apiPort: 8088,
          networkName: SELFNODE,
          nodeConfig: {
            restPort: 8888,
            network: {
              genesisBlock: {
                file: block0Path,
                hash: block0Hash,
              },
              secretFile: [secretPath],
            },
          },
          stakePoolRegistryUrl: STAKE_POOL_REGISTRY_URL[SELFNODE],
        });
      }
      if (cluster === NIGHTLY) {
        merge(launcherConfig, {
          nodeConfig: {
            network: {
              genesisBlock: {
                hash: block0Hash,
              },
            },
          },
          stakePoolRegistryUrl: STAKE_POOL_REGISTRY_URL[NIGHTLY],
        });
      }
      if (cluster === QA) {
        merge(launcherConfig, {
          nodeConfig: {
            network: {
              genesisBlock: {
                hash: block0Hash,
              },
            },
          },
          stakePoolRegistryUrl: STAKE_POOL_REGISTRY_URL[QA],
        });
      }
      if (cluster === ITN_REWARDS_V1) {
        merge(launcherConfig, {
          nodeConfig: {
            network: {
              genesisBlock: {
                file: block0Path,
                hash: block0Hash,
              },
            },
          },
        });
      }
      break;
    default:
      break;
  }

  Logger.info('Setting up CardanoLauncher now...', {
    walletOpts,
    launcherConfig,
  });

  return new cardanoLauncher.Launcher(launcherConfig, Logger);
}
