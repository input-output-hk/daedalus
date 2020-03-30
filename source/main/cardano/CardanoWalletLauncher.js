// @flow
import { merge } from 'lodash';
import * as fs from 'fs-extra';
import * as cardanoLauncher from 'cardano-launcher';
import type { Launcher } from 'cardano-launcher';
import type { NodeConfig } from '../config';
import { environment } from '../environment';
import { STAKE_POOL_REGISTRY_URL } from '../config';
import {
  MAINNET,
  SELFNODE,
  TESTNET,
  ITN_REWARDS_V1,
  ITN_SELFNODE,
  NIGHTLY,
  QA,
} from '../../common/types/environment.types';
import { createSelfnodeConfig } from './utils';
import { logger } from '../utils/logging';
import type { CardanoNodeImplementation } from '../../common/types/cardano-node.types';

export type WalletOpts = {
  nodeImplementation: CardanoNodeImplementation,
  nodeConfig: NodeConfig,
  cluster: string,
  stateDir: string,
  tlsPath: string,
  block0Path: string,
  block0Hash: string,
  secretPath: string,
  configPath: string,
  syncTolerance: string,
  logFile: any,
  cliBin: string,
};

export async function CardanoWalletLauncher(walletOpts: WalletOpts): Launcher {
  const {
    nodeImplementation,
    nodeConfig, // For cardano-node / byron only!
    cluster,
    stateDir,
    tlsPath,
    block0Path,
    block0Hash,
    secretPath,
    configPath,
    syncTolerance,
    logFile,
    cliBin,
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
    installSignalHandlers: false,
  };

  // Prepare development TLS files
  const { isProduction } = environment;
  if (!isProduction) {
    const devTlsPath = process.env.DEV_TLS_PATH || '';
    try {
      if (!devTlsPath) {
        throw new Error(
          'Daedalus improperly started!\n\nPlease provide DEV_TLS_PATH ENV variable.\n'
        );
      }

      logger.info('Preparing Daedalus development TLS files...');
      await fs.copy(devTlsPath, tlsPath);
      logger.info('Prepared Daedalus development TLS files', {
        devTlsPath,
        tlsPath,
      });
    } catch (error) {
      logger.error('Preparing Daedalus development TLS files failed', {
        devTlsPath,
        tlsPath,
        error,
      });
    }
  }

  // This switch statement handles any node specifc
  // configuration, prior to spawning the child process
  logger.info('Node implementation', { nodeImplementation });
  switch (nodeImplementation) {
    case 'cardano':
      if (cluster === SELFNODE) {
        const { configFile, genesisFile } = nodeConfig.network;
        const {
          configPath: selfnodeConfigPath,
          genesisPath: selfnodeGenesisPath,
          genesisHash: selfnodeGenesisHash,
        } = await createSelfnodeConfig(
          configFile,
          genesisFile,
          stateDir,
          cliBin
        );
        nodeConfig.network.configFile = selfnodeConfigPath;
        nodeConfig.network.genesisFile = selfnodeGenesisPath;
        nodeConfig.network.genesisHash = selfnodeGenesisHash;
        merge(launcherConfig, { apiPort: 8088 });
      }
      if (cluster !== MAINNET) {
        // All clusters except for Mainnet are treated as "Testnets"
        launcherConfig.networkName = TESTNET;
      }
      merge(launcherConfig, { nodeConfig });
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
          stakePoolRegistryUrl: STAKE_POOL_REGISTRY_URL[ITN_SELFNODE],
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

  logger.info('Setting up CardanoLauncher now...', {
    walletOpts,
    launcherConfig,
  });

  return new cardanoLauncher.Launcher(launcherConfig, logger);
}
