// @flow
import { merge } from 'lodash';
import path from 'path';
import * as fs from 'fs-extra';
import type { WriteStream } from 'fs';
import * as cardanoLauncher from 'cardano-launcher';
import type { Launcher } from 'cardano-launcher';
import type { NodeConfig } from '../config';
import { environment } from '../environment';
import { STAKE_POOL_REGISTRY_URL } from '../config';
import {
  MAINNET,
  STAGING,
  TESTNET,
  SELFNODE,
  ITN_REWARDS_V1,
  ITN_SELFNODE,
  NIGHTLY,
  QA,
} from '../../common/types/environment.types';
import { CardanoNodeImplementationOptions } from '../../common/types/cardano-node.types';
import { createSelfnodeConfig } from './utils';
import { logger } from '../utils/logging';
import type { CardanoNodeImplementations } from '../../common/types/cardano-node.types';

export type WalletOpts = {
  nodeImplementation: CardanoNodeImplementations,
  nodeConfig: NodeConfig,
  cluster: string,
  stateDir: string,
  tlsPath: string,
  block0Path: string,
  block0Hash: string,
  secretPath: string,
  configPath: string,
  syncTolerance: string,
  nodeLogFile: WriteStream,
  walletLogFile: WriteStream,
  cliBin: string,
  isStaging: boolean,
  smashUrl?: string,
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
    nodeLogFile,
    walletLogFile,
    cliBin,
    isStaging,
    smashUrl,
  } = walletOpts;
  console.log('smashUrl ------------', smashUrl);
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
    childProcessLogWriteStreams: {
      node: nodeLogFile,
      wallet: walletLogFile,
    },
    installSignalHandlers: false,
  };

  // TLS configuration used only for cardano-node
  const tlsConfiguration = {
    caCert: path.join(tlsPath, 'server/ca.crt'),
    svCert: path.join(tlsPath, 'server/server.crt'),
    svKey: path.join(tlsPath, 'server/server.key'),
  };

  // Prepare development TLS files
  const { isProduction } = environment;
  if (
    !isProduction &&
    nodeImplementation === CardanoNodeImplementationOptions.CARDANO
  ) {
    await fs.copy('tls', tlsPath);
  }

  // This switch statement handles any node specific
  // configuration, prior to spawning the child process
  logger.info('Node implementation', { nodeImplementation });
  switch (nodeImplementation) {
    case CardanoNodeImplementationOptions.CARDANO:
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
      if (cluster === MAINNET) {
        launcherConfig.networkName = MAINNET;
        logger.info('Launching Wallet with --mainnet flag');
      } else if (isStaging) {
        launcherConfig.networkName = STAGING;
        logger.info('Launching Wallet with --staging flag');
      } else {
        // All clusters not flagged as staging except for Mainnet are treated as "Testnets"
        launcherConfig.networkName = TESTNET;
        logger.info('Launching Wallet with --testnet flag');
      }
      if (smashUrl) {
        logger.info('Launching Wallet with --pool-metadata-fetching flag', {
          poolMetadataSource: { smashUrl },
        });
        merge(launcherConfig, {
          poolMetadataSource: { smashUrl },
        });
      }
      merge(launcherConfig, { nodeConfig, tlsConfiguration });
      break;
    case CardanoNodeImplementationOptions.JORMUNGANDR:
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
