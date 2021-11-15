// @flow
import { merge } from 'lodash';
import path from 'path';
import * as fs from 'fs-extra';
import type { WriteStream } from 'fs';
import * as cardanoLauncher from 'cardano-launcher';
import type { Launcher } from 'cardano-launcher';
import type { NodeConfig } from '../config';
import { environment } from '../environment';
import {
  FALLBACK_TOKEN_METADATA_SERVER_URL,
  MOCK_TOKEN_METADATA_SERVER_URL,
  MOCK_TOKEN_METADATA_SERVER_PORT,
} from '../config';
import {
  MAINNET,
  STAGING,
  TESTNET,
  SELFNODE,
} from '../../common/types/environment.types';
import { CardanoNodeImplementationOptions } from '../../common/types/cardano-node.types';
import { createSelfnodeConfig } from './utils';
import { logger } from '../utils/logging';
import type { CardanoNodeImplementations } from '../../common/types/cardano-node.types';

export type WalletOptions = {
  nodeImplementation: CardanoNodeImplementations,
  nodeConfig: NodeConfig,
  cluster: string,
  stateDir: string,
  tlsPath: string,
  configPath: string,
  syncTolerance: string,
  nodeLogFile: WriteStream,
  walletLogFile: WriteStream,
  cliBin: string,
  isStaging: boolean,
  metadataUrl?: string,
};

export async function CardanoWalletLauncher(
  walletOptions: WalletOptions
): Launcher {
  const {
    nodeImplementation,
    nodeConfig, // For cardano-node / byron only!
    cluster,
    stateDir,
    tlsPath,
    configPath,
    syncTolerance,
    nodeLogFile,
    walletLogFile,
    cliBin,
    isStaging,
    metadataUrl,
  } = walletOptions;
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

  let tokenMetadataServer;

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
      if (MOCK_TOKEN_METADATA_SERVER_PORT) {
        tokenMetadataServer = `${MOCK_TOKEN_METADATA_SERVER_URL}:${MOCK_TOKEN_METADATA_SERVER_PORT}`;
      } else if (metadataUrl) {
        tokenMetadataServer = metadataUrl;
      } else {
        tokenMetadataServer = FALLBACK_TOKEN_METADATA_SERVER_URL;
      }
      logger.info('Launching Wallet with --token-metadata-server flag', {
        tokenMetadataServer,
      });
      merge(launcherConfig, {
        nodeConfig,
        tlsConfiguration,
        tokenMetadataServer,
      });
      break;
    default:
      break;
  }

  logger.info('Setting up CardanoLauncher now...', {
    walletOptions,
    launcherConfig,
  });

  return new cardanoLauncher.Launcher(launcherConfig, logger);
}
