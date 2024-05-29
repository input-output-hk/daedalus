// Copyright © 2020 IOHK
// License: Apache-2.0

/**
 * Module for starting and managing the cardano-wallet process.
 *
 * The main function is [[cardanoWalletStartService]].
 *
 * @packageDocumentation
 */

import path from 'path';
import getPort from 'get-port';

import _ from 'lodash';

import { WriteStream } from 'fs';
import { CardanoNodeConfig } from './cardanoNode';
import { ServerTlsConfiguration } from './tls';
import { StartService, ShutdownMethod } from './service';
import { DirPath } from './common';

/*******************************************************************************
 * Configuration
 ******************************************************************************/

/**
 * How to fetch stake pool metadata.
 *   - `'none'` - disables stake pool metadata.
 *   - `'direct'` - enables fetching stake pool metadata from
 *     the registered pool URL.
 *   - otherwise, the URL of a SMASH metadata proxy server
 *     can be supplied. This must not contain the endpoint path.
 */
export type PoolMetadataSource = 'none' | 'direct' | { smashUrl: string };

/**
 * Configuration parameters for starting the wallet backend and node.
 */
export interface LaunchConfig {
  /**
   * Directory to store wallet databases, the blockchain, socket
   * files, etc.
   */
  stateDir: string;

  /**
   * Label for the network that will connected. This is used in the
   * state directory path name.
   */
  networkName: string;

  /**
   * TCP port to use for the `cardano-wallet` API server.
   * The default is to select any free port.
   */
  apiPort?: number;

  /**
   * IP address or hostname to bind the `cardano-wallet` API server
   * to. Can be an IPv[46] address, hostname, or `'*'`. Defaults to
   * 127.0.0.1.
   */
  listenAddress?: string;

  /**
   * Overrides the URL to the zip file containing stake pool metadata
   * which is downloaded by cardano-wallet.
   *
   * This is only useful in testing scenarios, or when running a local
   * development testnet.
   *
   * For Jörmungandr ITN, the default is
   * https://github.com/cardano-foundation/incentivized-testnet-stakepool-registry/archive/master.zip.
   */
  stakePoolRegistryUrl?: string;

  /**
   * Stake pool metadata fetching strategy.
   */
  poolMetadataSource?: PoolMetadataSource;

  /**
   * Token metadata server URL.
   */
  tokenMetadataServer?: string;

  /**
   * Maximum time difference (in seconds) between the tip slot and the
   * latest applied block within which we consider a wallet being
   * synced with the network. Defaults to 300 seconds.
   */
  syncToleranceSeconds?: number;

  /**
   * Configuration for starting `cardano-node`.
   */
  nodeConfig: CardanoNodeConfig;

  /**
   *  WriteStreams for the child process data events from stdout and stderr
   */
  childProcessLogWriteStreams?: {
    node: WriteStream;
    wallet: WriteStream;
  };

  /**
   *  Control the termination signal handling. Set this to false if the default
   *  behaviour interferes with your application shutdown behaviour.
   *  If setting this to false, ensure stop(0) is called as part of the shutdown.
   */
  installSignalHandlers?: boolean;

  /**
   * Paths to server TLS credentials for establishing a HTTPS connection using TLS
   * If not set, the connection will be served insecurely over HTTP.
   */
  tlsConfiguration?: ServerTlsConfiguration;
}

/*******************************************************************************
 * Starting the wallet
 ******************************************************************************/

export interface WalletStartService extends StartService {
  /** This will contain the port which the API is listening on,
   *  after the service has started. */
  apiPort: number;
}

/**
 * Produces a [[WalletStartService]] description of how to start
 * `cardano-wallet` with a given [[LaunchConfig]].
 *
 * Does not actually start the wallet.
 *
 * @param baseDir - The state directory, under which `cardano-wallet`
 *   shall store its databases.
 * @param config - Parameters for starting the server.
 * @return The launch instructions.
 */
export async function cardanoWalletStartService(
  baseDir: DirPath,
  config: LaunchConfig
): Promise<WalletStartService> {
  const apiPort = config.apiPort || (await getPort());
  const commandSuffix =
    config.nodeConfig.kind === 'shelley' ? '' : '-' + config.nodeConfig.kind;
  const base: WalletStartService = {
    command: 'cardano-wallet' + commandSuffix,
    args: [
      'serve',
      '+RTS',
      '-N',
      '-RTS',
      '--shutdown-handler',
      '--port',
      '' + apiPort,
      '--database',
      path.join(baseDir, 'wallets'),
    ].concat(
      config.listenAddress ? ['--listen-address', config.listenAddress] : [],
      config.tlsConfiguration
        ? [
            '--tls-ca-cert',
            config.tlsConfiguration.caCert,
            '--tls-sv-cert',
            config.tlsConfiguration.svCert,
            '--tls-sv-key',
            config.tlsConfiguration.svKey,
          ]
        : [],
      config.poolMetadataSource
        ? [
            '--pool-metadata-fetching',
            typeof config.poolMetadataSource === 'string'
              ? config.poolMetadataSource
              : config.poolMetadataSource.smashUrl,
          ]
        : [],
      config.tokenMetadataServer
        ? ['--token-metadata-server', config.tokenMetadataServer]
        : [],
      config.syncToleranceSeconds
        ? ['--sync-tolerance', `${config.syncToleranceSeconds}s`]
        : []
    ),
    extraEnv: config.stakePoolRegistryUrl
      ? {
          CARDANO_WALLET_STAKE_POOL_REGISTRY_URL: config.stakePoolRegistryUrl,
        }
      : undefined,
    shutdownMethod: ShutdownMethod.CloseStdin,
    apiPort,
  };
  const addArgs = (args: string[]): WalletStartService =>
    _.assign(base, { args: base.args.concat(args) });

  switch (config.nodeConfig.kind) {
    default:
      if (
        config.networkName !== 'mainnet' &&
        !config.nodeConfig.network.genesisFile
      ) {
        throw new Error('genesisFile must be configured');
      }

      const genesisArg =
        config.networkName == 'mainnet'
          ? ''
          : path.join(
              config.nodeConfig.configurationDir,
              config.nodeConfig.network.genesisFile as string
            );

      let networkArg;
      switch (config.networkName) {
        case 'mainnet':
          networkArg = ['--mainnet'];
          break;

        case 'staging':
          networkArg = ['--staging', genesisArg];
          break;

        default:
          networkArg = ['--testnet', genesisArg];
          break;
      }

      return addArgs(
        networkArg.concat(
          config.nodeConfig.socketFile
            ? ['--node-socket', config.nodeConfig.socketFile]
            : []
        )
      );
  }
}
