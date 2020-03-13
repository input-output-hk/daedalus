/**
 * Configuration for `cardano-node` (Byron)
 *
 * @packageDocumentation
 */

import path from 'path';
import getPort from 'get-port';

import { StartService } from './service';
import { FilePath, DirPath } from './common';

/** Predefined networks. */
export const networks: { [propName: string]: ByronNetwork } = {
  mainnet: {
    configFile: 'configuration-mainnet.yaml',
    genesisFile: 'mainnet-genesis.json',
    genesisHash:
      '5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb',
    topologyFile: 'mainnet-topology.json',
  },
};

/**
 * Definition of a `cardano-node` (Byron) network.
 */
export interface ByronNetwork {
  configFile: FilePath;
  genesisFile: FilePath;
  genesisHash: string;
  topologyFile: FilePath;
}

/**
 * Configuration parameters for starting the rewritten version of
 * cardano-node (Byron).
 */
export interface ByronNodeConfig {
  kind: 'byron';

  /** Directory containing configurations for all networks. */
  configurationDir: DirPath;

  /** Network parameters */
  network: ByronNetwork;

  /**
   * Directory which will contain a socket file to use for
   * communicating with the node. Optional -- will be set
   * automatically if not provided.
   */
  socketDir?: DirPath;
}

/**
 * The command-line arguments which can be supplied to `cardano-node` (Byron).
 */
export interface ByronNodeArgs {
  /**
   * Directory which will contain a socket file to use for
   * communicating with the node.
   */
  socketDir: DirPath;

  /**
   * The path to a file describing the topology.
   * Topology is ...
   */
  topologyFile: FilePath;

  /** Directory where the state is stored. */
  databaseDir: DirPath;

  /** Path to the delegation certificate. */
  delegationCertificate?: string;

  /** Path to the signing key. */
  signingKey?: string;

  /** The genesis block for this network's chain. */
  genesis: {
    /** The filename of the genesis block. */
    file: FilePath;
    /** The hash of the genesis block. */
    hash: string;
  };

  /** Configures the address to bind for P2P communication. */
  listen: {
    /** The TCP port for node P2P. */
    port: number;
    /** Optionally limit node P2P to one ipv6 or ipv4 address. */
    address?: string;
  };

  /** Configuration file for the cardano-node. */
  configFile: FilePath;

  /** Validate all on-disk database files. */
  validateDb?: boolean;

  /**
   * Extra arguments to add to the `cardano-node` command line.
   */
  extra?: string[];
}

/**
 * Convert a [[ByronNodeConfig]] into command-line arguments
 * ([[ByronNodeArgs]]) for `cardano-node`.
 */
function makeArgs(
  stateDir: DirPath,
  config: ByronNodeConfig,
  listenPort: number
): ByronNodeArgs {
  if (!config.socketDir) {
    config.socketDir = 'sockets'; // relative to working directory
  }
  return {
    socketDir: config.socketDir,
    topologyFile: path.join(
      config.configurationDir,
      config.network.topologyFile
    ),
    databaseDir: 'chain', // relative to working directory
    genesis: {
      file: path.join(config.configurationDir, config.network.genesisFile),
      hash: config.network.genesisHash,
    },
    listen: {
      port: listenPort,
    },
    configFile: path.join(config.configurationDir, config.network.configFile),
  };
}

/**
 * Chooses the command-line arguments for the node.
 *
 * @param stateDir - directory for node storage, specific to the node type and network.
 * @param config - parameters for starting the node.
 * @return the command-line for starting this node.
 */
export async function startByronNode(
  stateDir: DirPath,
  config: ByronNodeConfig
): Promise<StartService> {
  const listenPort = await getPort();
  const args = makeArgs(stateDir, config, listenPort);
  return {
    command: 'cardano-node',
    args: [
      '--socket-dir',
      args.socketDir,
      '--topology',
      args.topologyFile,
      '--database-path',
      args.databaseDir,
      '--genesis-file',
      args.genesis.file,
      '--genesis-hash',
      args.genesis.hash,
      '--port',
      '' + args.listen.port,
      '--config',
      args.configFile,
    ]
      .concat(args.listen.address ? ['--host-addr', args.listen.address] : [])
      .concat(args.validateDb || false ? ['--validate-db'] : [])
      .concat(args.signingKey ? ['--signing-key', args.signingKey] : [])
      .concat(
        args.delegationCertificate
          ? ['--delegation-certificate', args.delegationCertificate]
          : []
      )
      .concat(args.extra || []),
    supportsCleanShutdown: false,
    // set working directory to stateDir -- config file may have relative paths for logs.
    cwd: stateDir,
  };
}
