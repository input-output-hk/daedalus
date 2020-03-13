/**
 * Configuration for Jörmungandr.
 *
 * @packageDocumentation
 */

import path from 'path';
import _ from 'lodash';
import getPort from 'get-port';
import { StartService } from './service';
import { FilePath, DirPath } from './common';

/**
 * Pre-defined networks for `jormungandr`. The "self" config is a
 * special one to create a local node.
 *
 * The config files are cached in the `test/data/jormungandr` directory of this repo.
 *
 * Download the latest configs from
 *   https://hydra.iohk.io/job/Cardano/iohk-nix/jormungandr-deployment/latest/download/1/index.html
 *
 */
export const networks: { [propName: string]: JormungandrNetwork } = {
  itn_rewards_v1: {
    configFile: 'itn_rewards_v1-config.yaml',
    genesisBlock: {
      hash: '8e4d2a343f3dcf9330ad9035b3e8d168e6728904262f2c434a4f8f934ec7b676',
    },
  },
  self: {
    configFile: 'config.yaml',
    genesisBlock: {
      file: 'block0.bin',
      hash: 'f8c0622ea4b768421fea136a6e5a4e3b4c328fc5f16fad75817e40c8a2a56a56',
    },
    secretFile: ['secret.yaml'],
  },
};

/**
 * Definition of a Jörmungandr network.
 */
export interface JormungandrNetwork {
  configFile: FilePath;
  genesisBlock: GenesisBlockHash | GenesisBlockFile;
  secretFile?: FilePath[];
}

/**
 * Configuration parameters for starting the node.
 */
export interface JormungandrConfig {
  kind: 'jormungandr';

  /** Directory containing configurations for all networks. */
  configurationDir: DirPath;

  /** Network parameters */
  network: JormungandrNetwork;

  /** Optionally select a port for the node REST API. Otherwise, any unused port is chosen. */
  restPort?: number;

  /**
   * Extra arguments to add to the `jormungandr` command line.
   */
  extraArgs?: string[];
}

export interface GenesisBlockHash {
  hash: string;
}

export interface GenesisBlockFile extends GenesisBlockHash {
  file: string;
}

/**
 * Models the command-line arguments which can be supplied to `jormungandr`.
 */
export interface JormungandrArgs {
  /** Configuration file for the cardano-node. */
  configFile: FilePath;

  /** Directory where the state is stored. */
  storageDir: DirPath;

  genesisBlock: {
    /** The file of the genesis block for this network's chain. */
    file?: FilePath;
    /** The hash of the genesis block for this network's chain. */
    hash?: string;
  };

  /** BFT leaders secrets file(s). */
  secretFile?: FilePath[];

  /** Configures the address to bind for the REST API. */
  restListen?: string;

  /**
   * Extra arguments to add to the `jormungandr` command line.
   */
  extra?: string[];
}
export async function startJormungandr(
  stateDir: DirPath,
  config: JormungandrConfig
): Promise<StartService> {
  if (!config.restPort) {
    config.restPort = await getPort();
  }
  const args = makeArgs(stateDir, config);
  return {
    command: 'jormungandr',
    args: [
      '--config',
      args.configFile,
      '--storage',
      args.storageDir,
      // note: To support log file rotation from jormungandr, capture
      // its logs in json format and echo them into your frontend
      // logging framework (which presumably supports log rotation).
      // This will also mean that the node logs are correctly
      // interleaved with the frontend logs.
      // "--log-format", "json",
    ]
      .concat(args.restListen ? ['--rest-listen', args.restListen] : [])
      .concat(
        args.genesisBlock.file
          ? ['--genesis-block', args.genesisBlock.file]
          : args.genesisBlock.hash
          ? ['--genesis-block-hash', args.genesisBlock.hash]
          : []
      )
      .concat(_.flatMap(args.secretFile || [], secret => ['--secret', secret]))
      .concat(args.extra || []),
    supportsCleanShutdown: false,
  };
}

function makeArgs(
  stateDir: DirPath,
  config: JormungandrConfig
): JormungandrArgs {
  return {
    configFile: path.join(config.configurationDir, config.network.configFile),
    restListen: `127.0.0.1:${config.restPort || 0}`,
    genesisBlock: {
      file:
        'file' in config.network.genesisBlock
          ? path.join(config.configurationDir, config.network.genesisBlock.file)
          : undefined,
      hash:
        'hash' in config.network.genesisBlock
          ? config.network.genesisBlock.hash
          : undefined,
    },
    storageDir: path.join(stateDir, 'chain'),
    secretFile: _.map(config.network.secretFile || [], secret =>
      path.join(config.configurationDir, secret)
    ),
    extra: config.extraArgs,
  };
}
