/**
 * Configuration for Jörmungandr.
 *
 * @packageDocumentation
 */
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
export declare const networks: {
    [propName: string]: JormungandrNetwork;
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
export declare function startJormungandr(stateDir: DirPath, config: JormungandrConfig): Promise<StartService>;
