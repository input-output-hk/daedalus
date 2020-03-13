/**
 * Configuration for `cardano-node` (Byron)
 *
 * @packageDocumentation
 */
import { StartService } from './service';
import { FilePath, DirPath } from './common';
/** Predefined networks. */
export declare const networks: {
    [propName: string]: ByronNetwork;
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
 * Chooses the command-line arguments for the node.
 *
 * @param stateDir - directory for node storage, specific to the node type and network.
 * @param config - parameters for starting the node.
 * @return the command-line for starting this node.
 */
export declare function startByronNode(stateDir: DirPath, config: ByronNodeConfig): Promise<StartService>;
