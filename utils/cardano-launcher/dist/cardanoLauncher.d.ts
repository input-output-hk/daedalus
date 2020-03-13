/**
 * Module for starting and managing a Cardano node and wallet backend.
 *
 * The main class is [[Launcher]].
 *
 * @packageDocumentation
 */
import { EventEmitter } from 'tsee';
import { Logger } from './logging';
import { Service, ServiceExitStatus } from './service';
import * as byron from './byron';
import * as shelley from './shelley';
import * as jormungandr from './jormungandr';
export { ServiceStatus, ServiceExitStatus, serviceExitStatusMessage, Service, } from './service';
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
     * Maximum time difference (in seconds) between the tip slot and the
     * latest applied block within which we consider a wallet being
     * synced with the network. Defaults to 300 seconds.
     */
    syncToleranceSeconds?: number;
    /**
     * Configuration for starting `cardano-node`. The `kind` property will be one of
     *  * `"byron"` - [[ByronNodeConfig]]
     *  * `"shelley"` - [[ShelleyNodeConfig]]
     *  * `"jormungandr"` - [[JormungandrConfig]]
     */
    nodeConfig: byron.ByronNodeConfig | shelley.ShelleyNodeConfig | jormungandr.JormungandrConfig;
}
/**
 * This is the main object which controls the launched wallet backend
 * and its node.
 *
 * Example:
 *
 * ```javascript
 * var launcher = new cardanoLauncher.Launcher({
 *   networkName: "mainnet",
 *   stateDir: "/tmp/state-launcher",
 *   nodeConfig: {
 *     kind: "byron",
 *     configurationDir: "/home/user/cardano-node/configuration",
 *     network: {
 *       configFile: "configuration-mainnet.yaml",
 *       genesisFile: "mainnet-genesis.json",
 *       genesisHash: "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb",
 *       topologyFile: "mainnet-topology.json"
 *     }
 *   }
 * });
 * ```
 *
 * Initially, the backend is not started. Use [[Launcher.start]] for that.
 */
export declare class Launcher {
    /**
     * Use this attribute to monitor and control the `cardano-wallet` process.
     */
    readonly walletService: Service;
    /**
     * Use this to access the `cardano-wallet` API server.
     */
    readonly walletBackend: WalletBackend;
    /**
     * Use this to monitor the `cardano-node` process.
     */
    readonly nodeService: Service;
    /** Logging adapter */
    protected logger: Logger;
    /** Wallet API server port - set once it's known. */
    private apiPort;
    /**
     * Sets up a Launcher which can start and control the wallet backend.
     *
     * @param config - controls how the wallet and node are started
     * @param logger - logging backend that launcher will use
     */
    constructor(config: LaunchConfig, logger?: Logger);
    /**
     * Starts the wallet and node.
     *
     * Example:
     *
     * ```javascript
     * launcher.start().then(function(api) {
     *   console.log("*** cardano-wallet backend is ready, base URL is " + api.baseUrl);
     * });
     * ```
     *
     * @return a promise that will be fulfilled when the wallet API
     * server is ready to accept requests.
     */
    start(): Promise<Api>;
    /**
     * Poll TCP port of wallet API server until it accepts connections.
     * @param port - TCP port number
     * @return a promise that is completed once the wallet API server accepts connections.
     */
    private waitForApi;
    /**
     * Stops the wallet backend. Attempts to cleanly shut down the
     * processes. However, if they have not exited before the timeout,
     * they will be killed.
     *
     * @param timeoutSeconds - how long to wait before killing the processes.
     * @return a [[Promise]] that is fulfilled at the timeout, or before.
     *
     * @event exit - `walletBackend.events` will emit this when the
     *   wallet and node have both exited.
     */
    stop(timeoutSeconds?: number): Promise<{
        wallet: ServiceExitStatus;
        node: ServiceExitStatus;
    }>;
    /**
     * Stop services when this process gets killed.
     */
    private installSignalHandlers;
}
interface RequestParams {
    port: number;
    path: string;
    hostname: string;
}
/**
 * Connection parameters for the `cardano-wallet` API.
 * These should be used to build the HTTP requests.
 */
export interface Api {
    /**
     * API base URL, including trailling slash.
     */
    baseUrl: string;
    /**
     * URL components which can be used with the HTTP client library of
     * your choice.
     */
    requestParams: RequestParams;
}
/**
 * The result after the launched wallet backend has finished.
 */
export interface ExitStatus {
    wallet: ServiceExitStatus;
    node: ServiceExitStatus;
}
/**
 * Format an [[ExitStatus]] as a multiline human-readable string.
 */
export declare function exitStatusMessage(status: ExitStatus): string;
/**
 * Represents the API service of `cardano-wallet`.
 */
export interface WalletBackend {
    /**
     * @return HTTP connection parameters for the `cardano-wallet` API server.
     */
    getApi(): Api;
    /**
     * An [[EventEmitter]] that can be used to register handlers when
     * the process changes status.
     *
     * ```typescript
     * launcher.walletBackend.events.on('ready', api => { ... });
     * ```
     */
    events: WalletBackendEvents;
}
/**
 * The type of events for [[WalletBackend]].
 */
declare type WalletBackendEvents = EventEmitter<{
    /**
     * [[Launcher.walletBackend.events]] will emit this when the API
     *  server is ready to accept requests.
     * @event
     */
    ready: (api: Api) => void;
    /** [[Launcher.walletBackend.events]] will emit this when the
     *  wallet and node have both exited.
     * @event
     */
    exit: (status: ExitStatus) => void;
}>;
