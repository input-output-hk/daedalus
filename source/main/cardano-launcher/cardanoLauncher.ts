// Copyright © 2020 IOHK
// License: Apache-2.0

/**
 * Module for starting and managing a Cardano node and wallet backend.
 *
 * The main class is [[Launcher]].
 *
 * @packageDocumentation
 */

import { mkdirp } from 'mkdirp';
import process from 'process';
import net from 'net';

import _ from 'lodash';
import { EventEmitter } from 'tsee';

import { Logger, prependName } from './logging';
import {
  Service,
  ServiceExitStatus,
  ServiceStatus,
  StartService,
  setupService,
  serviceExitStatusMessage,
  defaultTimeoutSeconds,
} from './service';
import {
  DirPath,
  passthroughErrorLogger,
  ignorePromiseRejection,
} from './common';
import {
  LaunchConfig,
  cardanoWalletStartService,
  WalletStartService,
} from './cardanoWallet';

import { startCardanoNode } from './cardanoNode';

export {
  ServiceStatus,
  ServiceExitStatus,
  serviceExitStatusMessage,
  Service,
} from './service';
export { LaunchConfig } from './cardanoWallet';

/*******************************************************************************
 * Api
 ******************************************************************************/

export interface RequestParams {
  port: number;
  path: string;
  hostname: string;
  protocol: string;
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

class V2Api implements Api {
  /** URL of the API, including a trailing slash. */
  readonly baseUrl: string;
  /** URL components which can be used with the HTTP client library of
   * your choice. */
  readonly requestParams: RequestParams;

  constructor(port: number, protocol = 'http:') {
    const hostname = '127.0.0.1';
    const path = '/v2/';
    this.baseUrl = `${protocol}//${hostname}:${port}${path}`;
    this.requestParams = { port, path, hostname, protocol };
  }
}

/*******************************************************************************
 * Exit status types
 ******************************************************************************/

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
export function exitStatusMessage(status: ExitStatus): string {
  return _.map(status, serviceExitStatusMessage).join('\n');
}

/**
 * This instance of [[Error]] will be returned when the
 * `Launcher.start()` promise is rejected.
 */
export class BackendExitedError extends Error {
  status: ExitStatus;
  constructor(status: ExitStatus) {
    super(exitStatusMessage(status));
    Object.setPrototypeOf(this, new.target.prototype);
    this.status = status;
  }
}

function noop(): void {
  /* empty */
}

/*******************************************************************************
 * Launching
 ******************************************************************************/

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
 *     kind: "shelley",
 *     configurationDir: "/home/user/cardano-node/configuration/defaults/mainnet",
 *     network: {
 *       configFile: "configuration.yaml",
 *       topologyFile: "topology.json"
 *     }
 *   }
 *   childProcessLogWriteStream: fs.createWriteStream('./logs')
 * });
 * ```
 *
 * Initially, the backend is not started. Use [[Launcher.start]] for that.
 */
export class Launcher {
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
  private apiPort = 0;

  /** A state flag for whether the backend services have exited yet. */
  private exited = false;

  /** Removes process signal handlers, if they were installed. */
  private cleanupSignalHandlers: () => void = noop;

  /**
   * Sets up a Launcher which can start and control the wallet backend.
   *
   * @param config - controls how the wallet and node are started
   * @param logger - logging backend that launcher will use
   */
  constructor(config: LaunchConfig, logger: Logger = console) {
    logger.debug('Launcher init');
    const {
      childProcessLogWriteStreams,
      installSignalHandlers = true,
    } = config;
    this.logger = logger;

    const start = Launcher.makeServiceCommands(config, logger);

    this.walletService = setupService(
      start.wallet,
      prependName(logger, 'wallet'),
      childProcessLogWriteStreams?.wallet
    );
    this.nodeService = setupService(
      start.node,
      prependName(logger, 'node'),
      childProcessLogWriteStreams?.node
    );

    this.walletBackend = new WalletBackend(
      new V2Api(
        this.apiPort,
        config.tlsConfiguration !== undefined ? 'https:' : 'http:'
      )
    );

    start.wallet
      .then((startService: WalletStartService) => {
        this.apiPort = startService.apiPort;
        this.walletBackend.api = new V2Api(
          this.apiPort,
          config.tlsConfiguration !== undefined ? 'https:' : 'http:'
        );
      })
      .catch(passthroughErrorLogger);

    this.walletService.events.on('statusChanged', (status) => {
      if (status === ServiceStatus.Stopped) {
        this.logger.debug('wallet exited');
        this.stop().catch(passthroughErrorLogger);
      }
    });

    this.nodeService.events.on('statusChanged', (status) => {
      if (status === ServiceStatus.Stopped) {
        this.logger.debug('node exited');
        this.stop().catch(passthroughErrorLogger);
      }
    });

    if (installSignalHandlers) this.installSignalHandlers();
  }

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
  start(): Promise<Api> {
    const stopWaiting = (): boolean =>
      this.nodeService.getStatus() > ServiceStatus.Started ||
      this.walletService.getStatus() > ServiceStatus.Started;

    return new Promise((resolve, reject) => {
      this.nodeService.start().catch(ignorePromiseRejection);
      this.walletService.start().catch(ignorePromiseRejection);

      this.waitForApi(stopWaiting, () => {
        this.walletBackend.events.ready(this.walletBackend.api);
      });

      this.walletBackend.events.on('ready', resolve);
      this.walletBackend.events.on('exit', (st) =>
        reject(new BackendExitedError(st))
      );
    });
  }

  /**
   * Poll TCP port of wallet API server until it accepts connections.
   *
   * @param stop - a callback, which will terminate the polling loop
   *   if it returns a truey value.
   *
   * @param ready - a callback which is called once the wallet API
   *   server accepts connections.
   */
  private waitForApi(stop: () => boolean, ready: () => void): void {
    this.logger.debug('waitForApi');

    let addr: net.SocketConnectOpts;
    let client: net.Socket;
    const timer = setInterval(() => {
      if (stop()) {
        clearInterval(timer);
      } else if (this.apiPort) {
        if (!addr) {
          addr = { port: this.apiPort, host: '127.0.0.1' };
          this.logger.info(
            `Waiting for tcp port ${addr.host}:${addr.port} to accept connections...`
          );
        }

        if (client) {
          client.destroy();
        }
        client = new net.Socket();
        client.connect(addr, () => {
          this.logger.info(`... port is ready.`);
          clearInterval(timer);
          ready();
        });
        client.on('error', (err) => {
          this.logger.debug(`waitForApi: not ready yet: ${err}`);
        });
      }
    }, 250);
  }

  /**
   * Stops the wallet and node backends.
   *
   * Attempts to cleanly shut down the processes. However, if they
   * have not exited before the timeout, they will be killed. The
   * default timeout is [[defaultTimeoutSeconds]].
   *
   * @param timeoutSeconds - how long to wait before killing the processes.
   * @return a [[Promise]] that is fulfilled at the timeout, or before.
   *
   * @event exit - `walletBackend.events` will emit this when the
   *   wallet and node have both exited.
   */
  stop(
    timeoutSeconds = defaultTimeoutSeconds
  ): Promise<{ wallet: ServiceExitStatus; node: ServiceExitStatus }> {
    this.logger.debug(`Launcher.stop: stopping wallet and node`);
    return Promise.all([
      this.walletService.stop(timeoutSeconds),
      this.nodeService.stop(timeoutSeconds),
    ]).then(([wallet, node]) => {
      const status = { wallet, node };
      this.logger.debug(`Launcher.stop: both services are stopped.`, status);
      if (!this.exited) {
        this.walletBackend.events.exit(status);
        this.exited = true;
      }
      this.cleanupSignalHandlers();
      return status;
    });
  }

  /**
   * Stop services when this process gets killed.
   */
  private installSignalHandlers(): void {
    const signals: NodeJS.Signals[] = [
      'SIGINT',
      'SIGTERM',
      'SIGHUP',
      'SIGBREAK',
    ];
    const handler = (signal: NodeJS.Signals): void => {
      this.logger.info(`Received ${signal} - stopping services...`);
      this.walletService.stop(0).catch(passthroughErrorLogger);
      this.nodeService.stop(0).catch(passthroughErrorLogger);
    };
    signals.forEach((signal) => process.on(signal, handler));
    this.cleanupSignalHandlers = (): void => {
      signals.forEach((signal) => process.off(signal as any, handler));
      this.cleanupSignalHandlers = noop;
    };
  }

  private static makeServiceCommands(
    config: LaunchConfig,
    logger: Logger
  ): { wallet: Promise<WalletStartService>; node: Promise<StartService> } {
    logger.info(
      `Creating state directory ${config.stateDir} (if it doesn't already exist)`
    );
    const node = mkdirp(config.stateDir).then(() =>
      Launcher.nodeExe(config.stateDir, config)
    );
    const wallet = node.then(() => Launcher.walletExe(config.stateDir, config));
    return { wallet, node };
  }

  private static async walletExe(
    baseDir: DirPath,
    config: LaunchConfig
  ): Promise<WalletStartService> {
    return cardanoWalletStartService(baseDir, config);
  }

  private static nodeExe(
    baseDir: DirPath,
    config: LaunchConfig
  ): Promise<StartService> {
    switch (config.nodeConfig.kind) {
      case 'shelley':
        return startCardanoNode(baseDir, config.nodeConfig, config.networkName);
    }
  }
}

/**
 * Represents the API service of `cardano-wallet`.
 */
export class WalletBackend {
  api: Api;

  constructor(api: Api) {
    this.api = api;
    this.events = new WalletBackendEvents();
  }

  /**
   * @deprecated
   * @return HTTP connection parameters for the `cardano-wallet` API server.
   */
  getApi(): Api {
    return this.api;
  }

  /**
   * An [`EventEmitter`](https://nodejs.org/api/events.html#class-eventemitter)
   * that can be used to register handlers when
   * the process changes status.
   *
   * ### Example
   * ```typescript
   * launcher.walletBackend.events.on('ready', (api: Api) => { ... });
   * launcher.walletBackend.events.on('exit', (status: ExitStatus) => { ... });
   * ```
   */
  events: WalletBackendEvents;
}

/**
 * The type of events for [[WalletBackend]].
 */
export class WalletBackendEvents extends EventEmitter<{
  ready: (api: Api) => void;
  exit: (status: ExitStatus) => void;
}> {
  /**  @ignore */
  constructor() {
    super();
  }

  /**
   * Event emitted when the API server is ready to accept requests.
   *
   * @param api the wallet API info.
   * @event
   */
  ready(api: Api): void {
    this.emit('ready', api);
  }

  /**
   * Event emitted when both the wallet and node have both exited.
   *
   * @param stats the combined exit status.
   * @event
   */
  exit(status: ExitStatus): void {
    this.emit('exit', status);
  }
}
