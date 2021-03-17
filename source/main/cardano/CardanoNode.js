// @flow
import Store from 'electron-store';
import { spawn, exec } from 'child_process';
import type { ChildProcess } from 'child_process';
import type { WriteStream } from 'fs';
import type { Launcher } from 'cardano-launcher';
import { get, toInteger } from 'lodash';
import moment from 'moment';
import rfs from 'rotating-file-stream';
import { environment } from '../environment';
import {
  deriveProcessNames,
  deriveStorageKeys,
  promisedCondition,
} from './utils';
import { getProcess } from '../utils/processes';
import { safeExitWithCode } from '../utils/safeExitWithCode';
import type {
  CardanoNodeImplementations,
  CardanoNodeState,
  CardanoStatus,
  FaultInjection,
  FaultInjectionIpcRequest,
  FaultInjectionIpcResponse,
  TlsConfig,
} from '../../common/types/cardano-node.types';
import {
  CardanoNodeStates,
  CardanoNodeImplementationOptions,
} from '../../common/types/cardano-node.types';
import { CardanoWalletLauncher } from './CardanoWalletLauncher';
import { CardanoSelfnodeLauncher } from './CardanoSelfnodeLauncher';
import { launcherConfig } from '../config';
import type { NodeConfig } from '../config';
import type { Logger } from '../../common/types/logging.types';

/* eslint-disable consistent-return */

type Actions = {
  spawn: typeof spawn,
  exec: typeof exec,
  readFileSync: (path: string) => Buffer,
  createWriteStream: (path: string, options?: Object) => WriteStream,
  broadcastTlsConfig: (config: ?TlsConfig) => void,
  broadcastStateChange: (state: CardanoNodeState) => void,
};

type StateTransitions = {
  onStarting: () => void,
  onRunning: () => void,
  onStopping: () => void,
  onStopped: () => void,
  onUpdating: () => void,
  onUpdated: () => void,
  onCrashed: (code: number, signal: string) => void,
  onError: (code: number, signal: string) => void,
  onUnrecoverable: () => void,
};

type CardanoNodeIpcMessage = {
  Started?: Array<any>,
  ReplyPort?: number,
  FInjects?: FaultInjectionIpcResponse,
};

export type CardanoNodeConfig = {
  stateDir: string, // Path to the state directory
  nodeImplementation: CardanoNodeImplementations,
  nodeConfig: NodeConfig,
  logFilePath: string, // Log file path for cardano-sl
  tlsPath: string, // Path to cardano-node TLS folder
  startupTimeout: number, // Milliseconds to wait for cardano-node to startup
  startupMaxRetries: number, // Maximum number of retries for re-starting then ode
  shutdownTimeout: number, // Milliseconds to wait for cardano-node to gracefully shutdown
  killTimeout: number, // Milliseconds to wait for cardano-node to be killed
  updateTimeout: number, // Milliseconds to wait for cardano-node to update itself
  cluster: string,
  block0Path: string,
  block0Hash: string,
  secretPath: string,
  configPath: string,
  syncTolerance: string,
  cliBin: string, // Path to cardano-cli executable
  isStaging: boolean,
  metadataUrl?: string,
};

const CARDANO_UPDATE_EXIT_CODE = 20;
// grab the current network on which Daedalus is running
const network = String(environment.network);
const platform = String(environment.platform);
const { nodeImplementation } = launcherConfig;
const { isSelfnode } = environment;
// derive storage keys based on current network
const { PREVIOUS_CARDANO_PID } = deriveStorageKeys(network);
// derive Cardano process name based on current platform and node implementation
const { CARDANO_PROCESS_NAME } = deriveProcessNames(
  platform,
  nodeImplementation,
  isSelfnode
);
// create store for persisting CardanoNode and Daedalus PID's in fs
const store = new Store();

export class CardanoNode {
  /**
   * The config used to spawn cardano-node
   * @private
   */
  _config: CardanoNodeConfig;

  /**
   * The managed cardano-node child process
   * @private
   */
  _node: ?Launcher;

  /**
   * The ipc channel used for broadcasting messages to the outside world
   * @private
   */
  _actions: Actions;

  /**
   * The ipc channel used for broadcasting messages to the outside world
   * @private
   */
  _transitionListeners: StateTransitions;

  /**
   * logger instance to print debug messages to
   * @private
   */
  _log: Logger;

  /**
   * Log file stream for cardano-node / Jormungandr
   * @private
   */
  _cardanoNodeLogFile: WriteStream;

  /**
   * Log file stream for cardano-wallet
   * @private
   */
  _cardanoWalletLogFile: WriteStream;

  /**
   * The TLS config that is generated by the cardano-node
   * on each startup and is broadcasted over ipc channel
   * @private
   */
  _tlsConfig: ?TlsConfig = null;

  /**
   * The current state of the node, used for making decisions
   * when events like process crashes happen.
   *
   * @type {CardanoNodeState}
   * @private
   */
  _state: CardanoNodeState = CardanoNodeStates.STOPPED;

  /**
   * The last saved status of cardano node, acting as a cache for the
   * frontend to enable faster page reloads.
   *
   * @type {CardanoStatus}
   * @private
   */
  _status: ?CardanoStatus = null;

  /**
   * Number of retries to startup the node (without ever reaching running state)
   */
  _startupTries: number = 0;

  /**
   * Flag which makes cardano node to exit Daedalus after stopping
   */
  _exitOnStop: boolean = false;

  /**
   * All faults that have been injected and confirmed by cardano-node.
   * These faults can be used during testing to trigger faulty behavior
   * that would not be testable with a correctly working node.
   *
   * @type {Array}
   * @private
   */
  _injectedFaults: Array<FaultInjection> = [];

  /**
   * Cardano Node config getter
   * @returns {CardanoNodeImplementations}
   */
  get config(): CardanoNodeConfig {
    return this._config;
  }

  /**
   * Getter which copies and returns the internal tls config.
   * @returns {TlsConfig}
   */
  get tlsConfig(): TlsConfig {
    return Object.assign({}, this._tlsConfig);
  }

  /**
   * Getter which returns the PID of the child process of cardano-node
   * @returns {TlsConfig} // I think this returns a number...
   */
  get pid(): ?number {
    return get(this, '_node.pid', null);
  }

  /**
   * Getter for the current internal state of the node.
   * @returns {CardanoNodeState}
   */
  get state(): CardanoNodeState {
    return this._state;
  }

  /**
   * Getter for the cached status of the node.
   * @returns {CardanoStatus}
   */
  get status(): ?CardanoStatus {
    return Object.assign({}, this._status, {
      cardanoNodePID: get(this, '_node.pid', 0),
      cardanoWalletPID: get(this, '_node.wpid', 0),
    });
  }

  /**
   * Getter for the number of tried (and failed) startups
   * @returns {number}
   */
  get startupTries(): number {
    return this._startupTries;
  }

  /**
   * Constructs and prepares the CardanoNode instance for life.
   * @param log
   * @param actions
   * @param transitions
   */
  constructor(log: Logger, actions: Actions, transitions: StateTransitions) {
    this._log = log;
    this._actions = actions;
    this._transitionListeners = transitions;
  }

  /**
   * Starts cardano-node as child process with given config and log file stream.
   * Waits up to `startupTimeout` for the process to connect.
   * Registers ipc listeners for any necessary process lifecycle events.
   * Asks the node to reply with the current port.
   * Transitions into STARTING state.
   *
   * @param config {CardanoNodeConfig}
   * @param isForced {boolean}
   * @returns {Promise<void>} resolves if the node could be started, rejects with error otherwise.
   */
  start = async (
    config: CardanoNodeConfig,
    isForced: boolean = false
  ): Promise<void> => {
    const { _log } = this;

    // Guards
    const nodeCanBeStarted = await this._canBeStarted();

    if (!nodeCanBeStarted) {
      _log.error('CardanoNode#start: Cannot be started', {
        startupTries: this._startupTries,
      });
      return Promise.reject(new Error('CardanoNode: Cannot be started'));
    }
    if (this._isUnrecoverable(config) && !isForced) {
      _log.error('CardanoNode#start: Too many startup retries', {
        startupTries: this._startupTries,
      });
      return Promise.reject(new Error('CardanoNode: Too many startup retries'));
    }

    // Setup
    const {
      // startupTimeout,
      nodeConfig,
      stateDir,
      cluster,
      tlsPath,
      block0Path,
      block0Hash,
      secretPath,
      configPath,
      syncTolerance,
      cliBin,
      isStaging,
      metadataUrl,
    } = config;

    this._config = config;

    this._startupTries++;
    this._changeToState(CardanoNodeStates.STARTING);
    _log.info(
      `CardanoNode#start: trying to start cardano-node for the ${this._startupTries} time`,
      { startupTries: this._startupTries }
    );

    return new Promise(async (resolve, reject) => {
      const nodeLogFile = rfs(
        (time) => {
          // The module works by writing to the one file name before it is rotated out.
          if (!time) return 'node.log';
          const timestamp = moment.utc().format('YYYYMMDDHHmmss');
          return `node.log-${timestamp}`;
        },
        {
          size: '5M',
          path: config.logFilePath,
          maxFiles: 4,
        }
      );
      this._cardanoNodeLogFile = nodeLogFile;

      const walletLogFile = rfs(
        (time) => {
          // The module works by writing to the one file name before it is rotated out.
          if (!time) return 'cardano-wallet.log';
          const timestamp = moment.utc().format('YYYYMMDDHHmmss');
          return `cardano-wallet.log-${timestamp}`;
        },
        {
          size: '5M',
          path: config.logFilePath,
          maxFiles: 4,
        }
      );
      this._cardanoWalletLogFile = walletLogFile;

      if (isSelfnode) {
        try {
          const { selfnodeBin } = launcherConfig;
          const { node, replyPort } = await CardanoSelfnodeLauncher({
            selfnodeBin,
            processName: CARDANO_PROCESS_NAME,
            onStop: this._ensureProcessIsNotRunning,
          });
          this._node = node;
          this._handleCardanoNodeMessage({ ReplyPort: replyPort });
          resolve();
        } catch (error) {
          _log.error(
            'CardanoNode#start: Unable to initialize cardano-launcher',
            { error }
          );
          const { code, signal } = error || {};
          this._handleCardanoNodeError(code, signal);
          reject(
            new Error(
              'CardanoNode#start: Unable to initialize cardano-launcher'
            )
          );
        }
      } else {
        try {
          const node = await CardanoWalletLauncher({
            nodeImplementation,
            nodeConfig,
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
            metadataUrl,
          });

          this._node = node;

          _log.info('Starting cardano-node now...');

          // await promisedCondition(() => node.connected, startupTimeout);

          node
            .start()
            .then((api) => {
              const processes: {
                wallet: ChildProcess,
                node: ChildProcess,
              } = {
                wallet: node.walletService.getProcess(),
                node: node.nodeService.getProcess(),
              };

              // Setup event handling
              node.walletBackend.events.on('exit', (exitStatus) => {
                _log.info('CardanoNode#exit', { exitStatus });
                const { code, signal } = exitStatus.wallet;
                this._handleCardanoNodeExit(code, signal);
              });

              node.pid = processes.node.pid;
              node.wpid = processes.wallet.pid;
              node.connected = true; // TODO: use processes.wallet.connected here
              _log.info(
                `CardanoNode#start: cardano-node child process spawned with PID ${processes.node.pid}`,
                { pid: processes.node.pid }
              );
              _log.info(
                `CardanoNode#start: cardano-wallet child process spawned with PID ${processes.wallet.pid}`,
                { pid: processes.wallet.pid }
              );
              this._handleCardanoNodeMessage({
                ReplyPort: api.requestParams.port,
              });
              resolve();
            })
            .catch((exitStatus) => {
              _log.error(
                'CardanoNode#start: Error while spawning cardano-node',
                {
                  exitStatus,
                }
              );
              const { code, signal } = exitStatus.wallet || {};
              this._handleCardanoNodeError(code, signal);
              reject(
                new Error(
                  'CardanoNode#start: Error while spawning cardano-node'
                )
              );
            });
        } catch (error) {
          _log.error(
            'CardanoNode#start: Unable to initialize cardano-launcher',
            {
              error,
            }
          );
          const { code, signal } = error || {};
          this._handleCardanoNodeError(code, signal);
          reject(
            new Error(
              'CardanoNode#start: Unable to initialize cardano-launcher'
            )
          );
        }
      }
    });
  };

  /**
   * Stops cardano-node, first by stopping and waiting up to `shutdownTimeout`
   * for the node to shutdown itself properly. If that doesn't work as expected the
   * node is killed.
   *
   * @returns {Promise<void>} resolves if the node could be stopped, rejects with error otherwise.
   */
  async stop(): Promise<void> {
    const { _node, _log, _config } = this;
    if (await this._isDead()) {
      _log.info('CardanoNode#stop: process is not running anymore');
      return Promise.resolve();
    }
    _log.info('CardanoNode#stop: stopping cardano-node process');
    try {
      this._changeToState(CardanoNodeStates.STOPPING);
      if (_node) await _node.stop(_config.shutdownTimeout / 1000);
      await this._waitForNodeProcessToExit(_config.shutdownTimeout);
      await this._storeProcessStates();
      this._reset();
      return Promise.resolve();
    } catch (error) {
      _log.error('CardanoNode#stop: cardano-node did not stop correctly', {
        error,
      });
      try {
        await this.kill();
      } catch (killError) {
        return Promise.reject(killError);
      }
    }
  }

  /**
   * Kills cardano-node and waitsup to `killTimeout` for the node to
   * report the exit message.
   *
   * @returns {Promise<void>} resolves if the node could be killed, rejects with error otherwise.
   */
  kill(): Promise<void> {
    const { _node, _log } = this;
    return new Promise(async (resolve, reject) => {
      if (await this._isDead()) {
        _log.info('CardanoNode#kill: process is already dead');
        return Promise.resolve();
      }
      try {
        _log.info('CardanoNode#kill: killing cardano-node process');
        if (_node) _node.kill();
        await this._waitForCardanoToExitOrKillIt();
        await this._storeProcessStates();
        this._changeToState(CardanoNodeStates.STOPPED);
        this._reset();
        resolve();
      } catch (_) {
        _log.info('CardanoNode#kill: could not kill cardano-node');
        await this._storeProcessStates();
        this._reset();
        reject(new Error('Could not kill cardano-node.'));
      }
    });
  }

  /**
   * Stops cardano-node if necessary and starts it again with current config.
   * Optionally the restart can be forced, so that the `maxRestartTries` is ignored.
   *
   * @param isForced {boolean}
   * @returns {Promise<void>} resolves if the node could be restarted, rejects with error otherwise.
   */
  async restart(isForced: boolean = false): Promise<void> {
    const { _log, _config } = this;
    try {
      // Stop cardano nicely if it is still awake
      if (this._isConnected()) {
        _log.info('CardanoNode#restart: stopping current node');
        await this.stop();
      }
      _log.info('CardanoNode#restart: restarting node with previous config', {
        isForced,
      });
      await this._waitForCardanoToExitOrKillIt();
      if (this._exitOnStop) {
        _log.info('Daedalus:safeExit: exiting Daedalus with code 0', {
          code: 0,
        });
        safeExitWithCode(0);
      } else {
        await this.start(_config, isForced);
      }
    } catch (error) {
      _log.error('CardanoNode#restart: Could not restart cardano-node', {
        error,
      });
      if (this._state !== CardanoNodeStates.UNRECOVERABLE) {
        this._changeToState(CardanoNodeStates.ERRORED);
      }
      return Promise.reject(error);
    }
  }

  /**
   * Uses the configured action to broadcast the tls config
   */
  broadcastTlsConfig() {
    this._actions.broadcastTlsConfig(this._tlsConfig);
  }

  /**
   * Changes the internal state to UPDATING.
   * Waits up to the configured `updateTimeout` for the UPDATED state.
   * Kills cardano-node if it didn't properly update.
   *
   * @returns {Promise<void>} resolves if the node updated, rejects with error otherwise.
   */
  async expectNodeUpdate(): Promise<void> {
    const { _log, _config } = this;
    this._changeToState(CardanoNodeStates.UPDATING);
    _log.info('CardanoNode: waiting for node to apply update');
    try {
      await promisedCondition(
        () => this._state === CardanoNodeStates.UPDATED,
        _config.updateTimeout
      );
      await this._waitForNodeProcessToExit(_config.updateTimeout);
    } catch (error) {
      _log.info('CardanoNode: did not apply update as expected, killing it...');
      return this.kill();
    }
  }

  /**
   * Sends an ipc message to cardano-node to inject a specific fault.
   * This is useful for testing certain error cases that cannot be tested
   * with a properly working cardano-node.
   *
   * Returns a promise that resolves as soon as cardano-node confirmed the injection.
   *
   * @param request
   * @returns {Promise<void>}
   */
  setFault = async (request: FaultInjectionIpcRequest) => {
    if (!this._node) return;
    const fault = request[0];
    const isEnabled = request[1];
    this._node.send({ SetFInject: request });
    try {
      return await promisedCondition(() => {
        const hasFault = this._injectedFaults.includes(fault);
        return isEnabled ? hasFault : !hasFault;
      });
    } catch (error) {
      return Promise.reject(
        new Error(`cardano-node did not inject the fault "${fault}" correctly.`)
      );
    }
  };

  saveStatus(status: ?CardanoStatus) {
    this._status = status;
  }

  /**
   * Signals the cardano-node to exit Daedalus on stop
   */
  exitOnStop = () => {
    this._exitOnStop = true;
  };

  // ================================= PRIVATE ===================================
  /**
   * Handles node ipc messages sent by the cardano-node process.
   * Updates the tls config where possible and broadcasts it to
   * the outside if it is complete. Transitions into RUNNING state
   * after it broadcasted the tls config (that's the difference between
   * STARTING and RUNNING).
   *
   * @param msg
   * @private
   */
  _handleCardanoNodeMessage = (msg: CardanoNodeIpcMessage) => {
    if (msg == null) return;
    this._log.info('CardanoNode: received message', { msg });
    if (msg.ReplyPort != null)
      this._handleCardanoReplyPortMessage(msg.ReplyPort);
    if (msg.FInjects != null)
      this._handleCardanoFaultInjectionResponse(msg.FInjects);
  };

  /**
   * Reads the tls certificates and uses them together with the given port
   * to set the tls config, which will be used for any http communication
   * with the node.
   *
   * Changes state to RUNNING.
   *
   * @param port
   * @private
   */
  _handleCardanoReplyPortMessage = (port: number) => {
    const { _actions } = this;
    const { tlsPath } = this._config;
    this._tlsConfig =
      nodeImplementation === CardanoNodeImplementationOptions.JORMUNGANDR ||
      environment.isSelfnode
        ? {
            ca: ('': any),
            key: ('': any),
            cert: ('': any),
            hostname: 'localhost',
            port,
          }
        : {
            ca: _actions.readFileSync(`${tlsPath}/client/ca.crt`),
            key: _actions.readFileSync(`${tlsPath}/client/client.key`),
            cert: _actions.readFileSync(`${tlsPath}/client/client.pem`),
            hostname: 'localhost',
            port,
          };

    if (this._state === CardanoNodeStates.STARTING) {
      this._changeToState(CardanoNodeStates.RUNNING);
      this.broadcastTlsConfig();
      // Reset the startup tries when we managed to get the node running
      this._startupTries = 0;
    }
  };

  /**
   * Updates the active, injected faults confirmed by cardano-node.
   * @param response
   * @private
   */
  _handleCardanoFaultInjectionResponse = (
    response: FaultInjectionIpcResponse
  ) => {
    this._log.info('CardanoNode: the following injected faults are active', {
      injectedFaults: response,
    });
    this._injectedFaults = response;
  };

  _handleCardanoNodeError = async (code: number, signal: string) => {
    const { _log, _config } = this;
    _log.error('CardanoNode: error', { code, signal });
    if (this._isUnrecoverable(_config)) {
      this._changeToState(CardanoNodeStates.UNRECOVERABLE);
    } else {
      this._changeToState(CardanoNodeStates.ERRORED);
      this._transitionListeners.onError(code, signal);
      await this.restart();
    }
  };

  _handleCardanoNodeExit = async (code: number, signal: string) => {
    const { _log, _config, _node } = this;
    _log.info('CardanoNode exited', { code, signal });
    // We don't know yet what happened but we can be sure cardano-node is exiting
    if (this._state === CardanoNodeStates.RUNNING) {
      this._changeToState(CardanoNodeStates.EXITING);
    }
    try {
      // Before proceeding with exit procedures, wait until the node is really dead.
      await this._waitForNodeProcessToExit(_config.shutdownTimeout);
    } catch (_) {
      _log.error(
        `CardanoNode: sent exit code ${code} but was still running after ${_config.shutdownTimeout}ms. Killing it now.`,
        { code, shutdownTimeout: _config.shutdownTimeout }
      );
      try {
        if (_node)
          await this._ensureProcessIsNotRunning(
            _node.pid,
            CARDANO_PROCESS_NAME
          );
      } catch (e) {
        _log.info('CardanoNode: did not exit correctly');
      }
    }
    _log.info('CardanoNode: process really exited', { code, signal });
    // Handle various exit scenarios
    if (this._state === CardanoNodeStates.STOPPING) {
      this._changeToState(CardanoNodeStates.STOPPED);
    } else if (
      this._state === CardanoNodeStates.UPDATING &&
      code === CARDANO_UPDATE_EXIT_CODE
    ) {
      this._changeToState(CardanoNodeStates.UPDATED);
    } else if (this._isUnrecoverable(_config)) {
      this._changeToState(CardanoNodeStates.UNRECOVERABLE);
    } else {
      this._changeToState(CardanoNodeStates.CRASHED, code, signal);
    }
    await this._storeProcessStates();
    this._reset();
  };

  _reset = () => {
    if (this._cardanoNodeLogFile) this._cardanoNodeLogFile.end();
    if (this._cardanoWalletLogFile) this._cardanoWalletLogFile.end();
    if (this._node) this._node = null;
    this._tlsConfig = null;
  };

  _changeToState(state: CardanoNodeState, ...args: Array<any>) {
    const { _log, _transitionListeners } = this;
    _log.info(`CardanoNode: transitions to <${state}>`, { state });
    this._state = state;
    this._actions.broadcastStateChange(state);
    switch (state) {
      case CardanoNodeStates.STARTING:
        return _transitionListeners.onStarting();
      case CardanoNodeStates.RUNNING:
        return _transitionListeners.onRunning();
      case CardanoNodeStates.STOPPING:
        return _transitionListeners.onStopping();
      case CardanoNodeStates.STOPPED:
        return _transitionListeners.onStopped();
      case CardanoNodeStates.UPDATING:
        return _transitionListeners.onUpdating();
      case CardanoNodeStates.UPDATED:
        return _transitionListeners.onUpdated();
      case CardanoNodeStates.CRASHED:
        return _transitionListeners.onCrashed(...args);
      case CardanoNodeStates.UNRECOVERABLE:
        return _transitionListeners.onUnrecoverable();
      default:
    }
  }

  /**
   * Checks if cardano-node child_process is connected and can be interacted with
   * @returns {boolean}
   */
  _isConnected = (): boolean => this._node != null && this._node.connected;

  /**
   * Checks if cardano-node child_process is not running anymore
   * @returns {boolean}
   */
  _isDead = async (): Promise<boolean> =>
    !this._isConnected() && this._isNodeProcessNotRunningAnymore();

  /**
   * Checks if current cardano-node child_process is "awake" (created, connected, stateful)
   * If node is already awake, returns false.
   * Kills process with PID that matches PID of the previously running
   * cardano-node child_process that didn't shut down properly
   * @returns {boolean}
   * @private
   */
  _canBeStarted = async (): Promise<boolean> => {
    if (this._isConnected()) {
      return false;
    }
    try {
      await this._ensurePreviousCardanoNodeIsNotRunning();
      return true;
    } catch (error) {
      return false;
    }
  };

  _ensureProcessIsNotRunning = async (pid: number, name: string) => {
    const { _log } = this;
    _log.info(
      `CardanoNode: checking if ${name} process (PID: ${pid}) is still running`,
      { name, pid }
    );
    if (await this._isProcessRunning(pid, name)) {
      _log.info(`CardanoNode: killing ${name} process (PID: ${pid})`, {
        name,
        pid,
      });
      try {
        await this._killProcessWithName(pid, name);
        return Promise.resolve();
      } catch (error) {
        _log.error(
          `CardanoNode: could not kill ${name} process (PID: ${pid})`,
          { name, pid, error }
        );
        return Promise.reject();
      }
    }
    this._log.info(`CardanoNode: no ${name} process (PID: ${pid}) is running`, {
      name,
      pid,
    });
  };

  _ensureCurrentCardanoNodeIsNotRunning = async (): Promise<void> => {
    const { _log, _node } = this;
    _log.info(
      'CardanoNode: checking if current cardano-node process is still running'
    );
    if (_node == null) {
      return Promise.resolve();
    }
    return this._ensureProcessIsNotRunning(_node.pid, CARDANO_PROCESS_NAME);
  };

  _ensurePreviousCardanoNodeIsNotRunning = async (): Promise<void> => {
    const { _log } = this;
    const previousPID: ?number = await this._retrieveData(PREVIOUS_CARDANO_PID);
    _log.info(
      'CardanoNode: checking if previous cardano-node process is still running',
      { previousPID }
    );
    if (previousPID == null) {
      return Promise.resolve();
    }
    return this._ensureProcessIsNotRunning(previousPID, CARDANO_PROCESS_NAME);
  };

  _isProcessRunning = async (
    previousPID: number,
    processName: string
  ): Promise<boolean> => {
    const { _log } = this;
    try {
      const previousProcess = await getProcess(previousPID, processName);
      if (!previousProcess) {
        _log.info(
          `CardanoNode: No previous ${processName} process is running anymore`,
          { processName }
        );
        return false;
      }
      _log.info(`CardanoNode: previous ${processName} process found`, {
        processName,
        previousProcess,
      });
      return true;
    } catch (error) {
      _log.error('CardanoNode: _isProcessRunning error', { error });
      return false;
    }
  };

  // kills running process which did not shut down properly between sessions
  _killProcessWithName = async (pid: number, name: string): Promise<void> => {
    const { _config } = this;
    try {
      if (!environment.isWindows) {
        this._log.info(`CardanoNode: using "process.kill(${pid})" to kill it`, {
          pid,
        });
        process.kill(pid);
      } else {
        // https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/taskkill
        const windowsKillCmd = `taskkill /pid ${pid} /t /f`;
        this._log.info('CardanoNode (Windows): using kill command to kill it', {
          windowsKillCmd,
        });
        this._actions.exec(windowsKillCmd);
      }
      await promisedCondition(
        async () => (await this._isProcessRunning(pid, name)) === false,
        _config.killTimeout
      );

      this._log.info(
        `CardanoNode: successfully killed ${name} process (PID: ${pid})`,
        { name, pid }
      );
      return Promise.resolve();
    } catch (error) {
      this._log.error(
        `CardanoNode: _killProcessWithName returned an error attempting to kill ${name} process (PID: ${pid})`,
        { processName: name, pid, error }
      );
      return Promise.reject(error);
    }
  };

  async _storeProcessStates() {
    const { _log } = this;
    if (this._node != null) {
      const { pid } = this._node;
      _log.info('CardanoNode: storing last cardano-node PID', { pid });
      await this._storeData(PREVIOUS_CARDANO_PID, pid);
    }
  }

  // stores the current port/pid on which cardano-node or Daedalus is running
  _storeData = (identifier: string, data: number): Promise<void> =>
    new Promise((resolve, reject) => {
      try {
        // saves current port/pid in file system
        store.set(identifier, data);
        this._log.info(`CardanoNode: ${identifier} stored successfully`);
        resolve();
      } catch (error) {
        this._log.error(`CardanoNode: failed to store ${identifier}`, {
          error,
        });
        reject(error);
      }
    });

  // retrieves the last known port/pid on which cardano-node or Daedalus was running
  _retrieveData = (identifier: string): Promise<?number> =>
    new Promise((resolve, reject) => {
      try {
        // retrieves previous port/pid from file system
        const data: ?number = store.get(identifier);

        if (!data) {
          this._log.info(`CardanoNode: get ${identifier} returned null`);
          resolve(null);
        }

        this._log.info(`CardanoNode: get ${identifier} success`, {
          [`${identifier}`]: data,
        });
        resolve(toInteger(data));
      } catch (error) {
        this._log.error(`CardanoNode: get ${identifier} failed`, { error });
        reject(error);
      }
    });

  _isNodeProcessStillRunning = async (): Promise<boolean> =>
    this._node != null &&
    this._isProcessRunning(this._node.pid, CARDANO_PROCESS_NAME);

  _isNodeProcessNotRunningAnymore = async () =>
    (await this._isNodeProcessStillRunning()) === false;

  _waitForNodeProcessToExit = async (timeout: number) =>
    promisedCondition(this._isNodeProcessNotRunningAnymore, timeout);

  _waitForCardanoToExitOrKillIt = async () => {
    const { _config } = this;
    if (this._isNodeProcessNotRunningAnymore()) return Promise.resolve();
    try {
      await this._waitForNodeProcessToExit(_config.shutdownTimeout);
    } catch (_) {
      await this._ensureCurrentCardanoNodeIsNotRunning();
    }
  };

  _isUnrecoverable = (config: CardanoNodeConfig) =>
    this._startupTries >= config.startupMaxRetries;
}
