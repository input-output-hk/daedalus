import { readFileSync } from 'fs';
import { BrowserWindow } from 'electron';
import { logger } from './utils/logging';
import WatchdogManager from './WatchdogManager';
import type {
  InstallerResult,
  WatchdogState as InternalWatchdogState,
} from './WatchdogManager';
import type {
  MithrilProgress,
  WatchdogState,
} from '../common/types/watchdog.types';
import {
  mithrilProgressChannel,
  mithrilStatusChannel,
  walletPortChannel,
} from './ipc/mithrilPushChannel';
import {
  nodeStartupStatusChannel,
  nodeBlockSyncProgressChannel,
  watchdogStoppedChannel,
  backendStopStatusChannel,
} from './ipc/nodePushChannel';
import {
  buildMigrateStateCommand,
  forgetMigratedSettings,
} from './utils/watchdogStateMigration';
import { environment } from './environment';

type EventHandler = (event: Record<string, unknown>) => void;

// Watchdog events that can change the stop status pushed to the renderer.
const STOP_STATUS_EVENTS = new Set([
  'backend_stop_progress',
  'node_shutdown_ms',
  'node_started',
  'wallet_started',
  'wallet_ready',
  'wallet_unrecoverable',
  'node_unrecoverable',
  'mithril_status',
]);

class BackendLifecycle {
  private manager: WatchdogManager | null = null;
  private getWindow: () => BrowserWindow | null = () => null;
  private eventHandlers: EventHandler[] = [];
  private _defaultChainPath: string | null = null;
  private _customChainPath: string | null = null;
  private _stopPromise: Promise<void> | null = null;
  private _installerPath: string | null = null;

  // ---------------------------------------------------------------------------
  // Setup
  // ---------------------------------------------------------------------------

  setWindowProvider(getWindow: () => BrowserWindow | null): void {
    this.getWindow = getWindow;
  }

  setChainPaths(
    defaultChainPath: string | null,
    customChainPath: string | null
  ): void {
    this._defaultChainPath = defaultChainPath;
    this._customChainPath = customChainPath;
  }

  // ---------------------------------------------------------------------------
  // Start
  // ---------------------------------------------------------------------------

  // start() wires up process.stdin/stdout IPC with the watchdog parent process.
  // wallet-ready and error handling happen internally without blocking the caller.
  start(): void {
    const manager = new WatchdogManager();
    this.manager = manager;

    // Re-register any handlers that were added before this start call
    for (const handler of this.eventHandlers) {
      manager.onEvent(handler);
    }

    const sendWalletPort = (port: number) => {
      const win = this.getWindow();
      if (!win) return;
      let ca: number[] = [];
      let cert: number[] = [];
      let key: number[] = [];
      try {
        const caPath = process.env.TLS_CA_CERT;
        const certPath = process.env.TLS_CLIENT_CERT;
        const keyPath = process.env.TLS_CLIENT_KEY;
        if (caPath && certPath && keyPath) {
          ca = Array.from(readFileSync(caPath));
          cert = Array.from(readFileSync(certPath));
          key = Array.from(readFileSync(keyPath));
        }
      } catch (e) {
        logger.error('BackendLifecycle: failed to read TLS certs', {
          error: e,
        });
      }
      walletPortChannel.send({ port, ca, cert, key }, win.webContents);
    };

    // Push events to the renderer window
    manager.onEvent((event) => {
      const win = this.getWindow();
      if (!win) return;
      const eventType = event.event as string | undefined;
      if (eventType && STOP_STATUS_EVENTS.has(eventType)) {
        this._sendStopStatus();
      }
      if (eventType === 'wallet_ready') {
        sendWalletPort(event.port as number);
      } else if (eventType === 'mithril_progress') {
        const progress: MithrilProgress = {
          filesDownloaded: event.files_downloaded as number,
          filesTotal: event.files_total as number,
          bytesDownloaded: event.bytes_downloaded as number,
          bytesTotal: event.bytes_total as number,
          secondsElapsed: event.seconds_elapsed as number,
          stepNum: event.step_num as number,
          totalSteps: event.total_steps as number,
          phase: event.phase as MithrilProgress['phase'],
        };
        mithrilProgressChannel.send(progress, win.webContents);
      } else if (eventType === 'mithril_status') {
        mithrilStatusChannel.send(event as any, win.webContents);
      } else if (eventType === 'node_startup_status') {
        nodeStartupStatusChannel.send(
          { phase: event.phase as string },
          win.webContents
        );
      } else if (eventType === 'node_block_sync_progress') {
        nodeBlockSyncProgressChannel.send(
          {
            kind: event.kind as string,
            progress: event.progress as number,
          },
          win.webContents
        );
      } else if (eventType === 'stopped') {
        watchdogStoppedChannel.send(undefined, win.webContents);
      } else if (eventType === 'migrate_state_request') {
        this._handleMigrateStateRequest(manager);
      } else if (eventType === 'migrate_state_saved') {
        // watchdog-state.json now holds the migrated settings.
        forgetMigratedSettings(environment.network);
      }
    });

    manager.start();

    // walletReadyPromise resolves once (first wallet ready); the onEvent handler
    // above covers subsequent wallet restarts. Keep the promise for callers
    // (e.g. BackendLifecycle.getWalletPort()) but the port push is now event-driven.
    manager.walletReadyPromise.catch((reason) => {
      // When watchdog exits it also kills Electron (via tether_to_watchdog),
      // so there is nothing useful to do here except log the reason.
      logger.error('BackendLifecycle: watchdog stopped', { reason });
    });
  }

  // ---------------------------------------------------------------------------
  // Chain path update
  // ---------------------------------------------------------------------------

  async setCustomChainPath(customPath: string | null): Promise<void> {
    this._customChainPath = customPath;
    if (this.manager) {
      this.manager.sendCommand({ cmd: 'set_chain_path', path: customPath });
      this._sendStopStatus();
      logger.info('BackendLifecycle: setCustomChainPath — sent to watchdog', {
        customPath,
      });
    } else {
      logger.warn(
        'BackendLifecycle: setCustomChainPath called before manager started',
        { customPath }
      );
    }
  }

  // ---------------------------------------------------------------------------
  // Migration
  // ---------------------------------------------------------------------------

  private _handleMigrateStateRequest(manager: WatchdogManager): void {
    const command = buildMigrateStateCommand(environment.network);

    logger.info('BackendLifecycle: responding to migrate_state_request', {
      chainPath: command.chain_path,
      nodeExtraArgs: command.node_extra_args,
    });

    // The migrated keys stay in electron-store until the watchdog reports
    // migrate_state_saved, so a reply it never applied is sent again on the
    // next launch.
    manager.sendCommand(command);
  }

  // ---------------------------------------------------------------------------
  // Stop
  // ---------------------------------------------------------------------------

  // Stops the backend for quit. The manager stays in place until the process
  // exits, so the renderer keeps receiving state and progress while the window
  // shows the shutdown status. Repeated calls share one stop.
  stop(): Promise<void> {
    const { manager } = this;
    if (!manager) return Promise.resolve();
    if (this._stopPromise === null) {
      this._stopPromise = manager.stop();
      this._sendStopStatus();
    }
    return this._stopPromise;
  }

  isStopping(): boolean {
    return this._stopPromise !== null;
  }

  // Has the watchdog start the update installer at `path` once it has stopped
  // cardano-wallet and cardano-node, as Daedalus quits. Returns false when no
  // watchdog connection exists to ask.
  installUpdate(path: string): boolean {
    if (!this.manager) return false;
    this._installerPath = path;
    this.manager.requestInstall(path);
    return true;
  }

  // What came of installUpdate(), once stop() has resolved; null if it was
  // not called.
  getInstallOutcome(): { path: string; result: InstallerResult } | null {
    const path = this._installerPath;
    const result = this.manager?.getInstallResult() ?? null;
    if (path === null || result === null) return null;
    return { path, result };
  }

  // Tells the renderer at once that the backend is stopping, for quit or for a
  // requested restart, so it never reads the wallet going away as a lost
  // connection.
  private _sendStopStatus(): void {
    const win = this.getWindow();
    if (!win || win.isDestroyed()) return;
    const state = this.manager?.getState();
    backendStopStatusChannel.send(
      {
        quitting: this._stopPromise !== null,
        restart: state?.requestedRestart ?? null,
        progress: state?.backendStopProgress ?? null,
      },
      win.webContents
    );
  }

  // ---------------------------------------------------------------------------
  // State / commands
  // ---------------------------------------------------------------------------

  getState(): WatchdogState | null {
    const s: InternalWatchdogState | null = this.manager?.getState() ?? null;
    if (!s) return null;
    return {
      ...s,
      defaultChainPath: this._defaultChainPath,
      customChainPath: this._customChainPath,
    };
  }

  sendMithrilCommand(cmd: object): void {
    if (!this.manager) {
      logger.warn('BackendLifecycle: sendMithrilCommand called but no manager');
      return;
    }
    this.manager.sendCommand(cmd);
    this._sendStopStatus();
  }

  onEvent(handler: EventHandler): void {
    this.eventHandlers.push(handler);
    // If a manager is already running, register on it immediately
    this.manager?.onEvent(handler);
  }
}

export const backendLifecycle = new BackendLifecycle();
export default BackendLifecycle;
