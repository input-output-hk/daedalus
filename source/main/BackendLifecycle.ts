import { readFileSync } from 'fs';
import { BrowserWindow } from 'electron';
import { logger } from './utils/logging';
import WatchdogManager from './WatchdogManager';
import type { WatchdogConfig, WatchdogState as InternalWatchdogState } from './WatchdogManager';
import type { WatchdogState } from '../common/types/watchdog.types';
import {
  mithrilProgressChannel,
  mithrilStatusChannel,
  walletPortChannel,
} from './ipc/mithrilPushChannel';
import {
  nodeStartupStatusChannel,
  nodeBlockSyncProgressChannel,
} from './ipc/nodePushChannel';
import type { MithrilProgress } from '../common/types/watchdog.types';

export type { WatchdogConfig };

const RESTART_DELAY_MS = 3_000;

type EventHandler = (event: Record<string, unknown>) => void;

class BackendLifecycle {
  private manager: WatchdogManager | null = null;
  private getWindow: () => BrowserWindow | null = () => null;
  private eventHandlers: EventHandler[] = [];
  private _exePath: string = '';
  private _config: WatchdogConfig | null = null;
  private _defaultChainPath: string | null = null;
  private _customChainPath: string | null = null;
  private _tlsPath: string | null = null;

  // ---------------------------------------------------------------------------
  // Setup
  // ---------------------------------------------------------------------------

  setWindowProvider(getWindow: () => BrowserWindow | null): void {
    this.getWindow = getWindow;
  }

  setChainPaths(defaultChainPath: string | null, customChainPath: string | null): void {
    this._defaultChainPath = defaultChainPath;
    this._customChainPath = customChainPath;
  }

  setTlsPath(tlsPath: string): void {
    this._tlsPath = tlsPath;
  }

  // ---------------------------------------------------------------------------
  // Start
  // ---------------------------------------------------------------------------

  // start() returns as soon as the watchdog process is spawned; wallet-ready
  // and error handling happen internally so callers (including setCustomChainPath)
  // don't block waiting for the full startup sequence.
  start(exePath: string, config: WatchdogConfig): void {
    this._exePath = exePath;
    this._config = config;

    const manager = new WatchdogManager();
    this.manager = manager;

    // Re-register any handlers that were added before this start call
    for (const handler of this.eventHandlers) {
      manager.onEvent(handler);
    }

    // Push mithril events to the renderer window
    manager.onEvent((event) => {
      const win = this.getWindow();
      if (!win) return;
      const eventType = event.event as string | undefined;
      if (eventType === 'mithril_progress') {
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
        nodeStartupStatusChannel.send({ phase: event.phase as string }, win.webContents);
      } else if (eventType === 'node_block_sync_progress') {
        nodeBlockSyncProgressChannel.send({
          kind: event.kind as string,
          progress: event.progress as number,
        }, win.webContents);
      }
    });

    manager.start(exePath, config);

    // Handle wallet-ready promise internally without blocking the caller
    manager.walletReadyPromise.then((port) => {
      logger.info('BackendLifecycle: wallet ready', { port });
      const win = this.getWindow();
      if (!win) return;
      let ca: number[] = [];
      let cert: number[] = [];
      let key: number[] = [];
      if (this._tlsPath) {
        try {
          const path = require('path') as typeof import('path');
          ca = Array.from(readFileSync(path.join(this._tlsPath, 'client/ca.crt')));
          cert = Array.from(readFileSync(path.join(this._tlsPath, 'client/client.pem')));
          key = Array.from(readFileSync(path.join(this._tlsPath, 'client/client.key')));
        } catch (e) {
          logger.error('BackendLifecycle: failed to read TLS certs', { error: e });
        }
      }
      walletPortChannel.send({ port, ca, cert, key }, win.webContents);
    }).catch((reason) => {
      logger.error('BackendLifecycle: startup failed, scheduling restart', { reason });
      setTimeout(() => {
        this.start(exePath, config);
      }, RESTART_DELAY_MS);
    });
  }

  // ---------------------------------------------------------------------------
  // Chain path update
  // ---------------------------------------------------------------------------

  async setCustomChainPath(customPath: string | null): Promise<void> {
    this._customChainPath = customPath;

    if (!this._config || !this._exePath) {
      logger.warn('BackendLifecycle: setCustomChainPath called before start');
      return;
    }

    // Build updated config with new chain_path
    const effectivePath = customPath
      ? require('path').join(customPath, 'chain')
      : this._defaultChainPath;

    const newConfig: WatchdogConfig = {
      ...this._config,
      mithril: this._config.mithril
        ? { ...this._config.mithril, chain_path: effectivePath ?? this._config.mithril.chain_path }
        : undefined,
    };

    logger.info('BackendLifecycle: restarting watchdog with new chain path', {
      customPath,
      effectivePath,
    });

    await this.stop();
    this.start(this._exePath, newConfig);
  }

  // ---------------------------------------------------------------------------
  // Stop
  // ---------------------------------------------------------------------------

  async stop(): Promise<void> {
    if (!this.manager) return;
    const manager = this.manager;
    this.manager = null;
    await manager.stop();
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
  }

  onEvent(handler: EventHandler): void {
    this.eventHandlers.push(handler);
    // If a manager is already running, register on it immediately
    this.manager?.onEvent(handler);
  }
}

export const backendLifecycle = new BackendLifecycle();
export default BackendLifecycle;
