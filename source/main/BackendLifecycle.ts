import { readFileSync } from 'fs';
import { BrowserWindow } from 'electron';
import { logger } from './utils/logging';
import WatchdogManager from './WatchdogManager';
import type { WatchdogState as InternalWatchdogState } from './WatchdogManager';
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
} from './ipc/nodePushChannel';
import { requestElectronStore } from './ipc/electronStoreConversation';
import {
  STORAGE_KEYS as keys,
  STORAGE_TYPES as types,
} from '../common/config/electron-store.config';
import {
  getRtsFlagsSettings,
  deleteRtsFlagsSettings,
} from './utils/rtsFlagsSettings';
import { environment } from './environment';

type EventHandler = (event: Record<string, unknown>) => void;

class BackendLifecycle {
  private manager: WatchdogManager | null = null;
  private getWindow: () => BrowserWindow | null = () => null;
  private eventHandlers: EventHandler[] = [];
  private _defaultChainPath: string | null = null;
  private _customChainPath: string | null = null;

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
    const { network } = environment;

    const chainPath =
      (requestElectronStore({
        type: types.GET,
        key: keys.CUSTOM_CHAIN_PATH,
      }) as string | undefined) ?? null;

    // Raw RTS flags stored as e.g. ['-c']; wrap in +RTS/-RTS delimiters for cardano-node.
    const rawRtsFlags = getRtsFlagsSettings(network) ?? [];
    const nodeExtraArgs =
      rawRtsFlags.length > 0 ? ['+RTS', ...rawRtsFlags, '-RTS'] : [];

    logger.info('BackendLifecycle: responding to migrate_state_request', {
      chainPath,
      nodeExtraArgs,
    });

    manager.sendCommand({
      cmd: 'migrate_state',
      chain_path: chainPath,
      electron_flags: [],
      node_extra_args: nodeExtraArgs,
    });

    // Remove migrated keys so watchdog-state.json is the single source of truth.
    requestElectronStore({ type: types.DELETE, key: keys.CUSTOM_CHAIN_PATH });
    deleteRtsFlagsSettings(network);
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
