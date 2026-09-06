import { readFileSync } from 'fs';
import { logger } from './utils/logging';
import WatchdogManager from './WatchdogManager';
import type { WatchdogState as InternalWatchdogState } from './WatchdogManager';
import type {
  MithrilProgress,
  WatchdogState,
} from '../common/types/watchdog.types';
import type { MithrilStatusMainRequest } from '../common/ipc/api';
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
import {
  consumeIpcResponse,
  currentWindowSender,
} from './ipc/lib/currentWindowSender';
import { revokeCip30Sessions } from './cip30/runtime';

type EventHandler = (event: Record<string, unknown>) => void;

class BackendLifecycle {
  private manager: WatchdogManager | null = null;
  private eventHandlers: EventHandler[] = [];
  private _defaultChainPath: string | null = null;
  private _customChainPath: string | null = null;

  // ---------------------------------------------------------------------------
  // Setup
  // ---------------------------------------------------------------------------

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

    // Push backend events to the trusted renderer window.
    manager.onEvent((event) => {
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
        consumeIpcResponse(
          mithrilProgressChannel.send(progress, currentWindowSender.sender),
          'MITHRIL_PROGRESS_CHANNEL'
        );
      } else if (eventType === 'mithril_status') {
        consumeIpcResponse(
          mithrilStatusChannel.send(
            (event as unknown) as MithrilStatusMainRequest,
            currentWindowSender.sender
          ),
          'MITHRIL_STATUS_CHANNEL'
        );
      } else if (eventType === 'node_startup_status') {
        consumeIpcResponse(
          nodeStartupStatusChannel.send(
            { phase: event.phase as string },
            currentWindowSender.sender
          ),
          'NODE_STARTUP_STATUS_CHANNEL'
        );
      } else if (eventType === 'node_block_sync_progress') {
        consumeIpcResponse(
          nodeBlockSyncProgressChannel.send(
            {
              kind: event.kind as string,
              progress: event.progress as number,
            },
            currentWindowSender.sender
          ),
          'NODE_BLOCK_SYNC_PROGRESS_CHANNEL'
        );
      } else if (eventType === 'stopped') {
        revokeCip30Sessions();
        consumeIpcResponse(
          watchdogStoppedChannel.send(undefined, currentWindowSender.sender),
          'WATCHDOG_STOPPED_CHANNEL'
        );
      }
    });

    manager.start();

    // Handle wallet-ready promise internally without blocking the caller
    manager.walletReadyPromise
      .then((port) => {
        logger.info('BackendLifecycle: wallet ready', { port });

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
        consumeIpcResponse(
          walletPortChannel.send(
            { port, ca, cert, key },
            currentWindowSender.sender
          ),
          'WALLET_PORT_CHANNEL'
        );
      })
      .catch((reason) => {
        revokeCip30Sessions();
        logger.error('BackendLifecycle: watchdog stopped', { reason });
      });
  }

  // ---------------------------------------------------------------------------
  // Chain path update
  // ---------------------------------------------------------------------------

  async setCustomChainPath(customPath: string | null): Promise<void> {
    this._customChainPath = customPath;
    // In the inverted architecture watchdog is the parent process and cannot be
    // restarted by Electron. Chain-path changes take effect on the next launch.
    logger.warn(
      'BackendLifecycle: setCustomChainPath — change will apply on next launch',
      { customPath }
    );
  }

  // ---------------------------------------------------------------------------
  // Stop
  // ---------------------------------------------------------------------------

  async stop(): Promise<void> {
    revokeCip30Sessions();
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
