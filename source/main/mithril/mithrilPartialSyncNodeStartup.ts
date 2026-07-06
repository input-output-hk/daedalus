import fs from 'fs-extra';
import { dialog } from 'electron';
import type { BrowserWindow } from 'electron';

import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import type { CardanoNodeState } from '../../common/types/cardano-node.types';
import type { MithrilPartialSyncStatusSnapshot } from '../../common/types/mithril-partial-sync.types';
import type { CardanoNode } from '../cardano/CardanoNode';
import {
  clearMithrilPartialSyncMarker,
  readMithrilPartialSyncMarker,
  writeMithrilPartialSyncMarker,
} from './mithrilPartialSyncMarker';
import { logger } from '../utils/logging';
import { safeExitWithCode } from '../utils/safeExitWithCode';

const MITHRIL_COMPLETION_DELAY_MS = 6000;

type WipeChainAndSnapshots = (
  reason: string,
  nodeState: CardanoNodeState
) => Promise<void>;

type NodeStartupDependencies = {
  mainWindow: BrowserWindow;
  cardanoNode: CardanoNode;
  wipeChainAndSnapshots: WipeChainAndSnapshots;
  getGeneration: () => number;
  emitPartialSyncStatus: (
    status: MithrilPartialSyncStatusSnapshot
  ) => Promise<void>;
  getPartialSyncStatus: () => MithrilPartialSyncStatusSnapshot;
};

const waitForNodeStartupProof = () =>
  new Promise<void>((resolve) => {
    setTimeout(resolve, MITHRIL_COMPLETION_DELAY_MS);
  });

export class MithrilPartialSyncNodeStartup {
  _mainWindow: BrowserWindow;
  _cardanoNode: CardanoNode;
  _wipeChainAndSnapshots: WipeChainAndSnapshots;
  _getGeneration: () => number;
  _emitPartialSyncStatus: (
    status: MithrilPartialSyncStatusSnapshot
  ) => Promise<void>;
  _getPartialSyncStatus: () => MithrilPartialSyncStatusSnapshot;

  constructor({
    mainWindow,
    cardanoNode,
    wipeChainAndSnapshots,
    getGeneration,
    emitPartialSyncStatus,
    getPartialSyncStatus,
  }: NodeStartupDependencies) {
    this._mainWindow = mainWindow;
    this._cardanoNode = cardanoNode;
    this._wipeChainAndSnapshots = wipeChainAndSnapshots;
    this._getGeneration = getGeneration;
    this._emitPartialSyncStatus = emitPartialSyncStatus;
    this._getPartialSyncStatus = getPartialSyncStatus;
  }

  async handleInterruptedRecovery(currentGeneration: number): Promise<boolean> {
    const marker = await readMithrilPartialSyncMarker();

    if (!marker) {
      return false;
    }

    if (marker.state === 'node-start-verified') {
      // This marker state means a prior run already proved one successful node start on the installed DB.
      //  Reclaim leftover staging (close-without-dismiss or crash) and resume a normal boot.
      if (marker.stagingRootPath) {
        try {
          await fs.remove(marker.stagingRootPath);
        } catch (error) {
          // Reclaim is best-effort: a locked or busy staging directory must not
          // block a normal boot. The next partial sync start reclaims it when it
          // prepares the staging directory.
          logger.warn(
            'MithrilPartialSyncNodeStartup: staging reclaim failed; continuing normal boot',
            { error, stagingRootPath: marker.stagingRootPath }
          );
        }
      }
      await clearMithrilPartialSyncMarker();
      return false;
    }

    if (marker.state === 'installed-awaiting-node-start') {
      return false;
    }

    // This dialog is the single startup-interrupted recovery surface: startup-owned recovery must work
    //  before the renderer is ready and can't depend on diagnostics UI or a running node. In-session failures
    //  still use the React overlay (startInstalledNode catch).

    const { response } = await dialog.showMessageBox(this._mainWindow, {
      type: 'warning',
      buttons: ['Wipe chain and full Mithril sync', 'Quit'],
      defaultId: 0,
      cancelId: 1,
      noLink: true,
      title: 'Interrupted Mithril partial sync detected',
      message:
        'Daedalus found an interrupted Mithril partial sync after live chain replacement began. Normal startup is blocked until the chain data is wiped and a full Mithril sync can run again.',
    });

    if (currentGeneration !== this._getGeneration()) {
      return true;
    }

    if (response === 0) {
      await this._wipeChainAndSnapshots(
        'Interrupted Mithril partial sync detected on startup. Wiped chain directory and Mithril snapshots.',
        this._cardanoNode.state
      );
      if (marker.stagingRootPath) {
        try {
          await fs.remove(marker.stagingRootPath);
        } catch (error) {
          // Best-effort: a stuck staging directory must not block the wipe
          // recovery; the next partial sync start reclaims it.
          logger.warn(
            'MithrilPartialSyncNodeStartup: staging reclaim failed during startup wipe recovery; continuing',
            { error, stagingRootPath: marker.stagingRootPath }
          );
        }
      }
      await clearMithrilPartialSyncMarker();
      return false;
    }

    // The user chose Quit: actually exit. Startup stays blocked (return true)
    // while the log stream flushes and the process exits.
    logger.warn(
      'MithrilPartialSyncNodeStartup: user chose Quit at the interrupted-cutover recovery dialog; exiting',
      { markerState: marker.state }
    );
    safeExitWithCode(0);
    return true;
  }

  async startInstalledNode(currentGeneration: number): Promise<boolean> {
    const marker = await readMithrilPartialSyncMarker();

    if (!marker || marker.state !== 'installed-awaiting-node-start') {
      return false;
    }

    await this._emitPartialSyncStatus({
      ...this._getPartialSyncStatus(),
      status: 'starting-node',
      allowedRecoveryActions: ['wipe-and-full-sync'],
      error: null,
    });

    try {
      await this._cardanoNode.start();
      if (currentGeneration !== this._getGeneration()) {
        return true;
      }

      await this.finalizeInstalledNodeStart(currentGeneration);
      return true;
    } catch (error) {
      await this._emitPartialSyncStatus({
        ...this._getPartialSyncStatus(),
        status: 'failed',
        allowedRecoveryActions: ['wipe-and-full-sync'],
        error: {
          message:
            error instanceof Error
              ? error.message
              : 'Cardano node failed to start after Mithril partial sync cutover.',
          stage: 'starting-node',
        },
      });
      throw error;
    }
  }

  async finalizeInstalledNodeStart(currentGeneration: number): Promise<void> {
    const marker = await readMithrilPartialSyncMarker();

    if (!marker || marker.state !== 'installed-awaiting-node-start') {
      return;
    }

    await waitForNodeStartupProof();
    if (currentGeneration !== this._getGeneration()) {
      return;
    }

    if (this._cardanoNode.state !== CardanoNodeStates.RUNNING) {
      throw new Error(
        'Cardano node stopped responding during startup after Mithril partial sync cutover.'
      );
    }

    // Stamp node-start-verified and emit completed; the marker clear is deferred to the dismiss-driven
    //  finalize (finalizeCompletedPartialSync). Carry stagingRootPath forward so finalize can remove the exact dir.
    await writeMithrilPartialSyncMarker('node-start-verified', {
      managedChainPath: marker.managedChainPath,
      stagingRootPath: marker.stagingRootPath,
    });
    await this._emitPartialSyncStatus({
      ...this._getPartialSyncStatus(),
      status: 'completed',
      allowedRecoveryActions: [],
      error: null,
    });
  }

  async shouldSuppressStartupFallback(): Promise<boolean> {
    const marker = await readMithrilPartialSyncMarker();
    return marker?.state === 'installed-awaiting-node-start';
  }
}
