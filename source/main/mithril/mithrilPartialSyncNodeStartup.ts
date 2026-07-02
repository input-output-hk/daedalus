import fs from 'fs-extra';
import { dialog } from 'electron';
import type { BrowserWindow } from 'electron';

import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import type { CardanoNodeState } from '../../common/types/cardano-node.types';
import {
  emitMithrilPartialSyncStatus,
  getMithrilPartialSyncStatus,
} from '../ipc/mithrilPartialSyncChannel';
import type { CardanoNode } from '../cardano/CardanoNode';
import {
  clearMithrilPartialSyncMarker,
  readMithrilPartialSyncMarker,
  writeMithrilPartialSyncMarker,
} from './mithrilPartialSyncMarker';

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

  constructor({
    mainWindow,
    cardanoNode,
    wipeChainAndSnapshots,
    getGeneration,
  }: NodeStartupDependencies) {
    this._mainWindow = mainWindow;
    this._cardanoNode = cardanoNode;
    this._wipeChainAndSnapshots = wipeChainAndSnapshots;
    this._getGeneration = getGeneration;
  }

  async handleInterruptedRecovery(currentGeneration: number): Promise<boolean> {
    const marker = await readMithrilPartialSyncMarker();

    if (!marker) {
      return false;
    }

    if (marker.state === 'node-start-verified') {
      // Boundary C2: a prior run already proved one successful node start on the installed DB.
      // Reclaim leftover staging on a close-without-dismiss / crash, then resume
      // a normal boot. stagingRootPath is the durable colocated root persisted at cutover.
      if (marker.stagingRootPath) {
        await fs.remove(marker.stagingRootPath);
      }
      await clearMithrilPartialSyncMarker();
      return false;
    }

    if (marker.state === 'installed-awaiting-node-start') {
      return false;
    }

    // The native dialog below is the SINGLE authoritative startup-interrupted recovery
    // surface (startup-owned recovery must not depend on diagnostics UI or a running node, and works before
    // the renderer is ready). The redundant React `failed` emission was removed here to eliminate the
    // two-competing-surfaces problem. In-session failures keep the React overlay (startInstalledNode catch).

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
      await clearMithrilPartialSyncMarker();
      return false;
    }

    return true;
  }

  async startInstalledNode(currentGeneration: number): Promise<boolean> {
    const marker = await readMithrilPartialSyncMarker();

    if (!marker || marker.state !== 'installed-awaiting-node-start') {
      return false;
    }

    await emitMithrilPartialSyncStatus({
      ...getMithrilPartialSyncStatus(),
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
      await emitMithrilPartialSyncStatus({
        ...getMithrilPartialSyncStatus(),
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

    // Stamp node-start-verified (Boundary C2) and emit completed. The marker clear
    // is DEFERRED to the dismiss-driven service finalize (finalizeCompletedPartialSync). Carry the
    // durable stagingRootPath forward so the dismiss finalize / C2 reclaim can remove the exact dir.
    await writeMithrilPartialSyncMarker('node-start-verified', {
      managedChainPath: marker.managedChainPath,
      stagingRootPath: marker.stagingRootPath,
    });
    await emitMithrilPartialSyncStatus({
      ...getMithrilPartialSyncStatus(),
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
