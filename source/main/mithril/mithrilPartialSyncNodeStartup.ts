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
      await clearMithrilPartialSyncMarker();
      return false;
    }

    if (marker.state === 'installed-awaiting-node-start') {
      return false;
    }

    await emitMithrilPartialSyncStatus({
      status: 'failed',
      allowedRecoveryActions: ['wipe-and-full-sync'],
      transferProgress: {},
      progressItems: [],
      error: {
        message:
          'Daedalus detected an interrupted Mithril partial sync after live chain cutover. The installed chain data is not safe to start normally.',
        stage: 'installing',
      },
    });

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

    await clearMithrilPartialSyncMarker();
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
