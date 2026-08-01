import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  MITHRIL_BOOTSTRAP_DECISION_CHANNEL,
  MITHRIL_BOOTSTRAP_SNAPSHOTS_CHANNEL,
  MITHRIL_SYNC_STATUS_CHANNEL,
  MITHRIL_SYNC_CANCEL_CHANNEL,
  MITHRIL_SYNC_START_CHANNEL,
  MITHRIL_SYNC_RESTART_NODE_CHANNEL,
} from '../../common/ipc/api';
import type {
  MithrilBootstrapDecisionRendererRequest,
  MithrilBootstrapDecisionMainResponse,
  MithrilBootstrapSnapshotsRendererRequest,
  MithrilBootstrapSnapshotsMainResponse,
  MithrilSyncStatusRendererRequest,
  MithrilSyncStatusMainResponse,
  MithrilSyncCancelRendererRequest,
  MithrilSyncCancelMainResponse,
  MithrilSyncStartRendererRequest,
  MithrilSyncStartMainResponse,
  MithrilSyncRestartNodeRendererRequest,
  MithrilSyncRestartNodeMainResponse,
} from '../../common/ipc/api';
import type { MithrilBootstrapStatusUpdate } from '../../common/types/mithril-bootstrap.types';
import type { CardanoNodeState } from '../../common/types/cardano-node.types';
import { getMithrilController } from '../mithril/MithrilController';

// Node state provider used by chainStorageChannel.ts
let _nodeStateProvider: () => CardanoNodeState | null | undefined = () =>
  undefined;

// True while the watchdog is holding at the empty-chain decision window
// (chain_status(false) received, user has not yet chosen genesis or Mithril).
// During this window the node process is not yet running, so storage directory
// changes must be allowed even though cardanoNode.state is STARTING.
let _isAwaitingChainDecision = false;

export const getMithrilBootstrapNodeState = ():
  | CardanoNodeState
  | null
  | undefined => {
  if (_isAwaitingChainDecision) return null; // bypass the STOPPED guard
  return _nodeStateProvider();
};

export const setMithrilBootstrapNodeStateProvider = (
  provider: () => CardanoNodeState | null | undefined
): void => {
  _nodeStateProvider = provider;
};

const decisionChannel: MainIpcChannel<
  MithrilBootstrapDecisionRendererRequest,
  MithrilBootstrapDecisionMainResponse
> = new MainIpcChannel(MITHRIL_BOOTSTRAP_DECISION_CHANNEL);

const syncStartChannel: MainIpcChannel<
  MithrilSyncStartRendererRequest,
  MithrilSyncStartMainResponse
> = new MainIpcChannel(MITHRIL_SYNC_START_CHANNEL);

const syncRestartNodeChannel: MainIpcChannel<
  MithrilSyncRestartNodeRendererRequest,
  MithrilSyncRestartNodeMainResponse
> = new MainIpcChannel(MITHRIL_SYNC_RESTART_NODE_CHANNEL);

const snapshotsChannel: MainIpcChannel<
  MithrilBootstrapSnapshotsRendererRequest,
  MithrilBootstrapSnapshotsMainResponse
> = new MainIpcChannel(MITHRIL_BOOTSTRAP_SNAPSHOTS_CHANNEL);

export const syncStatusChannel: MainIpcChannel<
  MithrilSyncStatusRendererRequest,
  MithrilSyncStatusMainResponse
> = new MainIpcChannel(MITHRIL_SYNC_STATUS_CHANNEL);

const syncCancelChannel: MainIpcChannel<
  MithrilSyncCancelRendererRequest,
  MithrilSyncCancelMainResponse
> = new MainIpcChannel(MITHRIL_SYNC_CANCEL_CHANNEL);

let _currentStatus: MithrilBootstrapStatusUpdate = { status: 'idle' };
let _pendingDecision: 'accept' | 'decline' | null = null;
let _window: BrowserWindow | null = null;
let _initialized = false;

export const pushBootstrapStatus = async (
  status: MithrilBootstrapStatusUpdate
): Promise<void> => {
  _currentStatus = status;
  if (_window && !_window.isDestroyed()) {
    await syncStatusChannel
      .send({ ...status, flowType: 'bootstrap' }, _window.webContents)
      .catch(() => {});
  }
};

// Called from CardanoNode when chain_status event arrives from the watchdog.
export const onWatchdogChainStatus = (hasChain: boolean): void => {
  _isAwaitingChainDecision = !hasChain;
  const controller = getMithrilController();
  if (hasChain) {
    pushBootstrapStatus({ status: 'idle' });
  } else {
    pushBootstrapStatus({ status: 'decision' });
    controller.setBootstrapStatusSender(pushBootstrapStatus);
  }
};

export const handleMithrilBootstrapRequests = (window: BrowserWindow): void => {
  _window = window;

  if (_initialized) return;
  _initialized = true;

  syncStatusChannel.onRequest(async () => {
    const ps = getMithrilController().getPartialSyncStatus();
    if (ps.status !== 'idle') {
      return {
        status: ps.status,
        flowType: 'partial-sync' as const,
        filesDownloaded: ps.transferProgress.filesDownloaded,
        filesTotal: ps.transferProgress.filesTotal,
        snapshotBytesDownloaded: ps.transferProgress.snapshotBytesDownloaded,
        snapshotBytesTotal: ps.transferProgress.snapshotBytesTotal,
        ancillaryBytesDownloaded: ps.transferProgress.ancillaryBytesDownloaded,
        ancillaryBytesTotal: ps.transferProgress.ancillaryBytesTotal,
        progressItems: ps.progressItems,
        error: ps.error ?? null,
        logPath: ps.logPath,
        allowedRecoveryActions: ps.allowedRecoveryActions,
      };
    }
    return { ..._currentStatus, flowType: 'bootstrap' as const };
  });

  decisionChannel.onRequest(async ({ decision }) => {
    _pendingDecision = decision;
    if (decision === 'decline') {
      _isAwaitingChainDecision = false; // genesis chosen — decision window closed
      getMithrilController().startBootstrapNode();
      pushBootstrapStatus({ status: 'idle' });
    }
  });

  // Unified start channel: wipeChain=true → bootstrap, wipeChain=false → partial sync
  syncStartChannel.onRequest(async ({ wipeChain }) => {
    if (wipeChain) {
      _isAwaitingChainDecision = false; // mithril chosen — decision window closed
      if (_pendingDecision === 'accept') {
        _pendingDecision = null;
      }
      // Ensure the bootstrap status sender is wired up — may not be set when
      // the user triggers wipe+full-sync from a partial-sync cancel recovery.
      getMithrilController().setBootstrapStatusSender(pushBootstrapStatus);
      getMithrilController().startMithril({ wipeChain: true });
      pushBootstrapStatus({ status: 'preparing' });
    } else {
      getMithrilController().startMithril({ wipeChain: false });
    }
  });

  // Unified restart-node channel: restart node after cancel
  syncRestartNodeChannel.onRequest(async () => {
    getMithrilController().startNode();
    pushBootstrapStatus({ status: 'idle' });
  });

  // Unified cancel channel
  syncCancelChannel.onRequest(async () => {
    await getMithrilController().cancelMithril();
  });

  snapshotsChannel.onRequest(async () => []);
};
