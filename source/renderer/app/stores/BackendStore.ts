import { action, computed, observable, reaction, runInAction } from 'mobx';
import Store from './lib/Store';
import { getCachedBackendStatusChannel } from '../ipc/backendStatusChannel';
import { mithrilCommandChannel } from '../ipc/mithrilCommandChannel';
import {
  mithrilProgressChannel,
  mithrilStatusChannel,
  walletPortChannel,
} from '../ipc/mithrilPushChannel';
import {
  nodeStartupStatusChannel,
  nodeBlockSyncProgressChannel,
} from '../ipc/nodePushChannel';
import {
  validateChainStorageChannel,
  confirmChainStorageChannel,
} from '../ipc/chainStorageChannel';
import type {
  LoadingPhase,
  MithrilProgress,
  ChainStorageValidation,
} from '../../../common/types/watchdog.types';

// DEFINE CONSTANTS
const BACKEND_STATUS_POLL_INTERVAL = 2000; // ms
// END CONSTANTS

export default class BackendStore extends Store {
  // Poll interval handle
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _backendStatusPollingInterval: IntervalID | null | undefined = null;

  // Observable state — populated from WatchdogState poll
  @observable watchdogPid = 0;
  @observable nodePid = 0;
  @observable walletPid = 0;
  @observable nodeStartedAt: number | null = null;
  @observable walletStartedAt: number | null = null;
  @observable walletRestartCount = 0;
  @observable walletPort: number | null = null;
  @observable hasChain: boolean | null = null;
  @observable nodeStartupPhase: string | null = null;
  @observable blockSyncProgress = {
    replayedBlock: 0,
    validatingChunk: 0,
    pushingLedger: 0,
  };
  @observable mithrilPhase: string | null = null;
  @observable mithrilProgress: MithrilProgress | null = null;
  @observable lastError: string | null = null;
  @observable walletUnrecoverable = false;
  @observable nodeSocketWaitMs: number | null = null;
  @observable walletReadyWaitMs: number | null = null;
  @observable nodeForceKilled = false;
  @observable lastWalletExitCode: number | null = null;
  @observable lastWalletExitSignal: string | null = null;
  @observable mithrilSignificantlyBehind: {
    localImmutableCount: number;
    latestCertifiedImmutable: number;
  } | null = null;
  @observable _mithrilPromptDismissed = false;
  @observable _probeHasFired = false;
  // Chain storage paths (from BackendLifecycle, included in state poll)
  @observable defaultChainPath: string | null = null;
  @observable customChainPath: string | null = null;
  // True once user has confirmed their chain storage location this session;
  // survives watchdog restarts so we skip the picker on the second chain_status.
  @observable chainPathConfirmed = false;

  setup() {
    // ========== POLL INTERVAL =========== //
    this._backendStatusPollingInterval = setInterval(
      this._pollBackendStatus,
      BACKEND_STATUS_POLL_INTERVAL
    );

    // ========== PUSH CHANNEL LISTENERS =========== //
    mithrilProgressChannel.onReceive(this._onMithrilProgress);
    mithrilStatusChannel.onReceive(this._onMithrilStatus);
    walletPortChannel.onReceive(this._onWalletPort);
    nodeStartupStatusChannel.onReceive(this._onNodeStartupStatus);
    nodeBlockSyncProgressChannel.onReceive(this._onNodeBlockSyncProgress);

    // ========== MOBX REACTIONS =========== //
    // Fire probeMithril() exactly once when nodeStartedAt transitions from null to non-null.
    const disposeProbeReaction = reaction(
      () => this.nodeStartedAt,
      (nodeStartedAt) => {
        if (nodeStartedAt !== null && !this._probeHasFired) {
          this.probeMithril();
          runInAction('set _probeHasFired', () => {
            this._probeHasFired = true;
          });
          disposeProbeReaction();
        }
      }
    );
  }

  teardown() {
    super.teardown();
    if (this._backendStatusPollingInterval) {
      clearInterval(this._backendStatusPollingInterval);
      this._backendStatusPollingInterval = null;
    }
  }

  // =============== POLL HANDLER ===============
  @action
  _pollBackendStatus = async () => {
    try {
      const state = await getCachedBackendStatusChannel.request();
      if (!state) return;
      runInAction('update WatchdogState from poll', () => {
        this.watchdogPid = state.watchdogPid;
        this.nodePid = state.nodePid;
        this.walletPid = state.walletPid;
        this.nodeStartedAt = state.nodeStartedAt;
        this.walletStartedAt = state.walletStartedAt;
        this.walletRestartCount = state.walletRestartCount;
        this.walletPort = state.walletPort;
        this.hasChain = state.hasChain;
        this.nodeStartupPhase = state.nodeStartupPhase;
        this.blockSyncProgress = { ...state.blockSyncProgress };
        this.mithrilPhase = state.mithrilPhase;
        this.mithrilProgress = state.mithrilProgress;
        this.lastError = state.lastError;
        this.walletUnrecoverable = state.walletUnrecoverable;
        this.nodeSocketWaitMs = state.nodeSocketWaitMs;
        this.walletReadyWaitMs = state.walletReadyWaitMs;
        this.nodeForceKilled = state.nodeForceKilled;
        this.lastWalletExitCode = state.lastWalletExitCode;
        this.lastWalletExitSignal = state.lastWalletExitSignal;
        if (!this._mithrilPromptDismissed) {
          this.mithrilSignificantlyBehind = state.mithrilSignificantlyBehind;
        }
        this.defaultChainPath = state.defaultChainPath;
        this.customChainPath = state.customChainPath;
      });
    } catch (error) {} // eslint-disable-line
  };

  // =============== PUSH HANDLERS ===============
  @action
  _onMithrilProgress = async (progress: MithrilProgress): Promise<void> => {
    runInAction('update mithrilProgress', () => {
      this.mithrilProgress = progress;
    });
  };

  @action
  _onMithrilStatus = async (event: { phase: string }): Promise<void> => {
    runInAction('update mithrilPhase', () => {
      this.mithrilPhase = event.phase;
    });
  };

  @action
  _onWalletPort = async (event: {
    port: number;
    ca: number[];
    cert: number[];
    key: number[];
  }): Promise<void> => {
    runInAction('update walletPort from push', () => {
      this.walletPort = event.port;
    });
    // Reconfigure AdaApi so it connects to the correct port with the right TLS certs.
    this.api.ada.setRequestConfig({
      hostname: '127.0.0.1',
      port: event.port,
      ca: Uint8Array.from(event.ca),
      cert: Uint8Array.from(event.cert),
      key: Uint8Array.from(event.key),
    });
  };

  @action
  _onNodeStartupStatus = async (event: { phase: string }): Promise<void> => {
    runInAction('update nodeStartupPhase from push', () => {
      this.nodeStartupPhase = event.phase;
    });
  };

  @action
  _onNodeBlockSyncProgress = async (event: {
    kind: string;
    progress: number;
  }): Promise<void> => {
    runInAction('update blockSyncProgress from push', () => {
      const { kind, progress } = event;
      if (kind === 'replayedBlock') {
        this.blockSyncProgress.replayedBlock = progress;
      } else if (kind === 'validatingChunk') {
        this.blockSyncProgress.validatingChunk = progress;
      } else if (kind === 'pushingLedger') {
        this.blockSyncProgress.pushingLedger = progress;
      }
    });
  };

  // =============== COMPUTED ===============
  @computed
  get mithrilPromptDismissed(): boolean {
    return this._mithrilPromptDismissed;
  }

  @computed
  get loadingPhase(): LoadingPhase {
    // Unrecoverable error takes top priority
    if (this.walletUnrecoverable) {
      return 'error';
    }
    // No chain_status received yet
    if (this.hasChain === null) {
      return 'starting';
    }
    // chain_status { has_chain: false } — show storage picker first, then decision
    if (
      this.hasChain === false &&
      (this.mithrilPhase === null || this.mithrilPhase === 'cancelled')
    ) {
      return this.chainPathConfirmed ? 'bootstrap-decision' : 'chain-storage-setup';
    }
    // Mithril sync is in progress (phase non-null, not yet completed or cancelled)
    if (
      this.mithrilPhase !== null &&
      this.mithrilPhase !== 'completed' &&
      this.mithrilPhase !== 'cancelled'
    ) {
      return 'mithril-syncing';
    }
    // Wallet not yet ready
    if (this.walletPort === null) {
      return 'node-starting';
    }
    // Wallet ready
    return 'ready';
  }

  // =============== ACTIONS ===============
  @action
  startMithril = () => {
    mithrilCommandChannel.send({ cmd: 'start_mithril' });
  };

  @action
  startMithrilForce = () => {
    mithrilCommandChannel.send({ cmd: 'start_mithril', force: true });
  };

  @action
  startNode = () => {
    mithrilCommandChannel.send({ cmd: 'start_node' });
  };

  @action
  cancelMithril = () => {
    mithrilCommandChannel.send({ cmd: 'cancel_mithril' });
  };

  @action
  probeMithril = () => {
    mithrilCommandChannel.send({ cmd: 'probe_mithril' });
  };

  // =============== CHAIN STORAGE ACTIONS ===============

  validateChainStorageDirectory = async (
    path: string
  ): Promise<ChainStorageValidation> => {
    return validateChainStorageChannel.send({ path });
  };

  @action
  setChainStorageDirectory = async (
    customPath: string | null
  ): Promise<ChainStorageValidation | null> => {
    await confirmChainStorageChannel.send({ customPath });
    runInAction('set chainPathConfirmed after setChainStorageDirectory', () => {
      this.chainPathConfirmed = true;
    });
    return null;
  };

  @action
  resetChainStorageDirectory = async (): Promise<ChainStorageValidation | null> => {
    await confirmChainStorageChannel.send({ customPath: null });
    runInAction('set chainPathConfirmed after resetChainStorageDirectory', () => {
      this.chainPathConfirmed = true;
    });
    return null;
  };

  @action
  confirmStorageLocation = () => {
    runInAction('set chainPathConfirmed', () => {
      this.chainPathConfirmed = true;
    });
  };

  @action
  dismissMithrilPrompt = () => {
    runInAction('dismiss mithril prompt', () => {
      this._mithrilPromptDismissed = true;
      this.mithrilSignificantlyBehind = null;
    });
  };
}
