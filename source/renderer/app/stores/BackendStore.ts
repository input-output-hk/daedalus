import { action, computed, observable, reaction, runInAction } from 'mobx';
import Store from './lib/Store';
import { logger } from '../utils/logging';
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
  watchdogStoppedChannel,
  backendStopStatusChannel,
} from '../ipc/nodePushChannel';
import {
  validateChainStorageChannel,
  confirmChainStorageChannel,
} from '../ipc/chainStorageChannel';
import type {
  LoadingPhase,
  MithrilProgress,
  ChainStorageValidation,
  BackendStopProgress,
  ChainDamage,
  RequestedRestart,
} from '../../../common/types/watchdog.types';

// DEFINE CONSTANTS
const BACKEND_STATUS_POLL_INTERVAL = 2000; // ms

// Codes of the errors a rebuild reports when the chain storage location itself
// is the problem; repeating the rebuild there fails the same way.
const LOCATION_ERROR_CODES = ['INVALID_CHAIN_PATH', 'CHAIN_DIR_OTHER_NETWORK'];
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
  // The code of the Mithril error that set lastError, e.g. PROBE_FAILED
  @observable lastErrorCode: string | null = null;
  // True when the Mithril error came from a rebuild of the chain data
  @observable lastErrorRebuild = false;
  // True when a confirmed rebuild has not finished, e.g. Daedalus was closed
  // after the chain data was deleted; the location is already decided
  @observable rebuildPending = false;
  // The last hole in the chain database the watchdog detected
  @observable chainDamage: ChainDamage | null = null;
  @observable walletUnrecoverable = false;
  @observable nodeUnrecoverable = false;
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
  // Set by "Not now" on the chain-damaged screen; lasts until the app restarts
  @observable _chainDamageDismissed = false;
  // Set by Retry after a rebuild failed on the chain storage location; the
  // picker shows until a location is confirmed
  @observable _chainStoragePickerRequested = false;
  // Set when that picker confirmed a location, so the next Retry rebuilds
  // there instead of asking again; cleared when a rebuild is sent
  @observable _chainStorageReselected = false;
  // Set by the error view's secondary action after a rebuild removed the chain
  // data, so the bootstrap decision shows instead of the error; cleared when
  // the Mithril phase moves on
  @observable _mithrilErrorDismissed = false;
  @observable _probeHasFired = false;
  // Daedalus is quitting
  @observable isStopping = false;
  // Latest progress of a backend stop, for quit or a requested restart
  @observable backendStopProgress: BackendStopProgress | null = null;
  // A restart the user asked for, until it has stopped the process
  @observable requestedRestart: RequestedRestart | null = null;
  // Chain storage paths (from BackendLifecycle, included in state poll)
  @observable defaultChainPath: string | null = null;
  @observable customChainPath: string | null = null;
  // The configured storage folder's validation when its chain subdirectory
  // holds another network's database, which the watchdog will neither start
  // the node on nor change; null otherwise
  @observable customChainPathValidation: ChainStorageValidation | null = null;
  // Runtime overrides (from watchdog-state.json via watchdog_started event)
  @observable nodeExtraArgs: string[] = [];
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
    watchdogStoppedChannel.onReceive(this._onWatchdogStopped);
    backendStopStatusChannel.onReceive(this._onBackendStopStatus);

    // ========== ACTION LISTENERS =========== //
    this.actions.networkStatus.restartNode.listen(this._restartNode);
    this.actions.networkStatus.restartWallet.listen(this._restartWallet);

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
        if (state.mithrilPhase !== 'error') this._mithrilErrorDismissed = false;
        this.mithrilProgress = state.mithrilProgress;
        this.lastError = state.lastError;
        this.lastErrorCode = state.lastErrorCode ?? null;
        this.lastErrorRebuild = state.lastErrorRebuild ?? false;
        this.rebuildPending = state.rebuildPending ?? false;
        this.chainDamage = state.chainDamage ?? null;
        this.walletUnrecoverable = state.walletUnrecoverable;
        this.nodeUnrecoverable = state.nodeUnrecoverable ?? false;
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
        this.nodeExtraArgs = state.nodeExtraArgs ?? [];
        if (state.shutdownRequested) {
          this.isStopping = true;
        }
        this.backendStopProgress = state.backendStopProgress;
        this.requestedRestart = state.requestedRestart ?? null;
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
      if (event.phase !== 'error') this._mithrilErrorDismissed = false;
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
      // Node is starting up — the old wallet port is no longer valid. Clearing
      // it immediately forces loadingPhase to 'node-starting' so the startup
      // progress screen shows without waiting for the 2-second poll cycle.
      this.walletPort = null;
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

  @action
  _onWatchdogStopped = async (): Promise<void> => {
    runInAction('set isStopping on watchdog stopped', () => {
      this.isStopping = true;
    });
  };

  @action
  _onBackendStopStatus = async (event: {
    quitting: boolean;
    restart: RequestedRestart | null;
    progress: BackendStopProgress | null;
  }): Promise<void> => {
    runInAction('update backend stop status from push', () => {
      if (event.quitting) {
        this.isStopping = true;
      }
      this.requestedRestart = event.restart;
      this.backendStopProgress = event.progress;
    });
  };

  // =============== COMPUTED ===============
  @computed
  get mithrilPromptDismissed(): boolean {
    return this._mithrilPromptDismissed;
  }

  // A Mithril pipeline is running, or has failed and shows its error
  @computed
  get _mithrilActive(): boolean {
    const phase = this._shownMithrilPhase;
    return phase !== null && phase !== 'completed' && phase !== 'cancelled';
  }

  // The Mithril phase the loading screens go by: a dismissed error counts as
  // no Mithril run
  @computed
  get _shownMithrilPhase(): string | null {
    return this._mithrilErrorDismissed && this.mithrilPhase === 'error'
      ? null
      : this.mithrilPhase;
  }

  @computed
  get loadingPhase(): LoadingPhase {
    // Daedalus is quitting: the backend is being stopped, whatever state it
    // was in before
    if (this.isStopping) {
      return 'stopping';
    }
    // The chain database has a hole: offer the rebuild before anything else,
    // including a ready wallet. A running Mithril pipeline keeps the screen.
    if (
      this.chainDamage !== null &&
      !this._chainDamageDismissed &&
      !this._mithrilActive
    ) {
      return 'chain-damaged';
    }
    // Retry after a rebuild failed on the storage location: pick another one
    if (this._chainStoragePickerRequested) {
      return 'chain-storage-setup';
    }
    // Unrecoverable error takes top priority
    if (this.walletUnrecoverable || this.nodeUnrecoverable) {
      return 'error';
    }
    // A restart the user asked for: the process is stopping, or for a wallet
    // restart, starting again. A node restart continues with the startup
    // phases below once the new node has started.
    if (this.requestedRestart !== null) {
      return 'stopping';
    }
    // No chain_status received yet
    if (this.hasChain === null) {
      return 'starting';
    }
    // chain_status { has_chain: false } — show storage picker first, then decision
    if (
      this.hasChain === false &&
      (this._shownMithrilPhase === null ||
        this._shownMithrilPhase === 'cancelled')
    ) {
      return this.chainPathConfirmed || this.rebuildPending
        ? 'bootstrap-decision'
        : 'chain-storage-setup';
    }
    // Mithril sync is in progress (phase non-null, not yet completed or cancelled)
    if (this._mithrilActive) {
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
    this._mithrilErrorDismissed = false;
    mithrilCommandChannel.send({ cmd: 'start_mithril' });
  };

  @action
  startMithrilForce = () => {
    this._mithrilErrorDismissed = false;
    mithrilCommandChannel.send({ cmd: 'start_mithril', force: true });
  };

  // Deletes the chain database and downloads a verified copy with Mithril.
  // The rebuild targets the chain location already in use, so that location
  // counts as confirmed: if the rebuild fails and the watchdog reports no
  // chain, the bootstrap decision follows without the location picker.
  @action
  rebuildChain = () => {
    this.chainPathConfirmed = true;
    this._chainStorageReselected = false;
    this._mithrilErrorDismissed = false;
    mithrilCommandChannel.send({ cmd: 'start_mithril', wipe_chain: true });
    // The logger sends its arguments over IPC, which cannot clone a MobX
    // observable, so only plain values go in.
    logger.info('BackendStore: rebuilding the chain database', {
      kind: this.chainDamage?.kind ?? null,
      slot: this.chainDamage?.slot ?? null,
    });
  };

  // Retry on the Mithril error view. After a failed or refused rebuild it
  // repeats the rebuild, since the chain data may already be gone and a plain
  // sync would not replace what is left; when the storage location caused the
  // failure it asks for a location instead. Otherwise it runs Mithril again.
  @action
  retryMithril = () => {
    if (!this.lastErrorRebuild) {
      this.startMithril();
    } else if (
      !this._chainStorageReselected &&
      this.lastErrorCode != null &&
      LOCATION_ERROR_CODES.includes(this.lastErrorCode)
    ) {
      this._chainStoragePickerRequested = true;
    } else {
      this.rebuildChain();
    }
  };

  // "Not now" on the chain-damaged screen; the screen stays hidden until the
  // app restarts, and Diagnostics keeps the detection
  @action
  dismissChainDamage = () => {
    this._chainDamageDismissed = true;
  };

  @action
  startNode = () => {
    // The watchdog clears the Mithril phase when it gets the command; hiding
    // the error now keeps it off the screen until the next poll shows that.
    this._mithrilErrorDismissed = true;
    mithrilCommandChannel.send({ cmd: 'start_node' });
  };

  // Secondary action of the Mithril error view after a rebuild removed the
  // chain data: back to the choice between Mithril and a sync from genesis
  @action
  showBootstrapDecision = () => {
    this._mithrilErrorDismissed = true;
  };

  @action
  cancelMithril = () => {
    mithrilCommandChannel.send({ cmd: 'cancel_mithril' });
  };

  @action
  probeMithril = () => {
    mithrilCommandChannel.send({ cmd: 'probe_mithril' });
  };

  _restartNode = () => {
    mithrilCommandChannel.send({ cmd: 'restart_node' });
  };

  _restartWallet = () => {
    mithrilCommandChannel.send({ cmd: 'restart_wallet' });
  };

  // Retries after the watchdog has given up on a process that kept crashing:
  // the wallet alone when only the wallet failed, otherwise node and wallet.
  retryBackend = () => {
    mithrilCommandChannel.send({
      cmd:
        this.walletUnrecoverable && !this.nodeUnrecoverable
          ? 'restart_wallet'
          : 'restart_node',
    });
  };

  // =============== CHAIN STORAGE ACTIONS ===============

  validateChainStorageDirectory = async (
    path: string
  ): Promise<ChainStorageValidation> => {
    return validateChainStorageChannel.request({ path });
  };

  // Checks the configured storage folder when the picker is shown. A folder
  // was accepted when it was picked, but another network's Daedalus can write
  // its database there later, and a folder picked before such folders were
  // refused can hold one. Only that case is recorded: the watchdog refuses
  // such a database, so the picker must not offer to continue with it.
  checkCustomChainPath = async (): Promise<void> => {
    const path = this.customChainPath;
    let validation: ChainStorageValidation | null = null;
    try {
      validation =
        path == null
          ? null
          : await validateChainStorageChannel.request({ path });
    } catch {
      validation = null;
    }
    runInAction('set customChainPathValidation', () => {
      this.customChainPathValidation =
        validation != null &&
        validation.reason === 'chain-subdirectory-other-network'
          ? validation
          : null;
    });
  };

  @action
  setChainStorageDirectory = async (
    customPath: string | null
  ): Promise<ChainStorageValidation | null> => {
    await confirmChainStorageChannel.request({ customPath });
    runInAction('set chainPathConfirmed after setChainStorageDirectory', () => {
      this.chainPathConfirmed = true;
      this._confirmPickedLocation();
    });
    return null;
  };

  @action
  resetChainStorageDirectory =
    async (): Promise<ChainStorageValidation | null> => {
      await confirmChainStorageChannel.request({ customPath: null });
      runInAction(
        'set chainPathConfirmed after resetChainStorageDirectory',
        () => {
          this.chainPathConfirmed = true;
          this._confirmPickedLocation();
        }
      );
      return null;
    };

  @action
  confirmStorageLocation = () => {
    runInAction('set chainPathConfirmed', () => {
      this.chainPathConfirmed = true;
      this._confirmPickedLocation();
    });
  };

  // A location was confirmed on the picker that Retry asked for
  _confirmPickedLocation() {
    if (this._chainStoragePickerRequested) {
      this._chainStorageReselected = true;
    }
    this._chainStoragePickerRequested = false;
  }

  @action
  dismissMithrilPrompt = () => {
    runInAction('dismiss mithril prompt', () => {
      this._mithrilPromptDismissed = true;
      this.mithrilSignificantlyBehind = null;
    });
  };
}
