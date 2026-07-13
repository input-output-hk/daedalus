import fs from 'fs-extra';
import type { CardanoNodeState } from '../../common/types/cardano-node.types';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import type {
  MithrilBootstrapDecision,
  MithrilBootstrapStatusUpdate,
} from '../../common/types/mithril-bootstrap.types';
import { isMithrilBootstrapBlockingNodeStart } from '../../common/types/mithril-bootstrap.types';
import type { MithrilPartialSyncStatusSnapshot } from '../../common/types/mithril-partial-sync.types';
import { isMithrilPartialSyncBlockingNodeStart } from '../../common/types/mithril-partial-sync.types';
import { chainStorageCoordinator } from '../utils/chainStorageCoordinator';
import type { ManagedChainLayoutResult } from '../utils/chainStorageManagerShared';
import { logger } from '../utils/logging';
import type { CardanoNode } from '../cardano/CardanoNode';
import { MithrilPartialSyncNodeStartup } from './mithrilPartialSyncNodeStartup';
import { isMithrilDecisionCancelledError } from './mithrilDecision';

const MITHRIL_COMPLETION_DELAY_MS = 6000;

export type MithrilStartupGateState =
  | 'idle'
  | 'awaitingBootstrapDecision'
  | 'bootstrapRunning'
  | 'bootstrapFailedAwaitingDecision'
  | 'bootstrapCancelledAwaitingDecision'
  | 'partialSyncRunning'
  | 'partialSyncCutoverUnsafe'
  | 'startingNodeAfterRestore';

export type MithrilStartupGateResult<TResponse> = {
  handled: boolean;
  response: TResponse;
};

export type MithrilStartupGateDependencies = {
  cardanoNode: CardanoNode;
  wipeChainFlag: boolean;
  mithrilLockFilePath: string;
  markManagedChainLayoutError(error: unknown): Error;
  isManagedChainLayoutError(error: unknown): boolean;
  getGeneration(): number;
  partialSyncNodeStartup: MithrilPartialSyncNodeStartup;
};

type MithrilStartupGateController = {
  getBootstrapStatus(): MithrilBootstrapStatusUpdate;
  getPartialSyncStatus(): MithrilPartialSyncStatusSnapshot;
  getPendingBootstrapDecision(): MithrilBootstrapDecision | null;
  setBootstrapStatus(
    update: Partial<MithrilBootstrapStatusUpdate>
  ): MithrilBootstrapStatusUpdate;
  waitForBootstrapDecision(): Promise<MithrilBootstrapDecision>;
  resetBootstrapDecisionState(options?: {
    suppressStatusBroadcast?: boolean;
  }): void;
  broadcastBootstrapStatus(status: MithrilBootstrapStatusUpdate): Promise<void>;
};

export class MithrilStartupGate {
  _controller: MithrilStartupGateController;
  _state: MithrilStartupGateState = 'idle';
  _dependencies: MithrilStartupGateDependencies | null = null;
  _decisionInFlight = false;
  _failureDecisionInFlight = false;
  _declineInFlight: Record<'failure' | 'cancelled', boolean> = {
    failure: false,
    cancelled: false,
  };
  _startupCheckDone = false;
  _startupLayoutResult: ManagedChainLayoutResult | null = null;
  _decisionPrompted = false;
  _decision: MithrilBootstrapDecision | null = null;
  _bootstrapCompleted = false;
  _startInFlight = false;

  constructor(controller: MithrilStartupGateController) {
    this._controller = controller;
  }

  configure(dependencies: MithrilStartupGateDependencies): void {
    this._dependencies = dependencies;
  }

  onBootstrapStatus(status: MithrilBootstrapStatusUpdate): void {
    if (status.status === 'completed') {
      this._bootstrapCompleted = true;
      this._transition('bootstrapRunning', 'bootstrapCompleted');
      this.startNodeAfterMithrilCompletion().catch((error) => {
        logger.error(
          '[MITHRIL] Failed to start node after bootstrap completion',
          {
            error,
          }
        );
      });
      return;
    }

    if (status.status === 'failed') {
      this._waitForFailureDecline();
      return;
    }

    if (status.status === 'decision') {
      this._transition('awaitingBootstrapDecision', 'decisionStatus');
    } else if (
      [
        'preparing',
        'downloading',
        'verifying',
        'converting',
        'finalizing',
      ].includes(status.status)
    ) {
      this._transition('bootstrapRunning', 'bootstrapActive');
    } else if (status.status === 'cancelled') {
      this._transition(
        'bootstrapCancelledAwaitingDecision',
        'bootstrapCancelled'
      );
    } else if (status.status === 'idle') {
      this._transition('idle', 'bootstrapIdle');
    }
  }

  onBootstrapDecision(decision: MithrilBootstrapDecision): void {
    this._decision = decision;
    if (decision !== 'decline') return;

    const status = this._controller.getBootstrapStatus().status;
    const generation = this._getGeneration();

    if (status === 'failed') {
      this._handleDecline('failure', 'decision-listener', generation).catch(
        (error) => {
          logger.error('[MITHRIL] Decline handling failed after decision', {
            error,
          });
        }
      );
      return;
    }

    if (status === 'cancelled') {
      this._handleDecline('cancelled', 'decision-listener', generation).catch(
        (error) => {
          logger.error('[MITHRIL] Decline handling failed after cancel', {
            error,
          });
        }
      );
    }
  }

  onPartialSyncStatus(status: MithrilPartialSyncStatusSnapshot): void {
    if (status.status === 'installing') {
      this._transition('partialSyncCutoverUnsafe', 'partialSyncInstalling');
    } else if (status.status === 'starting-node') {
      this._transition('startingNodeAfterRestore', 'partialSyncStartingNode');
    } else if (status.status !== 'idle') {
      this._transition('partialSyncRunning', 'partialSyncActive');
    } else {
      this._transition('idle', 'partialSyncIdle');
    }
  }

  async ensureMithrilStartupGate(
    currentGeneration: number
  ): Promise<ManagedChainLayoutResult | null> {
    const deps = this._requireDependencies();

    if (this._startupCheckDone && this._startupLayoutResult) {
      return this._startupLayoutResult;
    }

    let layoutResult: ManagedChainLayoutResult;

    try {
      layoutResult =
        await this._ensureLayoutAndHandleWipeFlag(currentGeneration);
      if (currentGeneration !== deps.getGeneration()) return null;

      if (await fs.pathExists(deps.mithrilLockFilePath)) {
        await chainStorageCoordinator.wipeChainAndSnapshots(
          'Incomplete bootstrap detected. Wiped chain directory and Mithril snapshots.',
          deps.cardanoNode.state
        );
        if (currentGeneration !== deps.getGeneration()) return null;
        this._decisionPrompted = false;
        this._decision = null;
      }

      const blocked =
        await deps.partialSyncNodeStartup.handleInterruptedRecovery(
          currentGeneration
        );
      if (blocked) {
        this._transition(
          'partialSyncCutoverUnsafe',
          'partialSyncMarkerDetected'
        );
        return null;
      }
    } catch (error) {
      logger.error('[MITHRIL] Failed to verify managed chain layout', {
        error,
      });
      throw deps.markManagedChainLayoutError(error);
    }

    this._startupCheckDone = true;
    this._startupLayoutResult = layoutResult;
    return layoutResult;
  }

  resetOnDirectoryChange(): void {
    this._decisionInFlight = false;
    this._failureDecisionInFlight = false;
    this._declineInFlight = { failure: false, cancelled: false };
    this._startupCheckDone = false;
    this._startupLayoutResult = null;
    this._decisionPrompted = false;
    this._bootstrapCompleted = false;
    this._startInFlight = false;
    this._controller.resetBootstrapDecisionState({
      suppressStatusBroadcast: true,
    });
    this._transition('idle', 'directoryChanged');
  }

  syncPendingDecision(): void {
    const latestDecision = this._controller.getPendingBootstrapDecision();
    if (latestDecision && latestDecision !== this._decision) {
      this._decision = latestDecision;
    }
  }

  async handleStoppedNodeStartup<TResponse>(options: {
    currentGeneration: number;
    getStaleResponse: () => TResponse;
    response: TResponse;
  }): Promise<MithrilStartupGateResult<TResponse>> {
    const deps = this._requireDependencies();
    const { currentGeneration, getStaleResponse, response } = options;
    const chainEmpty = await chainStorageCoordinator.isManagedChainEmpty();
    if (currentGeneration !== deps.getGeneration()) {
      return { handled: true, response: getStaleResponse() };
    }

    return chainEmpty
      ? this._handleEmptyChainStartup(options)
      : this._handleExistingChainStartup(options);
  }

  canStartNodeAfterMithrilCompletion(): boolean {
    const deps = this._dependencies;
    if (!deps || this._startInFlight) return false;
    return [
      CardanoNodeStates.STOPPED,
      CardanoNodeStates.CRASHED,
      CardanoNodeStates.ERRORED,
    ].includes(deps.cardanoNode.state);
  }

  async startNodeAfterMithrilCompletion(): Promise<void> {
    const deps = this._requireDependencies();
    if (!this.canStartNodeAfterMithrilCompletion()) return;

    this._startInFlight = true;
    const generation = deps.getGeneration();
    try {
      await this._emitStartingNodeStatus();
      await deps.cardanoNode.start();
      if (generation !== deps.getGeneration()) return;
      await this._waitForNodeStartupProof();
      if (generation !== deps.getGeneration()) return;
      if (deps.cardanoNode.state !== CardanoNodeStates.RUNNING) {
        throw new Error(
          'Cardano node stopped responding during startup after Mithril bootstrap.'
        );
      }
      await this._emitIdleStatus();
      this._bootstrapCompleted = false;
      this._decision = null;
      this._decisionPrompted = false;
    } catch (error) {
      logger.error('[MITHRIL] Failed to start cardano-node after bootstrap', {
        error,
      });
      this._bootstrapCompleted = false;
      await this._emitStartFailure(error);
    } finally {
      this._startInFlight = false;
    }
  }

  async _handleDecline(
    kind: 'failure' | 'cancelled',
    source: string,
    currentGeneration: number = this._getGeneration()
  ): Promise<boolean> {
    const deps = this._requireDependencies();
    if (this._declineInFlight[kind]) return false;

    this._declineInFlight[kind] = true;
    try {
      if (this._controller.getPendingBootstrapDecision() !== 'decline') {
        return false;
      }
      if (
        kind === 'cancelled' &&
        this._controller.getBootstrapStatus().status !== 'cancelled'
      ) {
        return false;
      }

      await this._emitIdleStatus();
      if (currentGeneration !== deps.getGeneration()) return false;
      await chainStorageCoordinator.wipeChainAndSnapshots(
        `User declined after bootstrap ${
          kind === 'failure' ? 'failure' : 'cancel'
        } (${source}). Wiped chain directory and Mithril snapshots.`,
        deps.cardanoNode.state
      );
      if (currentGeneration !== deps.getGeneration()) return false;
      await deps.cardanoNode.start();
      this._decision = null;
      this._decisionPrompted = false;
      return true;
    } finally {
      this._declineInFlight[kind] = false;
    }
  }

  async _ensureLayoutAndHandleWipeFlag(
    currentGeneration: number
  ): Promise<ManagedChainLayoutResult> {
    const deps = this._requireDependencies();

    if (!deps.wipeChainFlag) {
      return chainStorageCoordinator.ensureManagedChainLayout(
        deps.cardanoNode.state
      );
    }

    if (
      deps.cardanoNode.state !== CardanoNodeStates.STOPPING &&
      deps.cardanoNode.state !== CardanoNodeStates.STOPPED
    ) {
      try {
        await deps.cardanoNode.stop();
      } catch (error) {
        logger.warn('[MITHRIL] Failed to stop cardano-node for wipe', {
          error,
        });
      }
    }

    const layoutResult = await chainStorageCoordinator.ensureManagedChainLayout(
      deps.cardanoNode.state
    );
    if (currentGeneration !== deps.getGeneration()) return layoutResult;
    await chainStorageCoordinator.wipeChainAndSnapshots(
      'wipe-chain flag set. Wiped chain directory and Mithril snapshots.',
      deps.cardanoNode.state
    );
    return layoutResult;
  }

  async _handleTerminalStatusDecline<TResponse>(options: {
    status: MithrilBootstrapStatusUpdate['status'];
    source: string;
    currentGeneration: number;
    response: TResponse;
  }): Promise<MithrilStartupGateResult<TResponse> | null> {
    const { status, source, currentGeneration, response } = options;
    if (status !== 'failed' && status !== 'cancelled') return null;
    await this._handleDecline(
      status === 'failed' ? 'failure' : 'cancelled',
      source,
      currentGeneration
    );
    this._markHadNotEnoughSpaceLeft(response);
    return { handled: true, response };
  }

  async _handleEmptyChainStartup<TResponse>(options: {
    currentGeneration: number;
    getStaleResponse: () => TResponse;
    response: TResponse;
  }): Promise<MithrilStartupGateResult<TResponse>> {
    const { currentGeneration, response } = options;
    const status = this._controller.getBootstrapStatus().status;

    const declineResult = await this._handleTerminalStatusDecline({
      status,
      source: 'polling-chain-empty',
      currentGeneration,
      response,
    });
    if (declineResult) return declineResult;

    if (status === 'completed' || status === 'starting-node') {
      this._bootstrapCompleted = true;
    }

    if (this._bootstrapCompleted) {
      await this.startNodeAfterMithrilCompletion();
      return { handled: true, response };
    }

    if (this._decision === 'accept') return { handled: true, response };

    if (!this._decisionPrompted) {
      await this._emitDecisionStatus();
      this._decisionPrompted = true;
    }

    if (this._decision === 'decline') {
      await this._handleBootstrapDecline(
        'bootstrap decline',
        currentGeneration
      );
      return { handled: true, response };
    }

    this._waitForInitialDecision(currentGeneration);
    this._markHadNotEnoughSpaceLeft(response);
    return { handled: true, response };
  }

  async _handleExistingChainStartup<TResponse>(options: {
    currentGeneration: number;
    getStaleResponse: () => TResponse;
    response: TResponse;
  }): Promise<MithrilStartupGateResult<TResponse>> {
    const deps = this._requireDependencies();
    const { currentGeneration, getStaleResponse, response } = options;
    const bootstrapStatus = this._controller.getBootstrapStatus().status;
    const partialSyncStatus = this._controller.getPartialSyncStatus().status;

    const declineResult = await this._handleTerminalStatusDecline({
      status: bootstrapStatus,
      source: 'polling-chain-present',
      currentGeneration,
      response,
    });
    if (declineResult) return declineResult;

    if (isMithrilBootstrapBlockingNodeStart(bootstrapStatus)) {
      return { handled: true, response };
    }

    if (
      await deps.partialSyncNodeStartup.startInstalledNode(currentGeneration)
    ) {
      this._markHadNotEnoughSpaceLeft(response);
      return { handled: true, response };
    }

    if (isMithrilPartialSyncBlockingNodeStart(partialSyncStatus)) {
      return { handled: true, response };
    }

    await this._emitIdleStatus();
    if (currentGeneration !== deps.getGeneration()) {
      return { handled: true, response: getStaleResponse() };
    }
    this._decisionInFlight = false;
    this._decision = null;
    this._decisionPrompted = false;
    this._controller.resetBootstrapDecisionState({
      suppressStatusBroadcast: true,
    });
    await deps.cardanoNode.start();
    await deps.partialSyncNodeStartup.finalizeInstalledNodeStart(
      currentGeneration
    );
    return { handled: true, response };
  }

  _waitForFailureDecline(): void {
    if (this._failureDecisionInFlight) return;
    this._failureDecisionInFlight = true;
    this._transition('bootstrapFailedAwaitingDecision', 'bootstrapFailed');
    const generation = this._getGeneration();
    this._controller
      .waitForBootstrapDecision()
      .then(async (decision) => {
        if (decision === 'decline') {
          await this._handleDecline('failure', 'status-listener', generation);
        }
      })
      .catch((error) => {
        if (!isMithrilDecisionCancelledError(error)) {
          logger.error('[MITHRIL] Decision wait failed after failure', {
            error,
          });
        }
      })
      .finally(() => {
        this._failureDecisionInFlight = false;
      });
  }

  _waitForInitialDecision(currentGeneration: number): void {
    if (this._decisionInFlight) return;
    this._decisionInFlight = true;
    this._controller
      .waitForBootstrapDecision()
      .then(async (decision) => {
        const deps = this._requireDependencies();
        if (currentGeneration !== deps.getGeneration()) return;
        this._decision = decision;
        this._decisionPrompted = decision !== 'accept';
        if (decision === 'decline') {
          await this._handleBootstrapDecline(
            'waited bootstrap decline',
            currentGeneration
          );
        }
      })
      .catch((error) => {
        if (!isMithrilDecisionCancelledError(error)) {
          logger.error('[MITHRIL] Decision wait failed', { error });
        }
      })
      .finally(() => {
        this._decisionInFlight = false;
      });
  }

  async _handleBootstrapDecline(
    source: string,
    currentGeneration: number
  ): Promise<boolean> {
    const deps = this._requireDependencies();
    await this._emitIdleStatus();
    if (currentGeneration !== deps.getGeneration()) return false;
    await chainStorageCoordinator.wipeChainAndSnapshots(
      'User declined Mithril bootstrap. Wiped chain directory and Mithril snapshots.',
      deps.cardanoNode.state
    );
    if (currentGeneration !== deps.getGeneration()) return false;
    logger.info(`[MITHRIL] Starting cardano-node after ${source}`);
    await deps.cardanoNode.start();
    return true;
  }

  _requireDependencies(): MithrilStartupGateDependencies {
    if (!this._dependencies) {
      throw new Error('Mithril startup gate dependencies are not configured.');
    }
    return this._dependencies;
  }

  _getGeneration(): number {
    return this._dependencies?.getGeneration() ?? 0;
  }

  _transition(nextState: MithrilStartupGateState, event: string): void {
    if (this._state === nextState) return;
    logger.info('[MITHRIL] Startup gate transition', {
      event,
      from: this._state,
      to: nextState,
    });
    this._state = nextState;
  }

  _emitDecisionStatus(): Promise<void> {
    return this._controller.broadcastBootstrapStatus({
      status: 'decision',
      snapshot: null,
      error: null,
    });
  }

  _emitIdleStatus(): Promise<void> {
    return this._controller.broadcastBootstrapStatus({
      status: 'idle',
      snapshot: null,
      error: null,
    });
  }

  _emitStartFailure(error: unknown): Promise<void> {
    const message =
      error instanceof Error
        ? error.message
        : 'Cardano node failed to start after Mithril bootstrap. Wipe the chain data and try again.';
    return this._controller.broadcastBootstrapStatus({
      status: 'failed',
      snapshot: null,
      error: { message, stage: 'node-start' },
    });
  }

  _emitStartingNodeStatus(): Promise<void> {
    return this._controller.broadcastBootstrapStatus({
      ...this._controller.getBootstrapStatus(),
      status: 'starting-node',
      error: null,
    });
  }

  _waitForNodeStartupProof(): Promise<void> {
    return new Promise((resolve) => {
      setTimeout(resolve, MITHRIL_COMPLETION_DELAY_MS);
    });
  }

  _markHadNotEnoughSpaceLeft<TResponse>(response: TResponse): void {
    if (response && typeof response === 'object') {
      (response as { hadNotEnoughSpaceLeft?: boolean }).hadNotEnoughSpaceLeft =
        false;
    }
  }
}
