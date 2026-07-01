import path from 'path';
import fs from 'fs-extra';
import type { ChildProcess } from 'child_process';
import EventEmitter from 'events';
import type { WriteStream } from 'fs';
import checkDiskSpace from 'check-disk-space';
import {
  launcherConfig,
  stateDirectoryPath,
  DISK_SPACE_REQUIRED,
} from '../config';
import { logger } from '../utils/logging';
import type { PartialSyncPreflightContext } from '../utils/chainStorageCoordinator';
import type {
  MithrilSnapshotItem,
  MithrilProgressItem,
} from '../../common/types/mithril-bootstrap.types';
import type {
  MithrilPartialSyncError,
  MithrilPartialSyncErrorStage,
  MithrilPartialSyncStatusSnapshot,
} from '../../common/types/mithril-partial-sync.types';
import type {
  RunCommandOptions,
  RunCommandResult,
} from './mithrilCommandRunner';
import { runCommand } from './mithrilCommandRunner';
import { parseMithrilProgressUpdate } from './mithrilProgress';
import { ChainStorageManager } from '../utils/chainStorageManager';
import {
  clearMithrilPartialSyncMarker,
  readMithrilPartialSyncMarker,
  writeMithrilPartialSyncMarker,
} from './mithrilPartialSyncMarker';
import { convertSnapshotDbToLsm } from './mithrilSnapshotConverter';
import {
  normalizeResolvedLatestSnapshot,
  normalizeSnapshotItem,
  parseMithrilJson,
  ResolvedLatestSnapshot,
  toTimestamp,
} from './mithrilSnapshotMetadata';
import {
  derivePartialSyncRange,
  PARTIAL_SYNC_STAGED_DB_INVALID_CODE,
  resolveLocalImmutableNumber,
} from './mithrilPartialSyncPreflight';
import {
  PARTIAL_SYNC_ALLOWED_INSTALL_ENTRIES,
  PartialSyncStagingPaths,
  preparePartialSyncStagingDirectory,
  validateConvertedStagedOutput,
  validateStagedDownloadOutput,
} from './mithrilPartialSyncStaging';
import { MithrilOutputLineBuffer } from './mithrilOutputLineBuffer';
import {
  addProgressItem,
  markActiveProgressItemAs,
  upsertProgressItem,
} from './mithrilProgressItems';

const PARTIAL_SYNC_LOG_FILE_NAME = 'mithril-partial-sync.log';
const PARTIAL_SYNC_STAGING_DIRECTORY_NAME = 'mithril-partial-sync';
const PARTIAL_SYNC_LATEST_DRIFT_CODE = 'PARTIAL_SYNC_LATEST_DRIFT';
const PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE =
  'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE';
// Scratch-space requirement. snapshot.size is the FULL certified DB (a conservative over-estimate of the
// partial range); 1.2× covers LSM-conversion + FS slack. Floored at DISK_SPACE_REQUIRED so a missing/zero
// size still fails closed on a near-full disk. Renderer copy for the code lives in task-ux-403.
const PARTIAL_SYNC_DISK_SAFETY_FACTOR = 1.2;

// Behind-ness threshold in IMMUTABLE FILES. Backend-owned; overridable via launcher config.
// Conservative starting point (≈ 1 epoch-equivalent of immutable files per PRD D2); calibrate down
// during QA so the prompt actually fires on a node that is days behind.
const DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES = 20; // ≈ 1 epoch (10·k = 21,600 slots ≈ 6h/file ⇒ ~20 files/5-day epoch); QA-calibratable
const PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS = 5 * 60 * 1000; // 5 min; only the aggregator query is cached

const DEFAULT_STATUS: MithrilPartialSyncStatusSnapshot = {
  status: 'idle',
  allowedRecoveryActions: [],
  transferProgress: {},
  progressItems: [],
  error: null,
};

class MithrilPartialSyncStageError extends Error {
  stage: MithrilPartialSyncErrorStage;
  code?: string;

  constructor(
    message: string,
    stage: MithrilPartialSyncErrorStage,
    code?: string
  ) {
    super(message);
    this.name = 'MithrilPartialSyncStageError';
    this.stage = stage;
    this.code = code;
  }
}

export class MithrilPartialSyncService {
  _status: MithrilPartialSyncStatusSnapshot = { ...DEFAULT_STATUS };
  _statusEmitter = new EventEmitter();
  _currentProcess: ChildProcess | null = null;
  _logStream: WriteStream | null = null;
  _activeWorkDir: string | null = null;
  _stagingChainDir: string | null = null;
  _startedAt: number | null = null;
  _isCancelled = false;
  _cancelFallbackErrorStage: MithrilPartialSyncErrorStage | null = null;
  _progressItems: MithrilProgressItem[] = [];
  _latestSnapshot: MithrilSnapshotItem | null = null;
  _stagedDbPath: string | null = null;
  _chainStorageManager: ChainStorageManager;
  _latestCertifiedImmutableCache: {
    value: number;
    epoch: number | null;
    fetchedAt: number;
  } | null = null;
  // #15 (D-702b-9): dedupe the per-probe local immutable read (getManagedChainPath → fork
  // checkDiskSpace via getConfig, plus the immutable/ readdir) under the SAME 5-min TTL as the
  // aggregator cache. Invalidated on Mithril lifecycle transitions via _invalidateBehindnessCaches().
  _localImmutableCache: {
    value: number;
    fetchedAt: number;
  } | null = null;

  constructor(
    chainStorageManager: ChainStorageManager = new ChainStorageManager()
  ) {
    this._chainStorageManager = chainStorageManager;
  }

  get status(): MithrilPartialSyncStatusSnapshot {
    return { ...this._status };
  }

  onStatus(
    listener: (update: MithrilPartialSyncStatusSnapshot) => void
  ): () => void {
    this._statusEmitter.on('status', listener);
    return () => this._statusEmitter.removeListener('status', listener);
  }

  async start(context: PartialSyncPreflightContext): Promise<void> {
    if (this._activeWorkDir) {
      throw new Error('Mithril partial sync is already in progress.');
    }

    // #15 (D-702b-9): the local immutable position is about to change — drop the behind-ness
    // input caches so the next probe re-resolves fresh inputs without waiting out the TTL.
    this._invalidateBehindnessCaches();

    this._activeWorkDir = context.mithrilWorkDir;
    this._stagingChainDir = context.mithrilWorkDir;
    this._startedAt = Date.now();
    this._isCancelled = false;
    this._cancelFallbackErrorStage = null;
    this._progressItems = [];
    this._latestSnapshot = null;
    this._stagedDbPath = null;

    try {
      this._addProgressItem('preparing', 'preparing', 'active');
      this._updateStatus({
        status: 'preparing',
        allowedRecoveryActions: [],
        transferProgress: {},
        error: null,
        logPath: this._getLogPath(),
        progressItems: [...this._progressItems],
      });

      const latestSnapshot = await this.resolveLatestSnapshotMetadata();
      this._latestSnapshot = latestSnapshot.snapshot;

      const localImmutableNumber = await resolveLocalImmutableNumber(
        context.layoutResult.managedChainPath,
        (stage, message, code) => this._createStageError(stage, message, code)
      );
      const partialSyncRange = derivePartialSyncRange(
        localImmutableNumber,
        latestSnapshot.latestCertifiedImmutableNumber,
        (stage, message, code) => this._createStageError(stage, message, code)
      );
      const stagingPaths = await this._prepareStagingDirectory(
        context.layoutResult.managedChainPath
      );

      await this._assertSufficientDiskSpace(stagingPaths.rootPath); // D7/BUG3 preflight

      this._activeWorkDir = stagingPaths.downloadParentPath;
      this._stagedDbPath = stagingPaths.dbPath;

      logger.info(
        'MithrilPartialSyncService: prepared partial sync preflight',
        {
          managedChainPath: context.layoutResult.managedChainPath,
          stagingRootPath: stagingPaths.rootPath,
          latestSnapshotDigest: latestSnapshot.snapshot.digest,
          latestCertifiedImmutableNumber:
            latestSnapshot.latestCertifiedImmutableNumber,
          localImmutableNumber,
          partialSyncRange,
        }
      );

      const latestSnapshotAtDownload = await this.resolveLatestSnapshotMetadata();
      if (
        latestSnapshotAtDownload.latestCertifiedImmutableNumber !==
        partialSyncRange.end
      ) {
        throw this._createStageError(
          'preparing',
          'The latest certified Mithril snapshot changed during partial sync preparation. Please retry with the refreshed range.',
          PARTIAL_SYNC_LATEST_DRIFT_CODE
        );
      }

      this._latestSnapshot = latestSnapshotAtDownload.snapshot;

      await this._downloadAndVerifyPartialSnapshot(
        stagingPaths,
        partialSyncRange
      );
      await validateStagedDownloadOutput(stagingPaths, (stage, message, code) =>
        this._createStageError(stage, message, code)
      );
      this._markActiveProgressItemAs('completed');
      this._updateStatus({
        status: 'verifying',
        progressItems: [...this._progressItems],
      });

      this._activatePostVerificationStage('converting');
      await this._convertStagedSnapshot(stagingPaths.dbPath);
      await validateConvertedStagedOutput(
        stagingPaths.dbPath,
        (stage, message, code) => this._createStageError(stage, message, code)
      );

      this._activatePostVerificationStage('installing');
      await writeMithrilPartialSyncMarker('cutover-in-progress', {
        managedChainPath: context.layoutResult.managedChainPath,
        stagingRootPath: this._getStagingRootPath(),
      });
      await this._installValidatedStagedSnapshot(stagingPaths.dbPath);
      await writeMithrilPartialSyncMarker('installed-awaiting-node-start', {
        managedChainPath: context.layoutResult.managedChainPath,
        stagingRootPath: this._getStagingRootPath(),
      });

      this._markActiveProgressItemAs('completed');
      this._activatePostVerificationStage('finalizing');
      this._updateStatus({
        status: 'finalizing',
        allowedRecoveryActions: ['wipe-and-full-sync'],
        progressItems: [...this._progressItems],
      });
    } catch (error) {
      if (this._isCancelled) {
        return;
      }

      const fallbackStage = this._getFallbackErrorStage();
      this._markActiveProgressItemAs('error');
      this._updateStatus({
        status: 'failed',
        allowedRecoveryActions: this._deriveAllowedRecoveryActions(error),
        error: this._buildError(error, fallbackStage),
        logPath: this._getLogPath(),
        progressItems: [...this._progressItems],
      });
      throw error;
    } finally {
      this._clearRuntimeWorkState();
    }
  }

  async cancel(): Promise<void> {
    // #15 (D-702b-9): invalidate behind-ness input caches at cancel entry so the next probe is fresh.
    this._invalidateBehindnessCaches();

    if (!this._activeWorkDir && !this._currentProcess) {
      logger.info(
        'MithrilPartialSyncService: ignoring cancel request with no active partial sync',
        null
      );
      // PRD D5(f) / gap #39: nothing to cancel in the node-stop window, but a cancel request MUST
      // always re-emit a status so the renderer never sticks on its optimistic stopping-node frame.
      // Re-emit the TRUE current status verbatim (do NOT fabricate `cancelled`); the existing push
      // pipeline (service.onStatus -> broadcastPartialSyncStatus -> _partialSyncStatusSender) delivers it.
      this._updateStatus({});
      return;
    }

    if (['installing', 'finalizing'].includes(this._status.status)) {
      throw new Error(
        'Mithril partial sync cancellation is no longer allowed after live chain cutover has started.'
      );
    }

    this._cancelFallbackErrorStage = this._getCurrentRecoveryStage();
    this._isCancelled = true;
    this._progressItems = [];
    this._addProgressItem('cleanup', 'cleanup', 'active');
    this._updateStatus({
      status: 'cancelling',
      allowedRecoveryActions: [],
      error: null,
      logPath: this._getLogPath(),
      progressItems: [...this._progressItems],
      transferProgress: {},
    });

    try {
      if (this._currentProcess) {
        this._currentProcess.kill();
      }
    } catch (error) {
      logger.warn('MithrilPartialSyncService: failed to kill process', {
        error,
      });
    }
  }

  async finalizeCancel(): Promise<void> {
    if (this._status.status !== 'cancelling') {
      return;
    }

    try {
      await this._cleanupPartialSyncArtifacts();
      this._progressItems = [];
      this._updateStatus({
        status: 'cancelled',
        allowedRecoveryActions: ['retry', 'restart-normal'],
        error: null,
        logPath: this._getLogPath(),
        progressItems: [],
        transferProgress: {},
      });
    } catch (error) {
      this._markActiveProgressItemAs('error');
      this._updateStatus({
        status: 'failed',
        allowedRecoveryActions: ['retry', 'restart-normal'],
        error: this._buildError(
          error,
          this._cancelFallbackErrorStage ?? this._getCurrentRecoveryStage()
        ),
        logPath: this._getLogPath(),
        progressItems: [...this._progressItems],
        transferProgress: {},
      });
    } finally {
      this._clearRuntimeWorkState();
    }
  }

  forceKill(): void {
    try {
      this._currentProcess?.kill('SIGKILL');
    } catch (error) {
      logger.warn('MithrilPartialSyncService: failed to force kill process', {
        error,
      });
    }
  }

  async abandonCancel(): Promise<void> {
    if (this._status.status !== 'cancelling') {
      return;
    }

    this._markActiveProgressItemAs('error');
    this._updateStatus({
      status: 'failed',
      allowedRecoveryActions: [],
      error: {
        message:
          "Daedalus couldn't finish cleaning up Mithril Sync. Restart Daedalus to continue safely.",
        stage: this._cancelFallbackErrorStage ?? 'preparing',
        logPath: this._getLogPath(),
      },
      logPath: this._getLogPath(),
      progressItems: [...this._progressItems],
      transferProgress: {},
    });
  }

  async restartNormal(): Promise<void> {
    this._assertRecoveryActionAllowed('restart-normal');

    try {
      await this._cleanupPartialSyncArtifacts();
      this._resetToIdleStatus();
    } finally {
      this._clearRuntimeWorkState();
    }
  }

  async wipeAndFullSync(): Promise<void> {
    this._assertRecoveryActionAllowed('wipe-and-full-sync');

    try {
      await fs.remove(this._getStagingRootPath());
      this._resetToIdleStatus();
    } finally {
      this._clearRuntimeWorkState();
    }
  }

  async finalizeWipeAndFullSync(): Promise<void> {
    await clearMithrilPartialSyncMarker();
    this._resetToIdleStatus();
    this._clearRuntimeWorkState();
  }

  async finalizeCompletedPartialSync(): Promise<void> {
    // PRD D9 step 4 (dismiss-driven). Idempotent: safe even when already idle (gap #41).
    // Resolve the staging root from the durable marker first so it is correct cross-session;
    // fall back to the in-session resolver if the marker carries no path.
    const marker = await readMithrilPartialSyncMarker();
    const stagingRoot = marker?.stagingRootPath ?? this._getStagingRootPath();

    this._resetToIdleStatus();
    await fs.remove(stagingRoot);
    await clearMithrilPartialSyncMarker();
    this._clearRuntimeWorkState();
  }

  assertStartAllowed(): void {
    if (this._status.status === 'failed') {
      if (!this._status.allowedRecoveryActions.includes('retry')) {
        throw new Error(
          'Mithril partial sync cannot retry from the current recovery boundary.'
        );
      }
    }

    if (this._status.status === 'cancelled') {
      return;
    }

    if (!['idle', 'failed'].includes(this._status.status)) {
      throw new Error(
        'Mithril partial sync cannot start from the current state.'
      );
    }
  }

  async _downloadAndVerifyPartialSnapshot(
    stagingPaths: PartialSyncStagingPaths,
    partialSyncRange: { start: number; end: number }
  ): Promise<void> {
    this._activateProgressStage('downloading');
    this._updateStatus({
      status: 'downloading',
      allowedRecoveryActions: [],
      error: null,
      logPath: this._getLogPath(),
      progressItems: [...this._progressItems],
    });

    const downloadArgs = [
      '--json',
      'cardano-db',
      'download',
      'latest',
      '--download-dir',
      stagingPaths.downloadParentPath,
      '--start',
      String(partialSyncRange.start),
      '--end',
      String(partialSyncRange.end),
      '--include-ancillary',
      '--allow-override',
    ];

    const applyProgressUpdate = (line: string) => {
      if (this._isCancelled) return;

      const update = parseMithrilProgressUpdate(line);
      if (!update) return;

      const nextStage = this._resolveStageFromProgressUpdate(update);
      if (nextStage === 'verifying') {
        this._activateProgressStage('verifying');
      }

      if (nextStage === 'downloading' && this._status.status === 'preparing') {
        this._activateProgressStage('downloading');
      }

      const status =
        nextStage === 'downloading' && this._status.status === 'verifying'
          ? 'verifying'
          : nextStage ?? this._status.status;

      this._updateStatus({
        status,
        progressItems: [...this._progressItems],
        transferProgress: {
          ...this._status.transferProgress,
          ...(update.filesDownloaded != null
            ? { filesDownloaded: update.filesDownloaded }
            : {}),
          ...(update.filesTotal != null
            ? { filesTotal: update.filesTotal }
            : {}),
          ...(update.bytesDownloaded != null
            ? { ancillaryBytesDownloaded: update.bytesDownloaded }
            : {}),
          ...(update.bytesTotal != null
            ? { ancillaryBytesTotal: update.bytesTotal }
            : {}),
          ...(update.elapsedSeconds != null
            ? { elapsedSeconds: update.elapsedSeconds }
            : {}),
        },
      });
    };
    const stdoutLines = new MithrilOutputLineBuffer(applyProgressUpdate);
    const stderrLines = new MithrilOutputLineBuffer(applyProgressUpdate);

    const { exitCode, stderr } = await this._runCommand(
      downloadArgs,
      {
        onStdout: (chunk) => stdoutLines.push(chunk),
        onStderr: (chunk) => stderrLines.push(chunk),
      },
      stagingPaths.downloadParentPath
    );

    stdoutLines.flush();
    stderrLines.flush();

    if (exitCode !== 0) {
      throw this._createStageError(
        this._status.status === 'verifying' ? 'verifying' : 'downloading',
        `Mithril partial sync ${
          this._status.status === 'verifying' ? 'verification' : 'download'
        } failed with exit code ${exitCode}`,
        'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED'
      );
    }
  }

  async _convertStagedSnapshot(dbPath: string): Promise<void> {
    const configPath = launcherConfig.nodeConfig.network.configFile;

    await convertSnapshotDbToLsm({
      dbDirectory: dbPath,
      configPath,
      runBinary: (binaryName, args) => this._runBinary(binaryName, args),
      createFailure: (message) =>
        this._createStageError(
          'converting',
          message,
          'PARTIAL_SYNC_CONVERSION_FAILED'
        ),
      onCommandPrepared: (_args, context) => {
        logger.info(
          'MithrilPartialSyncService: prepared LSM conversion command',
          context
        );
      },
    });
  }

  async _installValidatedStagedSnapshot(dbPath: string): Promise<void> {
    await this._chainStorageManager.installValidatedPartialSyncSnapshot(
      dbPath,
      {
        expectedTopLevelEntries: PARTIAL_SYNC_ALLOWED_INSTALL_ENTRIES,
      }
    );
  }

  async _runBinary(
    binaryName: string,
    args: Array<string>
  ): Promise<RunCommandResult> {
    const { runBinary } = await import('./mithrilCommandRunner');
    return runBinary(
      binaryName,
      args,
      this._activeWorkDir || stateDirectoryPath,
      {
        logFileName: PARTIAL_SYNC_LOG_FILE_NAME,
      }
    );
  }

  _activatePostVerificationStage(
    stage: 'converting' | 'installing' | 'finalizing'
  ): void {
    this._markActiveProgressItemAs('completed');
    this._upsertProgressItem(stage, stage, 'active');
    this._updateStatus({
      status: stage,
      progressItems: [...this._progressItems],
      error: null,
      allowedRecoveryActions: [],
    });
  }

  _deriveAllowedRecoveryActions(
    error: unknown
  ): MithrilPartialSyncStatusSnapshot['allowedRecoveryActions'] {
    const stage =
      error instanceof MithrilPartialSyncStageError
        ? error.stage
        : this._getFallbackErrorStage();

    if (['installing', 'finalizing', 'starting-node'].includes(stage)) {
      return ['wipe-and-full-sync'];
    }

    return ['retry', 'restart-normal', 'wipe-and-full-sync'];
  }

  _assertRecoveryActionAllowed(
    action: 'restart-normal' | 'wipe-and-full-sync'
  ): void {
    if (this._activeWorkDir || this._currentProcess) {
      throw new Error(
        `Cannot ${action} while Mithril partial sync is still in progress.`
      );
    }

    if (!this._status.allowedRecoveryActions.includes(action)) {
      throw new Error(
        `Mithril partial sync cannot ${action} from the current recovery boundary.`
      );
    }
  }

  async _cleanupPartialSyncArtifacts(): Promise<void> {
    await fs.remove(this._getStagingRootPath());
    await clearMithrilPartialSyncMarker();
  }

  _resetToIdleStatus(): void {
    // #15 (D-702b-9): covers restart-normal/wipe/finalize-wipe/finalize-completed — after a cutover
    // the next probe must re-resolve both behind-ness inputs so the prompt/Diagnostics flip promptly.
    this._invalidateBehindnessCaches();
    this._progressItems = [];
    this._latestSnapshot = null;
    this._cancelFallbackErrorStage = null;
    this._updateStatus({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      error: null,
      logPath: undefined,
      progressItems: [],
    });
  }

  _clearRuntimeWorkState(): void {
    this._activeWorkDir = null;
    this._currentProcess = null;
    this._logStream = null;
    this._stagedDbPath = null;
    this._startedAt = null;
    this._cancelFallbackErrorStage = null;
  }

  _getCurrentRecoveryStage(): MithrilPartialSyncErrorStage {
    return this._status.error?.stage ?? this._getFallbackErrorStage();
  }

  _getStagingRootPath(): string {
    // Colocate staging as a SIBLING of the resolved managed chain (backend PRD:250 / D7/BUG3): same volume
    // as the chain so cutover is an intra-volume rename, never inside the live chain subtree (the
    // isPathWithin guard in preparePartialSyncStagingDirectory still enforces that). _stagingChainDir is the
    // realpath-resolved chain dir captured at start(); the stateDirectoryPath fallback preserves the legacy
    // default location when no sync has run this session (cross-session reclaim is task-ux-202).
    const stagingParent = this._stagingChainDir
      ? path.dirname(this._stagingChainDir)
      : stateDirectoryPath;
    return path.join(stagingParent, PARTIAL_SYNC_STAGING_DIRECTORY_NAME);
  }

  async resolveLatestSnapshotMetadata(): Promise<ResolvedLatestSnapshot> {
    try {
      const latestSnapshot = await this._showSnapshotRaw('latest');
      if (latestSnapshot) {
        return latestSnapshot;
      }
    } catch (error) {
      logger.warn(
        'MithrilPartialSyncService: latest snapshot show lookup failed, falling back to list',
        { error }
      );
    }

    const snapshots = await this._listSnapshotsRaw();
    const latestSnapshot = snapshots
      .filter(({ snapshot }) => Boolean(snapshot.digest))
      .sort(
        (left, right) =>
          toTimestamp(right.snapshot.createdAt) -
          toTimestamp(left.snapshot.createdAt)
      )[0];

    if (!latestSnapshot) {
      throw this._createStageError(
        'preparing',
        'Unable to resolve the latest Mithril snapshot metadata.'
      );
    }

    return latestSnapshot;
  }

  async _getCachedLatestCertifiedImmutableNumber(): Promise<number> {
    const now = Date.now();
    if (
      this._latestCertifiedImmutableCache &&
      now - this._latestCertifiedImmutableCache.fetchedAt <
        PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS
    ) {
      return this._latestCertifiedImmutableCache.value;
    }
    const snapshot = await this.resolveLatestSnapshotMetadata();
    this._latestCertifiedImmutableCache = {
      value: snapshot.latestCertifiedImmutableNumber,
      epoch: snapshot.certifiedEpoch, // #16 (D-702b-10): carry the certified epoch from the same beacon
      fetchedAt: now,
    };
    return snapshot.latestCertifiedImmutableNumber;
  }

  // #16 (D-702b-10): sibling getter for the certified epoch carried on the aggregator cache. Returns
  // `null` when the cache is unpopulated or the beacon had no epoch. Does NOT change the immutable
  // getter's `number` return; read it AFTER _getCachedLatestCertifiedImmutableNumber populates the cache.
  _getCachedCertifiedEpoch(): number | null {
    return this._latestCertifiedImmutableCache?.epoch ?? null;
  }

  // #15 (D-702b-9): cache the local immutable read under the same 5-min TTL as the aggregator. On a
  // hit we skip BOTH getManagedChainPath (which transitively forks checkDiskSpace via getConfig) AND
  // resolveLocalImmutableNumber's immutable/ readdir — the per-probe CPU cost the 30s poll paid every tick.
  async _getCachedLocalImmutableNumber(): Promise<number> {
    const now = Date.now();
    if (
      this._localImmutableCache &&
      now - this._localImmutableCache.fetchedAt <
        PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS
    ) {
      return this._localImmutableCache.value;
    }
    const managedChainPath = await this._chainStorageManager.getManagedChainPath();
    const localImmutableNumber = await resolveLocalImmutableNumber(
      managedChainPath,
      (stage, message, code) => this._createStageError(stage, message, code)
    );
    this._localImmutableCache = {
      value: localImmutableNumber,
      fetchedAt: now,
    };
    return localImmutableNumber;
  }

  // #15 (D-702b-9): drop both behind-ness input caches so the next probe re-resolves fresh. Called at
  // start()/cancel() entry and inside _resetToIdleStatus() (restart-normal/wipe/finalize paths).
  _invalidateBehindnessCaches(): void {
    this._latestCertifiedImmutableCache = null;
    this._localImmutableCache = null;
  }

  _getBehindnessThresholdImmutables(): number {
    const configured = launcherConfig.mithrilPartialSyncThresholdImmutables;
    return typeof configured === 'number' && configured > 0
      ? configured
      : DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES;
  }

  async getPartialSyncBehindness(): Promise<{
    isSignificantlyBehind: boolean;
    behindByImmutables?: number;
    certifiedEpoch?: number | null;
  }> {
    try {
      const latest = await this._getCachedLatestCertifiedImmutableNumber();
      // #15 (D-702b-9): cached local read dedupes the checkDiskSpace fork + immutable/ readdir.
      const localImmutableNumber = await this._getCachedLocalImmutableNumber();
      // #16 (D-702b-10): same beacon → consistent epoch; null/undefined ⇒ omitted below.
      const certifiedEpoch = this._getCachedCertifiedEpoch();
      const gap = latest - localImmutableNumber;
      if (gap <= 0) {
        // local >= latest ⇒ no certified range to restore ⇒ never nudge (PRD D2 truthfulness)
        return {
          isSignificantlyBehind: false,
          ...(certifiedEpoch != null ? { certifiedEpoch } : {}),
        };
      }
      return {
        isSignificantlyBehind: gap >= this._getBehindnessThresholdImmutables(),
        behindByImmutables: gap,
        ...(certifiedEpoch != null ? { certifiedEpoch } : {}),
      };
    } catch (error) {
      // Aggregator unreachable, no immutable dir yet, or any failure ⇒ degrade to not-behind.
      logger.warn(
        'MithrilPartialSyncService: behind-ness probe failed; treating as not significantly behind',
        { error }
      );
      return { isSignificantlyBehind: false };
    }
  }

  async listSnapshots(): Promise<Array<MithrilSnapshotItem>> {
    const { stdout } = await this._runCommand(
      ['cardano-db', 'snapshot', 'list', '--json'],
      { requireKeys: false }
    );
    const parsed = this._safeJsonParse(stdout);
    if (!Array.isArray(parsed)) return [];
    return parsed.map((item) => normalizeSnapshotItem(item));
  }

  async showSnapshot(digest: string): Promise<MithrilSnapshotItem | null> {
    if (!digest || !digest.trim()) return null;
    const { stdout } = await this._runCommand(
      ['cardano-db', 'snapshot', 'show', digest, '--json'],
      { requireKeys: false }
    );
    const parsed = this._safeJsonParse(stdout);
    if (!parsed || typeof parsed !== 'object') return null;
    return normalizeSnapshotItem(parsed);
  }

  async _listSnapshotsRaw(): Promise<Array<ResolvedLatestSnapshot>> {
    const { stdout } = await this._runCommand(
      ['cardano-db', 'snapshot', 'list', '--json'],
      { requireKeys: false }
    );
    const parsed = this._safeJsonParse(stdout);
    if (!Array.isArray(parsed)) return [];

    return parsed.reduce<Array<ResolvedLatestSnapshot>>((snapshots, item) => {
      try {
        const normalizedSnapshot = this._normalizeResolvedLatestSnapshot(item);
        if (normalizedSnapshot) {
          snapshots.push(normalizedSnapshot);
        }
      } catch (error) {
        logger.warn(
          'MithrilPartialSyncService: ignoring unusable snapshot list item',
          { error }
        );
      }

      return snapshots;
    }, []);
  }

  async _showSnapshotRaw(
    digest: string
  ): Promise<ResolvedLatestSnapshot | null> {
    if (!digest || !digest.trim()) return null;

    const { stdout } = await this._runCommand(
      ['cardano-db', 'snapshot', 'show', digest, '--json'],
      { requireKeys: false }
    );
    const parsed = this._safeJsonParse(stdout);
    if (!parsed || typeof parsed !== 'object') return null;

    return this._normalizeResolvedLatestSnapshot(parsed);
  }

  _normalizeResolvedLatestSnapshot(
    raw: unknown
  ): ResolvedLatestSnapshot | null {
    const normalizedSnapshot = normalizeResolvedLatestSnapshot(raw);

    if (normalizedSnapshot == null) {
      throw this._createStageError(
        'preparing',
        'Unable to determine the latest certified immutable number from Mithril snapshot metadata.'
      );
    }

    return normalizedSnapshot;
  }

  async _prepareStagingDirectory(
    managedChainPath: string
  ): Promise<PartialSyncStagingPaths> {
    return preparePartialSyncStagingDirectory(
      this._getStagingRootPath(),
      managedChainPath,
      (stage, message, code) => this._createStageError(stage, message, code)
    );
  }

  async _assertSufficientDiskSpace(stagingRootPath: string): Promise<void> {
    const snapshotSize = this._latestSnapshot?.size ?? 0;
    const requiredBytes = Math.max(
      snapshotSize * PARTIAL_SYNC_DISK_SAFETY_FACTOR,
      DISK_SPACE_REQUIRED
    );

    let freeBytes: number;
    try {
      ({ free: freeBytes } = await checkDiskSpace(stagingRootPath));
    } catch (error) {
      // Could not measure — do NOT false-block (repo precedent: handleDiskSpace.ts:176-181 fail-open).
      logger.warn(
        'MithrilPartialSyncService: disk-space preflight could not measure free space; proceeding',
        { error, stagingRootPath }
      );
      return;
    }

    if (freeBytes < requiredBytes) {
      const requiredGb = Math.ceil(requiredBytes / 1073741824);
      const freeGb = Math.floor(freeBytes / 1073741824);
      throw this._createStageError(
        'preparing',
        `Not enough free disk space to stage the Mithril partial sync on the chain storage volume. ` +
          `Required ~${requiredGb} GB, available ~${freeGb} GB.`,
        PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE
      );
    }
  }

  _resolveStageFromProgressUpdate(
    update: ReturnType<typeof parseMithrilProgressUpdate>
  ): MithrilPartialSyncStatusSnapshot['status'] | null {
    if (!update) return null;
    if (update.stepNum != null && update.stepNum >= 4) {
      return 'verifying';
    }

    if (
      update.stepNum != null ||
      update.filesDownloaded != null ||
      update.filesTotal != null ||
      update.bytesDownloaded != null ||
      update.bytesTotal != null ||
      update.label != null
    ) {
      return 'downloading';
    }

    return null;
  }

  _updateStatus(update: Partial<MithrilPartialSyncStatusSnapshot>): void {
    const nextStatus = update.status ?? this._status.status;
    const normalizedUpdate =
      !('transferProgress' in update) && this._shouldTrackElapsed(nextStatus)
        ? {
            ...update,
            transferProgress: {
              ...this._status.transferProgress,
              elapsedSeconds: this._getElapsedSeconds(),
            },
          }
        : update;

    this._status = {
      ...this._status,
      ...normalizedUpdate,
      transferProgress:
        normalizedUpdate.transferProgress ??
        this._status.transferProgress ??
        {},
      progressItems:
        normalizedUpdate.progressItems ?? this._status.progressItems ?? [],
      allowedRecoveryActions:
        normalizedUpdate.allowedRecoveryActions ??
        this._status.allowedRecoveryActions ??
        [],
      error:
        normalizedUpdate.error === undefined
          ? this._status.error
          : normalizedUpdate.error,
    };
    this._statusEmitter.emit('status', { ...this._status });
  }

  _shouldTrackElapsed(
    status: MithrilPartialSyncStatusSnapshot['status']
  ): boolean {
    return !['idle', 'cancelled', 'failed', 'completed'].includes(status);
  }

  _getElapsedSeconds(): number | undefined {
    if (this._startedAt == null) return undefined;
    return Math.max(0, (Date.now() - this._startedAt) / 1000);
  }

  _markActiveProgressItemAs(state: 'completed' | 'error'): void {
    this._progressItems = markActiveProgressItemAs(this._progressItems, state);
  }

  _activateProgressStage(id: 'downloading' | 'verifying'): void {
    this._markActiveProgressItemAs('completed');
    this._upsertProgressItem(id, id, 'active');
  }

  _upsertProgressItem(
    id: string,
    label: string,
    state: MithrilProgressItem['state']
  ): void {
    this._progressItems = upsertProgressItem(
      this._progressItems,
      id,
      label,
      state
    );
  }

  _getFallbackErrorStage(): MithrilPartialSyncErrorStage {
    if (this._status.status === 'finalizing') {
      return 'finalizing';
    }

    if (this._status.status === 'installing') {
      return 'installing';
    }

    if (this._status.status === 'converting') {
      return 'converting';
    }

    if (this._status.status === 'verifying') {
      return 'verifying';
    }

    if (this._status.status === 'downloading') {
      return 'downloading';
    }

    return 'preparing';
  }

  _addProgressItem(
    id: string,
    label: string,
    state: MithrilProgressItem['state']
  ): void {
    this._progressItems = addProgressItem(
      this._progressItems,
      id,
      label,
      state
    );
  }

  _getLogPath(): string {
    return path.join(stateDirectoryPath, 'Logs', PARTIAL_SYNC_LOG_FILE_NAME);
  }

  _createStageError(
    stage: MithrilPartialSyncErrorStage,
    message: string,
    code?: string
  ): MithrilPartialSyncStageError {
    return new MithrilPartialSyncStageError(message, stage, code);
  }

  _buildError(
    error: unknown,
    fallbackStage?: MithrilPartialSyncErrorStage
  ): MithrilPartialSyncError {
    if (error instanceof MithrilPartialSyncStageError) {
      return {
        message: error.message,
        code: error.code,
        stage: error.stage,
        logPath: this._getLogPath(),
      };
    }

    if (error instanceof Error) {
      return {
        message: error.message,
        stage: fallbackStage,
        logPath: this._getLogPath(),
      };
    }

    return {
      message: 'Mithril partial sync failed',
      stage: fallbackStage,
      logPath: this._getLogPath(),
    };
  }

  async _runCommand(
    args: Array<string>,
    options: RunCommandOptions = {},
    workDir: string = this._activeWorkDir || stateDirectoryPath
  ): Promise<RunCommandResult> {
    return runCommand(
      args,
      workDir,
      {
        ...options,
        logFileName: PARTIAL_SYNC_LOG_FILE_NAME,
      },
      {
        onProcess: (child) => {
          this._currentProcess = child;
        },
        onLogStream: (logStream) => {
          this._logStream = logStream;
        },
      }
    );
  }

  _safeJsonParse(payload: string): any {
    return parseMithrilJson(payload, ({ error, payload: badPayload }) => {
      logger.warn('MithrilPartialSyncService: JSON parse failed', {
        error,
        payload: badPayload,
      });
    });
  }
}
