import path from 'path';
import fs from 'fs-extra';
import type { ChildProcess } from 'child_process';
import EventEmitter from 'events';
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
  MithrilPartialSyncErrorCode,
  MithrilPartialSyncErrorStage,
  MithrilPartialSyncStatus,
  MithrilPartialSyncStatusSnapshot,
} from '../../common/types/mithril-partial-sync.types';
import { makeIdlePartialSyncStatus } from '../../common/types/mithril-partial-sync.types';
import type {
  RunCommandCallbacks,
  RunCommandOptions,
  RunCommandResult,
} from './mithrilCommandRunner';
import { runBinary, runCommand } from './mithrilCommandRunner';
import { killProcessTree } from './killProcessTree';
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
import { MithrilPartialSyncStageError } from './mithrilErrors';
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
const PARTIAL_SYNC_ALREADY_RUNNING_CODE = 'PARTIAL_SYNC_ALREADY_RUNNING';
const PARTIAL_SYNC_START_NOT_ALLOWED_CODE = 'PARTIAL_SYNC_START_NOT_ALLOWED';
const PARTIAL_SYNC_CANCEL_NOT_ALLOWED_CODE = 'PARTIAL_SYNC_CANCEL_NOT_ALLOWED';
const PARTIAL_SYNC_RECOVERY_NOT_ALLOWED_CODE =
  'PARTIAL_SYNC_RECOVERY_NOT_ALLOWED';
const PARTIAL_SYNC_METADATA_UNAVAILABLE_CODE =
  'PARTIAL_SYNC_METADATA_UNAVAILABLE';
// Required space = missing bytes (snapshot − local chain, floored at 0) plus a margin sized on the full
//  snapshot — deliberately not the delta, as headroom for chain growth, conversion and FS slack — floored at
//  DISK_SPACE_REQUIRED so a zero/missing size still fails closed; unmeasurable local size ⇒ whole-snapshot bound.
const PARTIAL_SYNC_DISK_MARGIN_FACTOR = 0.2;
const PARTIAL_SYNC_DISK_SAFETY_FACTOR = 1 + PARTIAL_SYNC_DISK_MARGIN_FACTOR;

// Behind-ness threshold in immutable files; backend-owned, overridable via launcher config.
const DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES = 20; // ≈ 1 epoch: ~6h per immutable file ⇒ ~20 files per 5-day epoch; QA-calibratable
const PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS = 5 * 60 * 1000; // 5 min TTL for the behind-ness input caches

// Statuses that double as their own error stage; everything else (idle, the
// terminal statuses, stopping-node, cancelling, starting-node) falls back to
// 'preparing'.
const FALLBACK_ERROR_STAGES: ReadonlyArray<
  MithrilPartialSyncErrorStage & MithrilPartialSyncStatus
> = ['downloading', 'verifying', 'converting', 'installing', 'finalizing'];

// Per-call opt-in to register a child into the cancelable _currentProcess slot. Stripped before the
//  runner options, and deliberately not inferred from _activeWorkDir — the behind-ness probe runs with
//  _activeWorkDir set, so inferring would clobber the active download's slot.
type PartialSyncRunCommandOptions = RunCommandOptions & {
  trackAsCancelable?: boolean;
};

// Options threaded through the metadata-read helpers, which call _runCommand without the workDir positional.
type PartialSyncMetadataReadOptions = Pick<
  PartialSyncRunCommandOptions,
  'trackAsCancelable'
>;

// Thrown by _throwIfCancelled() to unwind start()'s stage machine on cancel. Caught by start()'s
//  'if (this._isCancelled) return', so a cancel never emits 'failed' and settles into finalizeCancel().
class MithrilPartialSyncCancelledError extends Error {
  constructor() {
    super('Mithril partial sync was cancelled.');
    this.name = 'MithrilPartialSyncCancelledError';
  }
}

export class MithrilPartialSyncService {
  _status: MithrilPartialSyncStatusSnapshot = makeIdlePartialSyncStatus();
  _statusEmitter = new EventEmitter();
  _currentProcess: ChildProcess | null = null;
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
  // Dedupe the per-probe local immutable read (managed-chain path resolution
  // plus the immutable/ readdir) under the same 5-min TTL as the aggregator
  // cache. Invalidated on Mithril lifecycle transitions via _invalidateBehindnessCaches().
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
      logger.warn(
        'MithrilPartialSyncService: rejecting start; a partial sync run is already in progress',
        { status: this._status.status }
      );
      throw new Error(PARTIAL_SYNC_ALREADY_RUNNING_CODE);
    }

    // The local immutable position is about to change — drop the behind-ness
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

      // Tracked as cancelable so a cancel during 'preparing' can still kill an in-flight metadata child.
      const latestSnapshot = await this.resolveLatestSnapshotMetadata({
        trackAsCancelable: true,
      });
      this._latestSnapshot = latestSnapshot.snapshot;

      const localImmutableNumber = await resolveLocalImmutableNumber(
        context.layoutResult.managedChainPath
      );
      const partialSyncRange = derivePartialSyncRange(
        localImmutableNumber,
        latestSnapshot.latestCertifiedImmutableNumber
      );
      const stagingPaths = await this._prepareStagingDirectory(
        context.layoutResult.managedChainPath
      );

      await this._assertSufficientDiskSpace(
        stagingPaths.rootPath,
        context.layoutResult.managedChainPath
      );

      // Cancel here unwinds before the latest-drift re-resolve and the download spawn, while status is cancelling.
      this._throwIfCancelled();

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

      // The latest-drift re-resolve is also part of the cancelable run.
      const latestSnapshotAtDownload = await this.resolveLatestSnapshotMetadata(
        { trackAsCancelable: true }
      );
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
      await validateStagedDownloadOutput(stagingPaths);

      // Cancel here unwinds before the 'verifying' emit and the 'converting' stage.
      this._throwIfCancelled();

      this._markActiveProgressItemAs('completed');
      this._updateStatus({
        status: 'verifying',
        progressItems: [...this._progressItems],
      });

      this._activatePostVerificationStage('converting');
      await this._convertStagedSnapshot(stagingPaths.dbPath);
      await validateConvertedStagedOutput(stagingPaths.dbPath);

      // Last cancel checkpoint before cutover: a cancel during 'converting' must never reach live-chain install.
      //  Defense-in-depth for the race where the conversion completes just as the kill lands.
      this._throwIfCancelled();

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
    // Invalidate behind-ness input caches at cancel entry so the next probe is fresh.
    this._invalidateBehindnessCaches();

    if (!this._activeWorkDir && !this._currentProcess) {
      logger.info(
        'MithrilPartialSyncService: ignoring cancel request with no active partial sync',
        null
      );
      // No active run to cancel, but re-emit the current status so the renderer doesn't stick on its optimistic
      //  stopping-node frame — re-emit the real status verbatim rather than fabricating 'cancelled'.
      this._updateStatus({});
      return;
    }

    if (['installing', 'finalizing'].includes(this._status.status)) {
      logger.warn(
        'MithrilPartialSyncService: rejecting cancel; live chain cutover has already started',
        { status: this._status.status }
      );
      throw new Error(PARTIAL_SYNC_CANCEL_NOT_ALLOWED_CODE);
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

    // Logs even when no child process is tracked (hadChild: false), recording the
    // slot state at cancel.
    logger.info('MithrilPartialSyncService: cancel entry', {
      status: this._status.status,
      pid: this._currentProcess?.pid ?? null,
      hadChild: this._currentProcess != null,
    });

    try {
      if (this._currentProcess) {
        killProcessTree(this._currentProcess, 'SIGTERM');
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

    logger.info('MithrilPartialSyncService: finalizing cancel', {
      status: this._status.status,
    });

    try {
      await this._cleanupPartialSyncArtifacts();
      logger.info(
        'MithrilPartialSyncService: cancel finalized; partial sync artifacts cleaned up',
        null
      );
    } catch (error) {
      // Best-effort: a locked staging dir must not strand the user on a failed screen. No cutover has
      //  happened, so nothing installed is at risk; the next start's staging prep reclaims the orphan.
      logger.warn(
        'MithrilPartialSyncService: finalizeCancel cleanup failed; landing on cancelled and leaving staging for the next start to reclaim',
        {
          error,
          cancelFallbackErrorStage: this._cancelFallbackErrorStage,
        }
      );
    }

    this._progressItems = [];
    this._updateStatus({
      status: 'cancelled',
      allowedRecoveryActions: ['retry', 'restart-normal'],
      error: null,
      logPath: this._getLogPath(),
      progressItems: [],
      transferProgress: {},
    });
    this._clearRuntimeWorkState();
  }

  forceKill(): void {
    try {
      // Group/tree SIGKILL (killProcessTree no-ops on an empty slot).
      killProcessTree(this._currentProcess, 'SIGKILL');
    } catch (error) {
      logger.warn('MithrilPartialSyncService: failed to force kill process', {
        error,
      });
    }
  }

  // SIGKILLs the tracked download/conversion child on quit — cancel()/forceKill() don't run on a plain
  //  quit, so a detached POSIX child would survive process.exit(). sync: true because safeExit calls
  //  process.exit() inside a stream-end callback, where an async Windows taskkill would never launch.
  forceKillForShutdown(): void {
    try {
      killProcessTree(this._currentProcess, 'SIGKILL', { sync: true });
    } catch (error) {
      logger.warn(
        'MithrilPartialSyncService: failed to force kill process on shutdown',
        {
          error,
        }
      );
    }
  }

  async abandonCancel(): Promise<void> {
    if (this._status.status !== 'cancelling') {
      return;
    }

    logger.warn(
      'MithrilPartialSyncService: abandoning cancel; cleanup could not be completed — user must restart Daedalus',
      {
        cancelFallbackErrorStage: this._cancelFallbackErrorStage ?? 'preparing',
      }
    );

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

  // Recovery actions may target a boundary startup emitted in a previous session; this service starts
  //  each session idle, so its allowed-action assertion would reject them. Adopt the controller's broadcast
  //  snapshot as the local boundary — only from idle, and only when it is a real recovery boundary.
  adoptRecoverySnapshot(snapshot: MithrilPartialSyncStatusSnapshot): void {
    if (this._status.status !== 'idle') {
      return;
    }
    if (
      snapshot.status === 'idle' ||
      snapshot.allowedRecoveryActions.length === 0
    ) {
      return;
    }
    this._status = { ...snapshot };
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
      // Marker-first staging resolution (see _cleanupPartialSyncArtifacts); the
      // marker itself is retained here until finalizeWipeAndFullSync clears it.
      const marker = await readMithrilPartialSyncMarker();
      await fs.remove(marker?.stagingRootPath ?? this._getStagingRootPath());
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
    // Dismiss-driven. Idempotent: safe even when already idle.
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
        logger.warn(
          'MithrilPartialSyncService: rejecting start; retry is not an allowed recovery action',
          { allowedRecoveryActions: this._status.allowedRecoveryActions }
        );
        throw new Error(PARTIAL_SYNC_START_NOT_ALLOWED_CODE);
      }
    }

    if (this._status.status === 'cancelled') {
      return;
    }

    if (!['idle', 'failed'].includes(this._status.status)) {
      logger.warn(
        'MithrilPartialSyncService: rejecting start; a run cannot start from the current status',
        { status: this._status.status }
      );
      throw new Error(PARTIAL_SYNC_START_NOT_ALLOWED_CODE);
    }
  }

  async _downloadAndVerifyPartialSnapshot(
    stagingPaths: PartialSyncStagingPaths,
    partialSyncRange: { start: number; end: number }
  ): Promise<void> {
    // Cancel here (before the 'downloading' re-emit and download spawn) prevents re-showing the sync
    //  screen and re-spawning the download after a cancel during 'preparing'.
    this._throwIfCancelled();

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
          : (nextStage ?? this._status.status);

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
        // The download is the cancelable run — its child must occupy the durable
        // _currentProcess slot that cancel()/forceKill() and the late-kill guard target.
        trackAsCancelable: true,
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
    return runBinary(
      binaryName,
      args,
      this._activeWorkDir || stateDirectoryPath,
      {
        logFileName: PARTIAL_SYNC_LOG_FILE_NAME,
      },
      {
        // Track the conversion child in the cancelable slot so cancel()/forceKill() can kill an in-flight
        //  conversion; the runner nulls it again via onProcess(null) on close. _runBinary is conversion-only,
        //  so this unconditional onProcess needs no trackAsCancelable flag.
        onProcess: (child) => {
          this._trackCurrentProcess(child);
        },
      }
    );
  }

  // Only cancelable runs register here (download, the two start()-phase metadata reads, conversion).
  //  Untracked metadata reads get no onProcess, so they can neither overwrite nor null the slot mid-run.
  _trackCurrentProcess(child: ChildProcess | null): void {
    this._currentProcess = child;

    // A cancel can land after the stage is shown but before the child reaches the slot (e.g. _runCommand's
    //  async env prep). Without this, cancel()/forceKill() would miss it and the join would fall to
    //  abandonCancel() while the download keeps running.
    if (child && this._isCancelled) {
      logger.info(
        'MithrilPartialSyncService: killing late-arriving child after cancel',
        {
          pid: child.pid,
        }
      );
      try {
        killProcessTree(child, 'SIGTERM');
      } catch (error) {
        logger.warn(
          'MithrilPartialSyncService: failed to kill late-spawned cancelled process',
          {
            error,
          }
        );
      }
    }
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
      logger.warn(
        'MithrilPartialSyncService: rejecting recovery action; a partial sync run is still in progress',
        { action }
      );
      throw new Error(PARTIAL_SYNC_RECOVERY_NOT_ALLOWED_CODE);
    }

    if (!this._status.allowedRecoveryActions.includes(action)) {
      logger.warn(
        'MithrilPartialSyncService: rejecting recovery action; not allowed from the current recovery boundary',
        { action, allowedRecoveryActions: this._status.allowedRecoveryActions }
      );
      throw new Error(PARTIAL_SYNC_RECOVERY_NOT_ALLOWED_CODE);
    }
  }

  async _cleanupPartialSyncArtifacts(): Promise<void> {
    // Resolve the staging root from the durable marker first so cleanup is
    // correct cross-session and on custom volumes; fall back to the in-session
    // resolver when no persisted path exists.
    const marker = await readMithrilPartialSyncMarker();
    const stagingRoot = marker?.stagingRootPath ?? this._getStagingRootPath();
    await fs.remove(stagingRoot);
    await clearMithrilPartialSyncMarker();
  }

  _resetToIdleStatus(): void {
    // Covers restart-normal/wipe/finalize-wipe/finalize-completed — after a cutover
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

  // Cancel checkpoint at each 'await' seam in start(): a no-op on the success path, throws the
  //  cancellation sentinel once cancel() has set _isCancelled.
  _throwIfCancelled(): void {
    if (this._isCancelled) {
      throw new MithrilPartialSyncCancelledError();
    }
  }

  _clearRuntimeWorkState(): void {
    this._activeWorkDir = null;
    this._currentProcess = null;
    this._stagedDbPath = null;
    this._startedAt = null;
    this._cancelFallbackErrorStage = null;
  }

  _getCurrentRecoveryStage(): MithrilPartialSyncErrorStage {
    return this._status.error?.stage ?? this._getFallbackErrorStage();
  }

  _getStagingRootPath(): string {
    // Colocate staging as a sibling of the managed chain so cutover is an intra-volume rename, never inside
    //  the live chain subtree. _stagingChainDir is the realpath captured at start(); the stateDirectoryPath
    //  fallback covers a session with no sync yet.
    const stagingParent = this._stagingChainDir
      ? path.dirname(this._stagingChainDir)
      : stateDirectoryPath;
    return path.join(stagingParent, PARTIAL_SYNC_STAGING_DIRECTORY_NAME);
  }

  // trackAsCancelable is true only for the two start()-phase reads; the behind-ness probe uses the untracked default.
  async resolveLatestSnapshotMetadata(
    options: PartialSyncMetadataReadOptions = {}
  ): Promise<ResolvedLatestSnapshot> {
    try {
      const latestSnapshot = await this._showSnapshotRaw('latest', options);
      if (latestSnapshot) {
        return latestSnapshot;
      }
    } catch (error) {
      logger.warn(
        'MithrilPartialSyncService: latest snapshot show lookup failed, falling back to list',
        { error }
      );
    }

    const snapshots = await this._listSnapshotsRaw(options);
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
        'Unable to resolve the latest Mithril snapshot metadata.',
        PARTIAL_SYNC_METADATA_UNAVAILABLE_CODE
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
    // Untracked: the behind-ness probe runs concurrently with an active download and must not occupy the cancelable slot.
    const snapshot = await this.resolveLatestSnapshotMetadata();
    this._latestCertifiedImmutableCache = {
      value: snapshot.latestCertifiedImmutableNumber,
      epoch: snapshot.certifiedEpoch, // Carry the certified epoch from the same beacon
      fetchedAt: now,
    };
    return snapshot.latestCertifiedImmutableNumber;
  }

  // Certified epoch carried on the aggregator cache; null when unpopulated or the beacon had no epoch.
  //  Read after _getCachedLatestCertifiedImmutableNumber has populated the cache.
  _getCachedCertifiedEpoch(): number | null {
    return this._latestCertifiedImmutableCache?.epoch ?? null;
  }

  // Cache the local immutable read under the same 5-min TTL as the aggregator. On a
  // hit we skip both the managed-chain path resolution and resolveLocalImmutableNumber's
  // immutable/ readdir — the per-probe cost the 30s poll paid every tick.
  async _getCachedLocalImmutableNumber(): Promise<number> {
    const now = Date.now();
    if (
      this._localImmutableCache &&
      now - this._localImmutableCache.fetchedAt <
        PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS
    ) {
      return this._localImmutableCache.value;
    }
    const managedChainPath =
      await this._chainStorageManager.getManagedChainPath();
    // Fresh install: the chain directory (or its immutable/ subdir) may not exist
    // yet, or the immutable directory may be empty (node just initialised).
    // Treat these as local position 0 so the probe can still fire and the
    // proactive prompt can surface for a no-snapshot node.
    const immutablePath = path.join(managedChainPath, 'immutable');
    let localImmutableNumber: number;
    if (!(await fs.pathExists(immutablePath))) {
      localImmutableNumber = 0;
    } else {
      try {
        localImmutableNumber = await resolveLocalImmutableNumber(managedChainPath);
      } catch (err: any) {
        if (err?.code === 'PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE') {
          localImmutableNumber = 0;
        } else {
          throw err;
        }
      }
    }
    this._localImmutableCache = {
      value: localImmutableNumber,
      fetchedAt: now,
    };
    return localImmutableNumber;
  }

  // Drop both behind-ness input caches so the next probe re-resolves fresh inputs.
  _invalidateBehindnessCaches(): void {
    this._latestCertifiedImmutableCache = null;
    this._localImmutableCache = null;
  }

  // A chain-directory change swaps the chain store under the probe; the cached
  // inputs describe the old location, so drop them before the next read.
  onChainDirectoryChanged(): void {
    this._invalidateBehindnessCaches();
  }

  _getBehindnessThresholdImmutables(): number {
    const configured = launcherConfig.mithrilPartialSyncThresholdImmutables;
    return typeof configured === 'number' && configured > 0
      ? configured
      : DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES;
  }

  async getPartialSyncBehindness(): Promise<{
    isSignificantlyBehind: boolean;
    isProbeFailed?: boolean;
    behindByImmutables?: number;
    certifiedEpoch?: number;
  }> {
    try {
      // The aggregator query and local read are independent, so run them concurrently.
      const [latest, localImmutableNumber] = await Promise.all([
        this._getCachedLatestCertifiedImmutableNumber(),
        this._getCachedLocalImmutableNumber(),
      ]);
      // Same beacon → consistent epoch; null/undefined ⇒ omitted below.
      const certifiedEpoch = this._getCachedCertifiedEpoch();
      const gap = latest - localImmutableNumber;
      if (gap <= 0) {
        // local >= latest ⇒ no certified range to restore ⇒ never nudge
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
      // Aggregator unreachable, no immutable dir yet, or any failure ⇒ degrade
      // to not-behind, but flag the failure so consumers can render an
      // availability-unknown state instead of a confident near-tip one.
      logger.warn(
        'MithrilPartialSyncService: behind-ness probe failed; treating as not significantly behind',
        { error }
      );
      return { isSignificantlyBehind: false, isProbeFailed: true };
    }
  }

  async _listSnapshotsRaw(
    options: PartialSyncMetadataReadOptions = {}
  ): Promise<Array<ResolvedLatestSnapshot>> {
    const { stdout } = await this._runCommand(
      ['cardano-db', 'snapshot', 'list', '--json'],
      { requireKeys: false, ...options }
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
    digest: string,
    options: PartialSyncMetadataReadOptions = {}
  ): Promise<ResolvedLatestSnapshot | null> {
    if (!digest || !digest.trim()) return null;

    const { stdout } = await this._runCommand(
      ['cardano-db', 'snapshot', 'show', digest, '--json'],
      { requireKeys: false, ...options }
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
        'Unable to determine the latest certified range from Mithril snapshot metadata.',
        PARTIAL_SYNC_METADATA_UNAVAILABLE_CODE
      );
    }

    return normalizedSnapshot;
  }

  async _prepareStagingDirectory(
    managedChainPath: string
  ): Promise<PartialSyncStagingPaths> {
    return preparePartialSyncStagingDirectory(
      this._getStagingRootPath(),
      managedChainPath
    );
  }

  async _assertSufficientDiskSpace(
    stagingRootPath: string,
    managedChainPath: string
  ): Promise<void> {
    const snapshotSize = this._latestSnapshot?.size ?? 0;

    let requiredBytes: number;
    try {
      const chainDirBytes =
        await this._chainStorageManager._getPathSizeBytes(managedChainPath);
      requiredBytes = Math.max(
        Math.max(snapshotSize - chainDirBytes, 0) +
          snapshotSize * PARTIAL_SYNC_DISK_MARGIN_FACTOR,
        DISK_SPACE_REQUIRED
      );
    } catch (error) {
      // Local size unmeasurable — require the conservative whole-snapshot bound
      // instead of under-requiring.
      logger.warn(
        'MithrilPartialSyncService: disk-space preflight could not measure the local chain size; using the whole-snapshot bound',
        { error, managedChainPath }
      );
      requiredBytes = Math.max(
        snapshotSize * PARTIAL_SYNC_DISK_SAFETY_FACTOR,
        DISK_SPACE_REQUIRED
      );
    }

    let freeBytes: number;
    try {
      ({ free: freeBytes } = await checkDiskSpace(stagingRootPath));
    } catch (error) {
      // Could not measure free space — fail open rather than false-block (repo precedent: handleDiskSpace.ts).
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
        `Not enough free disk space on the chain storage volume for Mithril Sync. ` +
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
    const { status } = this._status;
    return (
      FALLBACK_ERROR_STAGES.find((stage) => stage === status) ?? 'preparing'
    );
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
    code?: MithrilPartialSyncErrorCode
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
    options: PartialSyncRunCommandOptions = {},
    workDir: string = this._activeWorkDir || stateDirectoryPath
  ): Promise<RunCommandResult> {
    const { trackAsCancelable, ...runnerOptions } = options;

    const callbacks: RunCommandCallbacks = {};
    // Register onProcess only for cancelable runs. Omitting it for untracked reads keeps them from
    //  overwriting the slot on spawn or nulling it on close — the clobber that left a cancelled download unkillable.
    if (trackAsCancelable === true) {
      callbacks.onProcess = (child) => {
        this._trackCurrentProcess(child);
      };
    }

    return runCommand(
      args,
      workDir,
      {
        ...runnerOptions,
        logFileName: PARTIAL_SYNC_LOG_FILE_NAME,
      },
      callbacks
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
