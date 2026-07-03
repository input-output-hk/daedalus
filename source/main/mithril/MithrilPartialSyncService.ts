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
// Disk preflight. snapshot.size is the FULL certified DB, but bytes already present in the
// local chain dir do not need to be fetched again, so the requirement is the missing delta
// (snapshot − chain dir, floored at 0) plus a margin proportional to the FULL snapshot —
// deliberately not to the delta — as headroom for chain growth, LSM-conversion and FS slack.
// Floored at DISK_SPACE_REQUIRED so a missing/zero snapshot size still fails closed on a
// near-full disk. If the local chain size cannot be measured, fall back to the conservative
// whole-snapshot bound (snapshot × safety factor).
const PARTIAL_SYNC_DISK_MARGIN_FACTOR = 0.2;
const PARTIAL_SYNC_DISK_SAFETY_FACTOR = 1 + PARTIAL_SYNC_DISK_MARGIN_FACTOR;

// Behind-ness threshold in IMMUTABLE FILES. Backend-owned; overridable via launcher config.
// Conservative starting point (≈ 1 epoch-equivalent of immutable files); calibrate down
// during QA so the prompt actually fires on a node that is days behind.
const DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES = 20; // ≈ 1 epoch (10·k = 21,600 slots ≈ 6h/file ⇒ ~20 files/5-day epoch); QA-calibratable
const PARTIAL_SYNC_BEHINDNESS_CACHE_TTL_MS = 5 * 60 * 1000; // 5 min; only the aggregator query is cached

// Slot-lifecycle: service-LOCAL _runCommand options. `trackAsCancelable` is the
// explicit, call-site-threaded opt-in that lets a run register its child into the cancelable
// _currentProcess slot. It is deliberately NOT a field on the runner's RunCommandOptions —
// _runCommand strips it before spreading the remaining fields into the runner options — and it
// MUST NOT be inferred from this._activeWorkDir: the 30s behind-ness probe also runs while
// _activeWorkDir is set during `downloading`, so inferring would re-introduce the exact
// slot-clobber this flag fixes.
type PartialSyncRunCommandOptions = RunCommandOptions & {
  trackAsCancelable?: boolean;
};

// Threading shape for the shared metadata-read helpers (resolveLatestSnapshotMetadata →
// _showSnapshotRaw/_listSnapshotsRaw), which call _runCommand WITHOUT the workDir positional —
// the flag rides the options object from the call site down to _runCommand.
type PartialSyncMetadataReadOptions = Pick<
  PartialSyncRunCommandOptions,
  'trackAsCancelable'
>;

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

// Cancellation sentinel thrown by the _throwIfCancelled() checkpoints in
// start(). It is caught by start()'s existing `if (this._isCancelled) return;` guard, so it never
// reaches the `failed` emit — a requested cancel unwinds the stage machine while status is still
// `cancelling`, letting the coordinator join settle into finalizeCancel() (not abandonCancel()).
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
  // Dedupe the per-probe local immutable read (getManagedChainPath → fork
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

      // This start()-phase metadata read is PART of the cancelable run — it
      // registers into _currentProcess so a cancel landing during `preparing` can still kill an
      // in-flight metadata child (preserves cancel-at-`preparing` coverage).
      const latestSnapshot = await this.resolveLatestSnapshotMetadata({
        trackAsCancelable: true,
      });
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

      await this._assertSufficientDiskSpace(
        stagingPaths.rootPath,
        context.layoutResult.managedChainPath
      ); // preflight

      // CP-A: a cancel requested during `preparing` unwinds here — before the
      // latest-drift re-resolve and the download re-spawn — while status is still `cancelling`.
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
      await validateStagedDownloadOutput(stagingPaths, (stage, message, code) =>
        this._createStageError(stage, message, code)
      );

      // CP-C: a cancel during `downloading`/`verifying` unwinds before the
      // `verifying` emit and the `converting` stage.
      this._throwIfCancelled();

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

      // CP-D: last checkpoint before the cutover marker/install. A cancel
      // during `converting` never reaches live-chain cutover (Boundary-A-only cancel invariant).
      // Prompt interruption of an in-flight conversion comes from the tracked converter child
      // (_runBinary onProcess); CP-D is defense-in-depth for the window where a conversion
      // completes successfully just as the kill lands.
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
      // Nothing to cancel in the node-stop window, but a cancel request MUST
      // always re-emit a status so the renderer never sticks on its optimistic stopping-node frame.
      // Re-emit the TRUE current status verbatim (do NOT fabricate `cancelled`); the existing push
      // pipeline (service.onStatus -> broadcastPartialSyncStatus -> _partialSyncStatusSender) delivers it.
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

    // UNCONDITIONAL cancel-entry line — fires even when the slot is EMPTY
    // (hadChild: false), the case the in-branch line below can never log, so a re-repro
    // directly observes the slot state at cancel instead of inferring it from missing kill lines.
    logger.info('MithrilPartialSyncService: cancel entry', {
      status: this._status.status,
      pid: this._currentProcess?.pid ?? null,
      hadChild: this._currentProcess != null,
    });

    try {
      if (this._currentProcess) {
        logger.info(
          'MithrilPartialSyncService: cancelling active partial sync process',
          {
            status: this._status.status,
            pid: this._currentProcess?.pid,
            hadChild: !!this._currentProcess,
          }
        );
        // Group/tree kill (POSIX process.kill(-pid) via the detached spawn;
        // Windows async taskkill /t /f) instead of the direct-pid child.kill().
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
      // Staging removal is best-effort on cancel: a locked or busy staging
      // directory must not strand the user on a failed screen with a cleanup
      // retry loop. No cutover has happened yet, so nothing installed is at
      // risk and no marker exists; the orphaned staging directory is reclaimed
      // when the next partial sync prepares its staging directory.
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

  // Shutdown reap: SIGKILL the tracked cancelable child as the app quits.
  // cancel()/forceKill() never run on a plain quit, so without this a live (detached on
  // POSIX) mithril-client would survive process.exit(). The _currentProcess slot
  // durably names the live download/conversion child while start() is still awaiting it;
  // when start() has already returned the slot is null and this is a harmless no-op.
  // sync: true — safeExitWithCode reaches process.exit() inside a stream-end callback, so
  // an async Windows taskkill issued that late would never launch. Fully try/caught: it
  // must never throw or block inside safeExit().
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

  // Startup-owned recovery emissions update only the controller-held broadcast
  // snapshot; this service starts each session idle, so the allowed-action
  // assertion would reject the very action the backend itself offered. Before a
  // recovery action delegates here, the controller hands us its snapshot and we
  // adopt it as the local boundary — only from idle, and only when the snapshot
  // is a real recovery boundary (non-idle with at least one allowed action).
  // No re-emit: the broadcast that produced the snapshot already delivered it.
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
    // CP-B: a cancel requested during `preparing` (no download process spawned
    // yet, so cancel() has nothing to kill) unwinds here BEFORE the unconditional `downloading`
    // re-emit and the download spawn — this is what stops the "Mithril Sync screen pops back over"
    // frame and the background download re-spawn.
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
        // The download IS the cancelable run — its child must occupy the durable
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
        // Track the awaited `snapshot-converter` conversion child in
        // the cancelable slot exactly as _runCommand does for the download, so cancel()/forceKill()
        // can kill an in-flight conversion instead of only being observed after it returns. The
        // runner nulls _currentProcess again via onProcess(null) on close/error.
        // Invariant: _runBinary is conversion-only (sole consumer:
        // _convertStagedSnapshot), so this unconditional onProcess is opt-in by construction —
        // it never runs for metadata reads and needs no trackAsCancelable flag.
        onProcess: (child) => {
          this._trackCurrentProcess(child);
        },
      }
    );
  }

  // Invariant: only CANCELABLE runs may register here — the download and the two
  // start()-phase metadata reads (via _runCommand with trackAsCancelable: true) and the
  // conversion (via _runBinary's unconditional onProcess). Untracked metadata reads (the 30s
  // behind-ness probe, ad-hoc listSnapshots/showSnapshot) receive NO onProcess callback, so
  // they can neither overwrite nor null the slot mid-run (the confirmed slot-clobber). They DO
  // keep onLogStream, which still overwrites the shared _logStream slot mid-run — safe ONLY
  // while _logStream stays write-only (declared as a class field, nulled in
  // _clearRuntimeWorkState(), assigned in _runCommand, never read).
  _trackCurrentProcess(child: ChildProcess | null): void {
    this._currentProcess = child;

    // A cancel can land after the UI has exposed the stage but before the spawned child reaches
    // the tracked slot (notably _runCommand's async env prep before spawn). When that happens,
    // cancel()/forceKill() would otherwise miss the late child and the coordinator would fall to
    // abandonCancel() while the download keeps running in the background.
    if (child && this._isCancelled) {
      logger.info(
        'MithrilPartialSyncService: killing late-arriving child after cancel',
        {
          pid: child.pid,
        }
      );
      try {
        // Group/tree kill, matching the cancel()/forceKill() routing.
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

  // Cancellation checkpoint used at each `await` seam in start() (CP-A..CP-D)
  // and at the top of _downloadAndVerifyPartialSnapshot (CP-B). A no-op on the success path; throws
  // the swallowed cancellation sentinel once cancel() has flipped _isCancelled.
  _throwIfCancelled(): void {
    if (this._isCancelled) {
      throw new MithrilPartialSyncCancelledError();
    }
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
    // Colocate staging as a SIBLING of the resolved managed chain: same volume
    // as the chain so cutover is an intra-volume rename, never inside the live chain subtree (the
    // isPathWithin guard in preparePartialSyncStagingDirectory still enforces that). _stagingChainDir is the
    // realpath-resolved chain dir captured at start(); the stateDirectoryPath fallback preserves the legacy
    // default location when no sync has run this session.
    const stagingParent = this._stagingChainDir
      ? path.dirname(this._stagingChainDir)
      : stateDirectoryPath;
    return path.join(stagingParent, PARTIAL_SYNC_STAGING_DIRECTORY_NAME);
  }

  // Callers opt their metadata child into the cancelable _currentProcess slot via
  // options.trackAsCancelable — true ONLY from the two start()-phase reads (part of the
  // cancelable run); the 30s behind-ness probe uses the untracked default.
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
    // Untracked (the default): the background behind-ness probe must NEVER occupy the
    // cancelable _currentProcess slot — it runs concurrently with an active download.
    const snapshot = await this.resolveLatestSnapshotMetadata();
    this._latestCertifiedImmutableCache = {
      value: snapshot.latestCertifiedImmutableNumber,
      epoch: snapshot.certifiedEpoch, // Carry the certified epoch from the same beacon
      fetchedAt: now,
    };
    return snapshot.latestCertifiedImmutableNumber;
  }

  // Sibling getter for the certified epoch carried on the aggregator cache. Returns
  // `null` when the cache is unpopulated or the beacon had no epoch. Does NOT change the immutable
  // getter's `number` return; read it AFTER _getCachedLatestCertifiedImmutableNumber populates the cache.
  _getCachedCertifiedEpoch(): number | null {
    return this._latestCertifiedImmutableCache?.epoch ?? null;
  }

  // Cache the local immutable read under the same 5-min TTL as the aggregator. On a
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
    const managedChainPath =
      await this._chainStorageManager.getManagedChainPath();
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

  // Drop both behind-ness input caches so the next probe re-resolves fresh. Called at
  // start()/cancel() entry, inside _resetToIdleStatus() (restart-normal/wipe/finalize
  // paths), and on chain-directory changes (onChainDirectoryChanged).
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
    certifiedEpoch?: number | null;
  }> {
    try {
      // Cached local read dedupes the checkDiskSpace fork + immutable/ readdir;
      // the aggregator query and the local read are independent, so run them
      // concurrently.
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

  async listSnapshots(): Promise<Array<MithrilSnapshotItem>> {
    const snapshots = await this._listSnapshotsRaw();
    return snapshots.map(({ snapshot }) => snapshot);
  }

  async showSnapshot(digest: string): Promise<MithrilSnapshotItem | null> {
    const resolved = await this._showSnapshotRaw(digest);
    return resolved ? resolved.snapshot : null;
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
      managedChainPath,
      (stage, message, code) => this._createStageError(stage, message, code)
    );
  }

  async _assertSufficientDiskSpace(
    stagingRootPath: string,
    managedChainPath: string
  ): Promise<void> {
    const snapshotSize = this._latestSnapshot?.size ?? 0;

    let requiredBytes: number;
    try {
      const chainDirBytes = await this._chainStorageManager._getPathSizeBytes(
        managedChainPath
      );
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
    options: PartialSyncRunCommandOptions = {},
    workDir: string = this._activeWorkDir || stateDirectoryPath
  ): Promise<RunCommandResult> {
    // Strip the service-local trackAsCancelable flag BEFORE building the runner options: the
    // spread below must forward only the runner's own RunCommandOptions fields.
    const { trackAsCancelable, ...runnerOptions } = options;

    const callbacks: RunCommandCallbacks = {
      // ALWAYS registered — even untracked metadata reads write to the shared partial-sync log
      // file. A metadata read's onLogStream overwrites the shared _logStream slot mid-run,
      // which is harmless ONLY because _logStream is write-only plumbing today: declared as a
      // class field, nulled in _clearRuntimeWorkState(), assigned here, never read.
      onLogStream: (logStream) => {
        this._logStream = logStream;
      },
    };
    // Only explicitly cancelable runs (the download and the start()-phase metadata
    // reads) may register into the cancelable _currentProcess slot. When trackAsCancelable is
    // false/absent, omit onProcess ENTIRELY so an untracked metadata child can neither
    // overwrite the slot on spawn nor null it on its own clean close (the confirmed
    // slot-clobber that left a cancelled download untracked and unkillable).
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
