import path from 'path';
import fs from 'fs-extra';
import type { ChildProcess } from 'child_process';
import EventEmitter from 'events';
import type { WriteStream } from 'fs';
import { launcherConfig, stateDirectoryPath } from '../config';
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
  _startedAt: number | null = null;
  _isCancelled = false;
  _progressItems: MithrilProgressItem[] = [];
  _latestSnapshot: MithrilSnapshotItem | null = null;
  _stagedDbPath: string | null = null;
  _chainStorageManager: ChainStorageManager;

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

    this._activeWorkDir = context.mithrilWorkDir;
    this._startedAt = Date.now();
    this._isCancelled = false;
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

      const latestSnapshotAtDownload =
        await this.resolveLatestSnapshotMetadata();
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
      });
      await this._installValidatedStagedSnapshot(stagingPaths.dbPath);
      await writeMithrilPartialSyncMarker('installed-awaiting-node-start', {
        managedChainPath: context.layoutResult.managedChainPath,
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
    if (!this._activeWorkDir && !this._currentProcess) {
      logger.info(
        'MithrilPartialSyncService: ignoring cancel request with no active partial sync',
        null
      );
      return;
    }

    if (['installing', 'finalizing'].includes(this._status.status)) {
      throw new Error(
        'Mithril partial sync cancellation is no longer allowed after live chain cutover has started.'
      );
    }

    this._isCancelled = true;

    try {
      if (this._currentProcess) {
        this._currentProcess.kill();
      }
    } catch (error) {
      logger.warn('MithrilPartialSyncService: failed to kill process', {
        error,
      });
    }

    try {
      await this._cleanupPartialSyncArtifacts();
      this._progressItems = [];
      this._updateStatus({
        status: 'cancelled',
        allowedRecoveryActions: [
          'retry',
          'restart-normal',
          'wipe-and-full-sync',
        ],
        error: null,
        logPath: this._getLogPath(),
        progressItems: [],
        transferProgress: {},
      });
    } catch (error) {
      this._markActiveProgressItemAs('error');
      this._updateStatus({
        status: 'failed',
        allowedRecoveryActions: [
          'retry',
          'restart-normal',
          'wipe-and-full-sync',
        ],
        error: this._buildError(error, this._getCurrentRecoveryStage()),
        logPath: this._getLogPath(),
        progressItems: [...this._progressItems],
        transferProgress: {},
      });
      throw error;
    } finally {
      this._clearRuntimeWorkState();
    }
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
    this._progressItems = [];
    this._latestSnapshot = null;
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
  }

  _getCurrentRecoveryStage(): MithrilPartialSyncErrorStage {
    return this._status.error?.stage ?? this._getFallbackErrorStage();
  }

  _getStagingRootPath(): string {
    return path.join(stateDirectoryPath, PARTIAL_SYNC_STAGING_DIRECTORY_NAME);
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
