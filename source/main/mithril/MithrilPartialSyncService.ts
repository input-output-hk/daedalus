import path from 'path';
import fs from 'fs-extra';
import type { ChildProcess } from 'child_process';
import EventEmitter from 'events';
import type { Stats, WriteStream } from 'fs';
import { stateDirectoryPath } from '../config';
import { logger } from '../utils/logging';
import type { PartialSyncPreflightContext } from '../utils/chainStorageCoordinator';
import { isPathWithin } from '../utils/chainStorageManagerShared';
import type {
  MithrilSnapshotItem,
  MithrilProgressItem,
} from '../../common/types/mithril-bootstrap.types';
import type {
  MithrilPartialSyncError,
  MithrilPartialSyncErrorStage,
  MithrilPartialSyncStatusUpdate,
} from '../../common/types/mithril-partial-sync.types';
import type {
  RunCommandOptions,
  RunCommandResult,
} from './mithrilCommandRunner';
import { runCommand } from './mithrilCommandRunner';

const PARTIAL_SYNC_LOG_FILE_NAME = 'mithril-partial-sync.log';
const PARTIAL_SYNC_NOT_READY_CODE = 'PARTIAL_SYNC_NOT_READY';
const PARTIAL_SYNC_STAGING_DIRECTORY_NAME = 'mithril-partial-sync';

type ResolvedLatestSnapshot = {
  snapshot: MithrilSnapshotItem;
  latestCertifiedImmutableNumber: number;
};

type PartialSyncStagingPaths = {
  rootPath: string;
  downloadParentPath: string;
  dbPath: string;
};

const DEFAULT_STATUS: MithrilPartialSyncStatusUpdate = {
  status: 'idle',
  allowedRecoveryActions: [],
  error: null,
};

const normalizeSnapshotItem = (
  raw: Record<string, any>
): MithrilSnapshotItem => {
  const digest = raw.digest || raw.snapshot_digest || raw.hash || '';
  const createdAt = raw.created_at || raw.createdAt || raw.timestamp || '';
  const size = Number(
    raw.size ??
      raw.total_size ??
      raw.total_db_size_uncompressed ??
      raw.size_bytes ??
      0
  );
  const cardanoNodeVersion =
    raw.cardano_node_version || raw.cardanoNodeVersion || raw.node_version;
  const network = raw.network || raw.cardano_network || raw.cardanoNetwork;
  return {
    digest,
    createdAt,
    size,
    cardanoNodeVersion,
    network,
  };
};

const toTimestamp = (value: string): number => {
  const timestamp = Date.parse(value);
  return Number.isNaN(timestamp) ? 0 : timestamp;
};

const toPositiveInteger = (value: unknown): number | null => {
  if (typeof value === 'number' && Number.isInteger(value) && value >= 0) {
    return value;
  }

  if (typeof value === 'string' && /^\d+$/.test(value.trim())) {
    return Number(value);
  }

  return null;
};

const getNestedValue = (value: unknown, keyPath: Array<string>): unknown => {
  let current = value;

  for (const key of keyPath) {
    if (!current || typeof current !== 'object') {
      return undefined;
    }

    current = (current as Record<string, unknown>)[key];
  }

  return current;
};

const extractLatestCertifiedImmutableNumber = (
  raw: Record<string, unknown>
): number | null => {
  const explicitPaths = [
    ['beacon', 'immutable_file_number'],
    ['beacon', 'immutableFileNumber'],
    ['cardano_db_beacon', 'immutable_file_number'],
    ['cardanoDbBeacon', 'immutableFileNumber'],
    ['immutable_file_number'],
    ['immutableFileNumber'],
    ['last_immutable_file_number'],
    ['lastImmutableFileNumber'],
  ];

  for (const keyPath of explicitPaths) {
    const parsedNumber = toPositiveInteger(getNestedValue(raw, keyPath));
    if (parsedNumber != null) {
      return parsedNumber;
    }
  }

  return null;
};

const parseImmutableFileNumber = (entryName: string): number | null => {
  const stem = entryName.split('.')[0]?.trim();
  return stem && /^\d+$/.test(stem) ? Number(stem) : null;
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
  _status: MithrilPartialSyncStatusUpdate = { ...DEFAULT_STATUS };
  _statusEmitter = new EventEmitter();
  _currentProcess: ChildProcess | null = null;
  _logStream: WriteStream | null = null;
  _activeWorkDir: string | null = null;
  _startedAt: number | null = null;
  _isCancelled = false;
  _progressItems: MithrilProgressItem[] = [];
  _latestSnapshot: MithrilSnapshotItem | null = null;

  get status(): MithrilPartialSyncStatusUpdate {
    return { ...this._status };
  }

  onStatus(
    listener: (update: MithrilPartialSyncStatusUpdate) => void
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

    try {
      this._addProgressItem('preparing', 'preparing', 'active');
      this._updateStatus({
        status: 'preparing',
        error: null,
        logPath: this._getLogPath(),
        progressItems: [...this._progressItems],
      });

      const latestSnapshot = await this.resolveLatestSnapshotMetadata();
      this._latestSnapshot = latestSnapshot.snapshot;

      const localImmutableNumber = await this._resolveLocalImmutableNumber(
        context.layoutResult.managedChainPath
      );
      const partialSyncRange = this._derivePartialSyncRange(
        localImmutableNumber,
        latestSnapshot.latestCertifiedImmutableNumber
      );
      const stagingPaths = await this._prepareStagingDirectory(
        context.layoutResult.managedChainPath
      );

      this._activeWorkDir = stagingPaths.downloadParentPath;

      logger.info('MithrilPartialSyncService: prepared partial sync preflight', {
        managedChainPath: context.layoutResult.managedChainPath,
        stagingRootPath: stagingPaths.rootPath,
        latestSnapshotDigest: latestSnapshot.snapshot.digest,
        latestCertifiedImmutableNumber:
          latestSnapshot.latestCertifiedImmutableNumber,
        localImmutableNumber,
        partialSyncRange,
      });

      throw this._createStageError(
        'preparing',
        'Mithril partial sync download execution is not implemented yet.',
        PARTIAL_SYNC_NOT_READY_CODE
      );
    } catch (error) {
      if (this._isCancelled) {
        return;
      }

      this._markActiveProgressItemAs('error');
      this._updateStatus({
        status: 'failed',
        error: this._buildError(error, 'preparing'),
        logPath: this._getLogPath(),
        progressItems: [...this._progressItems],
      });
      throw error;
    } finally {
      this._activeWorkDir = null;
      this._currentProcess = null;
      this._logStream = null;
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

    this._isCancelled = true;
    this._progressItems = [];
    this._updateStatus({
      status: 'cancelled',
      error: null,
      logPath: this._getLogPath(),
      progressItems: [],
      filesDownloaded: undefined,
      filesTotal: undefined,
      ancillaryBytesDownloaded: undefined,
      ancillaryBytesTotal: undefined,
      elapsedSeconds: undefined,
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

    this._activeWorkDir = null;
    this._currentProcess = null;
    this._logStream = null;
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

  async _showSnapshotRaw(digest: string): Promise<ResolvedLatestSnapshot | null> {
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
    if (!raw || typeof raw !== 'object') {
      return null;
    }

    const normalizedSnapshot = normalizeSnapshotItem(
      raw as Record<string, unknown>
    );
    const latestCertifiedImmutableNumber = extractLatestCertifiedImmutableNumber(
      raw as Record<string, unknown>
    );

    if (latestCertifiedImmutableNumber == null) {
      throw this._createStageError(
        'preparing',
        'Unable to determine the latest certified immutable number from Mithril snapshot metadata.'
      );
    }

    return {
      snapshot: normalizedSnapshot,
      latestCertifiedImmutableNumber,
    };
  }

  async _resolveLocalImmutableNumber(managedChainPath: string): Promise<number> {
    const managedChainStats = await this._statRequiredPath(
      managedChainPath,
      'Unable to read the managed chain directory for Mithril partial sync preflight.'
    );

    if (!managedChainStats.isDirectory()) {
      throw this._createStageError(
        'preparing',
        'The managed chain path is not a directory.',
        'PARTIAL_SYNC_MANAGED_CHAIN_INVALID'
      );
    }

    const immutablePath = path.join(managedChainPath, 'immutable');
    const immutableStats = await this._statRequiredPath(
      immutablePath,
      'Unable to read the immutable directory for Mithril partial sync preflight.'
    );

    if (!immutableStats.isDirectory()) {
      throw this._createStageError(
        'preparing',
        'The managed chain immutable path is not a directory.',
        'PARTIAL_SYNC_IMMUTABLE_INVALID'
      );
    }

    await this._ensureReadablePath(
      immutablePath,
      'Unable to read the immutable directory for Mithril partial sync preflight.',
      fs.constants.R_OK | fs.constants.X_OK
    );

    const protocolMagicIdPath = path.join(managedChainPath, 'protocolMagicId');
    const protocolMagicIdStats = await this._statRequiredPath(
      protocolMagicIdPath,
      'Unable to read protocolMagicId for Mithril partial sync preflight.'
    );

    if (!protocolMagicIdStats.isFile()) {
      throw this._createStageError(
        'preparing',
        'The managed chain protocolMagicId path is not a file.',
        'PARTIAL_SYNC_PROTOCOL_MAGIC_INVALID'
      );
    }

    await this._ensureReadablePath(
      protocolMagicIdPath,
      'Unable to read protocolMagicId for Mithril partial sync preflight.',
      fs.constants.R_OK
    );

    const immutableEntries = await this._readImmutableDirectory(immutablePath);
    const localImmutableNumber = immutableEntries
      .map((entryName) => parseImmutableFileNumber(entryName))
      .filter((value): value is number => value != null)
      .sort((left, right) => right - left)[0];

    if (localImmutableNumber == null) {
      throw this._createStageError(
        'preparing',
        'Unable to determine the local immutable position from the managed chain immutable directory.',
        'PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE'
      );
    }

    return localImmutableNumber;
  }

  _derivePartialSyncRange(
    localImmutableNumber: number,
    latestCertifiedImmutableNumber: number
  ): { start: number; end: number } {
    if (localImmutableNumber >= latestCertifiedImmutableNumber) {
      throw this._createStageError(
        'preparing',
        'The managed chain is not missing any certified immutable files for Mithril partial sync.',
        'PARTIAL_SYNC_NO_CERTIFIED_RANGE'
      );
    }

    return {
      start: localImmutableNumber + 1,
      end: latestCertifiedImmutableNumber,
    };
  }

  async _prepareStagingDirectory(
    managedChainPath: string
  ): Promise<PartialSyncStagingPaths> {
    const rootPath = path.join(
      stateDirectoryPath,
      PARTIAL_SYNC_STAGING_DIRECTORY_NAME
    );
    const resolvedManagedChainPath = path.resolve(managedChainPath);
    const resolvedRootPath = path.resolve(rootPath);

    if (isPathWithin(resolvedManagedChainPath, resolvedRootPath)) {
      throw this._createStageError(
        'preparing',
        'The partial sync staging directory must be outside the managed chain path.',
        'PARTIAL_SYNC_STAGING_INSIDE_MANAGED_CHAIN'
      );
    }

    await fs.remove(resolvedRootPath);

    const downloadParentPath = path.join(resolvedRootPath, 'download');
    const dbPath = path.join(downloadParentPath, 'db');

    await fs.ensureDir(downloadParentPath);

    return {
      rootPath: resolvedRootPath,
      downloadParentPath,
      dbPath,
    };
  }

  async _statRequiredPath(targetPath: string, message: string): Promise<Stats> {
    try {
      return await fs.stat(targetPath);
    } catch (error) {
      throw this._createStageError('preparing', message);
    }
  }

  async _ensureReadablePath(
    targetPath: string,
    message: string,
    mode: number
  ): Promise<void> {
    try {
      await fs.access(targetPath, mode);
    } catch (error) {
      throw this._createStageError('preparing', message);
    }
  }

  async _readImmutableDirectory(immutablePath: string): Promise<Array<string>> {
    try {
      return await fs.readdir(immutablePath);
    } catch (error) {
      throw this._createStageError(
        'preparing',
        'Unable to read the immutable directory for Mithril partial sync preflight.'
      );
    }
  }

  _updateStatus(update: Partial<MithrilPartialSyncStatusUpdate>): void {
    const nextStatus = update.status ?? this._status.status;
    const normalizedUpdate =
      !('elapsedSeconds' in update) && this._shouldTrackElapsed(nextStatus)
        ? {
            ...update,
            elapsedSeconds: this._getElapsedSeconds(),
          }
        : update;

    this._status = {
      ...this._status,
      ...normalizedUpdate,
      allowedRecoveryActions:
        normalizedUpdate.allowedRecoveryActions ??
        this._status.allowedRecoveryActions ??
        [],
    };
    this._statusEmitter.emit('status', { ...this._status });
  }

  _shouldTrackElapsed(status: MithrilPartialSyncStatusUpdate['status']): boolean {
    return !['idle', 'cancelled', 'failed', 'completed'].includes(status);
  }

  _getElapsedSeconds(): number | undefined {
    if (this._startedAt == null) return undefined;
    return Math.max(0, (Date.now() - this._startedAt) / 1000);
  }

  _markActiveProgressItemAs(state: 'completed' | 'error'): void {
    this._progressItems = this._progressItems.map((item) =>
      item.state === 'active' ? { ...item, state } : item
    );
  }

  _addProgressItem(
    id: string,
    label: string,
    state: MithrilProgressItem['state']
  ): void {
    if (this._progressItems.some((item) => item.id === id)) return;
    this._progressItems = [
      ...this._progressItems,
      { id, label, state, timestamp: new Date().toISOString() },
    ];
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
    try {
      return JSON.parse(payload);
    } catch (error) {
      const lines = payload
        .split('\n')
        .map((line) => line.trim())
        .filter(Boolean);
      for (let index = lines.length - 1; index >= 0; index -= 1) {
        const line = lines[index];
        if (line.startsWith('{') || line.startsWith('[')) {
          try {
            return JSON.parse(line);
          } catch (nestedError) {
            logger.warn('MithrilPartialSyncService: JSON parse failed', {
              error: nestedError,
              payload: line.slice(0, 200),
            });
            break;
          }
        }
      }

      logger.warn('MithrilPartialSyncService: JSON parse failed', {
        error,
        payload: payload?.slice(0, 200),
      });
      return null;
    }
  }
}
