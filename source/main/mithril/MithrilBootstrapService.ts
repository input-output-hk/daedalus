import path from 'path';
import fs from 'fs-extra';
import type { ChildProcess } from 'child_process';
import EventEmitter from 'events';
import type { WriteStream } from 'fs';
import { environment } from '../environment';
import { logger } from '../utils/logging';
import ensureDirectoryExists from '../utils/ensureDirectoryExists';
import { stateDirectoryPath } from '../config';
import type {
  MithrilBootstrapError,
  MithrilBootstrapErrorStage,
  MithrilBootstrapStatusUpdate,
  MithrilSnapshotItem,
  MithrilProgressItem,
} from '../../common/types/mithril-bootstrap.types';
import { parseMithrilProgressUpdate } from './mithrilProgress';
import type {
  RunCommandResult,
  RunCommandOptions,
} from './mithrilCommandRunner';
import { runCommand } from './mithrilCommandRunner';
import {
  MithrilBootstrapStageError,
  buildMithrilError,
  createStageError,
  inferErrorStageFromStatus,
} from './mithrilErrors';

const DEFAULT_STATUS: MithrilBootstrapStatusUpdate = {
  status: 'idle',
  snapshot: null,
  error: null,
};

const STEP_LABEL_MAP: Record<number, string> = {
  1: 'disk-check',
  2: 'certificate-chain',
  3: 'downloading-snapshot',
  4: 'verifying-digests',
  5: 'verifying-database',
  6: 'computing-message',
  7: 'verifying-signature',
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

const isNonEmptyString = (value: unknown): value is string =>
  typeof value === 'string' && value.trim().length > 0;

export class MithrilBootstrapService {
  _status: MithrilBootstrapStatusUpdate = { ...DEFAULT_STATUS };
  _statusEmitter = new EventEmitter();
  _currentProcess: ChildProcess | null = null;
  _logStream: WriteStream | null = null;
  _cardanoDbDownloadMode?: 'download' | 'snapshot';
  _lockFilePath: string;
  _workDir: string;
  _bootstrapStartedAt: number | null = null;
  _progressItems: MithrilProgressItem[] = [];
  _isCancelled = false;
  _lastFilesDownloaded: number | undefined = undefined;
  _lastFilesTotal: number | undefined = undefined;
  _verifyingSynthesized = false;

  constructor(workDir: string = stateDirectoryPath) {
    this._workDir = workDir;
    this._lockFilePath = path.join(
      stateDirectoryPath,
      'Logs',
      'mithril-bootstrap.lock'
    );
  }

  get status(): MithrilBootstrapStatusUpdate {
    return { ...this._status };
  }

  setWorkDir(workDir: string): void {
    this._workDir = workDir;
  }

  onStatus(
    listener: (update: MithrilBootstrapStatusUpdate) => void
  ): () => void {
    this._statusEmitter.on('status', listener);
    return () => this._statusEmitter.removeListener('status', listener);
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
    if (!isNonEmptyString(digest)) return null;
    const { stdout } = await this._runCommand(
      ['cardano-db', 'snapshot', 'show', digest, '--json'],
      { requireKeys: false }
    );
    const parsed = this._safeJsonParse(stdout);
    if (!parsed || typeof parsed !== 'object') return null;
    return normalizeSnapshotItem(parsed);
  }

  async startBootstrap(
    digest?: string,
    options?: {
      wipeChain?: boolean;
    }
  ): Promise<void> {
    if (this._currentProcess) {
      throw new Error('Mithril bootstrap already in progress');
    }

    if (options?.wipeChain) {
      await this.wipeChainAndSnapshots('bootstrap-request');
    }
    this._isCancelled = false;
    this._lastFilesDownloaded = undefined;
    this._lastFilesTotal = undefined;
    this._verifyingSynthesized = false;
    this._bootstrapStartedAt = Date.now();
    this._progressItems = [];
    await this._createLockFile();

    this._addProgressItem('preparing', 'preparing', 'active');
    this._updateStatus({
      status: 'preparing',
      filesDownloaded: undefined,
      filesTotal: undefined,
      ancillaryBytesDownloaded: undefined,
      ancillaryBytesTotal: undefined,
      error: null,
      progressItems: [...this._progressItems],
    });

    const snapshotDigest = isNonEmptyString(digest) ? digest : 'latest';
    let snapshot: MithrilSnapshotItem | null = null;

    try {
      snapshot = await this.showSnapshot(snapshotDigest);
      if (snapshot) {
        this._updateStatus({
          snapshot,
          progressItems: [...this._progressItems],
        });
      }
    } catch (error) {
      logger.warn('MithrilBootstrapService: unable to fetch snapshot details', {
        error,
        snapshotDigest,
      });
    }

    try {
      // Mark preparing complete before beginning download
      this._markActiveProgressItemAs('completed');
      await this._downloadSnapshot(snapshotDigest, snapshot);

      // Conversion would go here when needed:
      // this._markActiveProgressItemAs('completed');
      // this._addProgressItem('conversion', 'conversion', 'active');
      // await this._convertSnapshot(snapshot);
      // Skip conversion - Mithril provides in-memory format snapshots, no conversion needed

      const dbDirectory = await this._resolveDbDirectory(snapshot?.digest);

      // Mark final download step complete; begin installation
      this._markActiveProgressItemAs('completed');
      this._addProgressItem('install-snapshot', 'install-snapshot', 'active');
      this._updateStatus({
        status: 'unpacking',
        filesDownloaded: undefined,
        filesTotal: undefined,
        ancillaryBytesDownloaded: undefined,
        ancillaryBytesTotal: undefined,
        progressItems: [...this._progressItems],
      });

      await this._installSnapshot(dbDirectory);

      // Mark install complete; begin cleanup
      this._markActiveProgressItemAs('completed');
      this._addProgressItem('cleanup', 'cleanup', 'active');
      this._updateStatus({
        status: 'finalizing',
        filesDownloaded: undefined,
        filesTotal: undefined,
        ancillaryBytesDownloaded: undefined,
        ancillaryBytesTotal: undefined,
        progressItems: [...this._progressItems],
      });

      await this._cleanupSnapshotArtifacts({ preserveDb: true });
      await this.clearLockFile();

      this._markActiveProgressItemAs('completed');
      this._updateStatus({
        status: 'finalizing',
        filesDownloaded: undefined,
        filesTotal: undefined,
        progressItems: [...this._progressItems],
      });

      this._updateStatus({
        status: 'completed',
        filesDownloaded: undefined,
        filesTotal: undefined,
        progressItems: [...this._progressItems],
      });
    } catch (error) {
      if (this._isCancelled) {
        throw error;
      }
      const normalizedError = this._buildError(
        error,
        this._inferErrorStageFromStatus(this._status.status)
      );
      this._markActiveProgressItemAs('error');
      await this._cleanupSnapshotArtifacts({ preserveDb: false });
      this._updateStatus({
        status: 'failed',
        filesDownloaded: undefined,
        filesTotal: undefined,
        ancillaryBytesDownloaded: undefined,
        ancillaryBytesTotal: undefined,
        progressItems: [...this._progressItems],
        error: normalizedError,
      });
      throw error;
    }
  }

  async cancel(): Promise<void> {
    this._isCancelled = true;
    this._progressItems = [];
    this._updateStatus({
      status: 'cancelled',
      progressItems: [],
    });

    try {
      if (this._currentProcess) {
        this._currentProcess.kill();
        this._currentProcess = null;
      }
    } catch (error) {
      logger.warn('MithrilBootstrapService: failed to kill process', { error });
    }

    await this._cleanupSnapshotArtifacts({ preserveDb: false });
    await this.clearLockFile();
  }

  async wipeChainAndSnapshots(reason: string): Promise<void> {
    const chainDir = path.join(stateDirectoryPath, 'chain');
    try {
      if (await fs.pathExists(chainDir)) {
        await fs.emptyDir(chainDir);
      }
      await this._cleanupSnapshotArtifacts({ preserveDb: false });
      await this.clearLockFile();
      logger.info(`[MITHRIL] ${reason}`);
    } catch (error) {
      logger.warn('MithrilBootstrapService: failed to wipe chain data', {
        error,
      });
    }
  }

  _updateStatus(update: Partial<MithrilBootstrapStatusUpdate>) {
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
    };
    this._statusEmitter.emit('status', { ...this._status });
  }

  _shouldTrackElapsed(status: MithrilBootstrapStatusUpdate['status']) {
    return !['idle', 'decision', 'cancelled'].includes(status);
  }

  _getElapsedSeconds(): number | undefined {
    if (this._bootstrapStartedAt == null) return undefined;
    return Math.max(0, (Date.now() - this._bootstrapStartedAt) / 1000);
  }

  _updateProgressStep(stepNum: number): void {
    const id = `step-${stepNum}`;
    if (this._progressItems.some((item) => item.id === id)) return;
    this._progressItems = this._progressItems.map((item) =>
      item.state === 'active' ? { ...item, state: 'completed' as const } : item
    );
    const label = STEP_LABEL_MAP[stepNum] ?? `step-${stepNum}`;
    this._progressItems = [
      ...this._progressItems,
      {
        id,
        label,
        state: 'active' as const,
        timestamp: new Date().toISOString(),
      },
    ];
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

  async _resolveCardanoDbDownloadMode(): Promise<'download' | 'snapshot'> {
    if (this._cardanoDbDownloadMode) return this._cardanoDbDownloadMode;
    try {
      const { stdout } = await this._runCommand(['cardano-db', '--help']);
      const hasDownload =
        stdout.includes('\n  download') ||
        stdout.includes('download  Download');
      this._cardanoDbDownloadMode = hasDownload ? 'download' : 'snapshot';
    } catch (error) {
      logger.warn(
        'MithrilBootstrapService: unable to resolve download command',
        {
          error,
        }
      );
      this._cardanoDbDownloadMode = 'download';
    }
    return this._cardanoDbDownloadMode;
  }

  _buildError(
    error: unknown,
    fallbackStage?: MithrilBootstrapErrorStage
  ): MithrilBootstrapError {
    return {
      ...buildMithrilError(error, fallbackStage),
      logPath: this._getBootstrapLogPath(),
    };
  }

  _getBootstrapLogPath(): string {
    return path.join(stateDirectoryPath, 'Logs', 'mithril-bootstrap.log');
  }

  _createStageError(
    stage: MithrilBootstrapErrorStage,
    message: string,
    code?: string
  ): MithrilBootstrapStageError {
    return createStageError(stage, message, code);
  }

  _buildCommandFailure(
    stage: MithrilBootstrapErrorStage,
    fallbackMessage: string,
    stderr?: string
  ): MithrilBootstrapStageError {
    const detail = this._extractRelevantStderr(stderr);
    const message =
      this._extractRelevantStderrMessage(detail) || fallbackMessage;

    return this._createStageError(stage, message, detail);
  }

  _inferErrorStageFromStatus(
    status: MithrilBootstrapStatusUpdate['status']
  ): MithrilBootstrapErrorStage | undefined {
    return inferErrorStageFromStatus(status);
  }

  _extractRelevantStderr(stderr?: string): string | undefined {
    const trimmed = stderr?.trim();
    if (!trimmed) return undefined;

    const errorIndex = trimmed.lastIndexOf('Error:');
    if (errorIndex >= 0) {
      return trimmed.slice(errorIndex).trim().slice(0, 4000);
    }

    const lines = trimmed
      .split('\n')
      .map((line) => line.trimEnd())
      .filter((line) => {
        const normalized = line.trim();
        return (
          normalized.length > 0 &&
          !normalized.startsWith('{') &&
          !normalized.startsWith('src:')
        );
      });

    if (lines.length === 0) return trimmed.slice(-4000);

    return lines.slice(-8).join('\n').trim().slice(0, 4000);
  }

  _extractRelevantStderrMessage(detail?: string): string | undefined {
    if (!detail) return undefined;

    const lines = detail
      .split('\n')
      .map((line) => line.trim())
      .filter(Boolean);

    const explicitError = lines.find((line) => line.startsWith('Error:'));
    if (explicitError) {
      return explicitError.replace(/^Error:\s*/, '').trim();
    }

    return lines[0];
  }

  async _createLockFile(): Promise<void> {
    try {
      const logsDir = path.dirname(this._lockFilePath);
      ensureDirectoryExists(logsDir);
      await fs.writeFile(this._lockFilePath, new Date().toISOString());
    } catch (error) {
      logger.warn('MithrilBootstrapService: failed to create lock file', {
        error,
      });
    }
  }

  async clearLockFile(): Promise<void> {
    try {
      await fs.remove(this._lockFilePath);
    } catch (error) {
      logger.warn('MithrilBootstrapService: failed to remove lock file', {
        error,
      });
    }
  }

  async _cleanupSnapshotArtifacts(options: {
    preserveDb: boolean;
  }): Promise<void> {
    try {
      const dataDir = path.join(this._workDir, 'data');
      if (await fs.pathExists(dataDir)) {
        await fs.remove(dataDir);
      }
      if (!options.preserveDb) {
        const dbDir = path.join(this._workDir, 'db');
        if (await fs.pathExists(dbDir)) {
          await fs.remove(dbDir);
        }
      }
    } catch (error) {
      logger.warn('MithrilBootstrapService: failed to cleanup artifacts', {
        error,
      });
    }
  }

  async _runCommand(
    args: Array<string>,
    options: RunCommandOptions = {}
  ): Promise<RunCommandResult> {
    return runCommand(args, this._workDir, options, {
      onProcess: (child) => {
        this._currentProcess = child;
      },
      onLogStream: (logStream) => {
        this._logStream = logStream;
      },
    });
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
            logger.warn('MithrilBootstrapService: JSON parse failed', {
              error: nestedError,
              payload: line.slice(0, 200),
            });
            break;
          }
        }
      }

      logger.warn('MithrilBootstrapService: JSON parse failed', {
        error,
        payload: payload?.slice(0, 200),
      });
      return null;
    }
  }

  async _downloadSnapshot(
    digest: string,
    snapshot: MithrilSnapshotItem | null
  ): Promise<void> {
    await this._cleanupSnapshotArtifacts({ preserveDb: false });
    this._addProgressItem('downloading', 'downloading', 'active');
    this._updateStatus({
      status: 'downloading',
      progressItems: [...this._progressItems],
    });

    let stdoutBuffered = '';
    let stderrBuffered = '';
    const downloadMode = await this._resolveCardanoDbDownloadMode();
    const downloadArgs =
      downloadMode === 'download'
        ? ['cardano-db', 'download', '--include-ancillary', digest, '--json']
        : [
            'cardano-db',
            'snapshot',
            'download',
            '--include-ancillary',
            digest,
            '--json',
          ];
    const applyProgressUpdate = (line: string) => {
      if (this._isCancelled) return;

      const update = parseMithrilProgressUpdate(line);
      if (!update) return;

      // Track progress steps when mithril-client announces a step number
      if (update.stepNum != null) {
        this._updateProgressStep(update.stepNum);
      }

      // Synthesize completion when CLI transitions to verification (step 4)
      if (
        update.stepNum != null &&
        update.stepNum >= 4 &&
        !this._verifyingSynthesized
      ) {
        this._verifyingSynthesized = true;
        const verifyingUpdate: Partial<MithrilBootstrapStatusUpdate> = {
          status: 'verifying',
          snapshot: snapshot || undefined,
          progressItems: [...this._progressItems],
        };
        if (this._lastFilesTotal != null) {
          verifyingUpdate.filesDownloaded = this._lastFilesTotal;
          verifyingUpdate.filesTotal = this._lastFilesTotal;
        }
        if (this._status.ancillaryBytesTotal != null) {
          verifyingUpdate.ancillaryBytesDownloaded = this._status.ancillaryBytesTotal;
          verifyingUpdate.ancillaryBytesTotal = this._status.ancillaryBytesTotal;
        }
        this._updateStatus(verifyingUpdate);
        return;
      }

      // After verification started, drop late download-phase updates
      if (this._verifyingSynthesized) {
        if (update.label === 'Ancillary' || update.label === 'Files') return;

        // Step-only updates during verification (steps 5-7)
        if (update.stepNum != null) {
          this._updateStatus({
            status: 'verifying',
            snapshot: snapshot || undefined,
            progressItems: [...this._progressItems],
          });
        }
        return;
      }

      // Route Ancillary progress to dedicated ancillary fields only
      if (update.label === 'Ancillary') {
        this._updateStatus({
          status: 'downloading',
          snapshot: snapshot || undefined,
          ancillaryBytesDownloaded: update.bytesDownloaded,
          ancillaryBytesTotal: update.bytesTotal,
          progressItems: [...this._progressItems],
        });
        return;
      }

      // Files stream or no label (graceful degradation for older mithril-client)
      const { progress } = update;
      if (progress != null && !Number.isNaN(progress)) {
        this._lastFilesDownloaded = update.filesDownloaded;
        this._lastFilesTotal = update.filesTotal;
        this._updateStatus({
          status: 'downloading',
          snapshot: snapshot || undefined,
          filesDownloaded: update.filesDownloaded,
          filesTotal: update.filesTotal,
          elapsedSeconds: update.elapsedSeconds,
          progressItems: [...this._progressItems],
        });
        return;
      }

      if (update.elapsedSeconds != null || update.stepNum != null) {
        this._updateStatus({
          status: 'downloading',
          snapshot: snapshot || undefined,
          filesDownloaded: update.filesDownloaded ?? this._lastFilesDownloaded,
          filesTotal: update.filesTotal ?? this._lastFilesTotal,
          elapsedSeconds: update.elapsedSeconds,
          progressItems: [...this._progressItems],
        });
      }
    };
    const { exitCode, stderr } = await this._runCommand(downloadArgs, {
      onStdout: (chunk) => {
        stdoutBuffered += chunk;
        const lines = stdoutBuffered.split('\n');
        stdoutBuffered = lines.pop() || '';
        lines.forEach(applyProgressUpdate);
      },
      onStderr: (chunk) => {
        stderrBuffered += chunk;
        const lines = stderrBuffered.split('\n');
        stderrBuffered = lines.pop() || '';
        lines.forEach(applyProgressUpdate);
      },
    });

    if (exitCode !== 0) {
      const errorStage = this._verifyingSynthesized ? 'verify' : 'download';
      throw this._buildCommandFailure(
        errorStage,
        `Mithril ${
          errorStage === 'verify' ? 'verification' : 'download'
        } failed with exit code ${exitCode}`,
        stderr
      );
    }

    this._updateStatus({
      status: this._verifyingSynthesized ? 'verifying' : 'downloading',
      snapshot: snapshot || undefined,
      progressItems: [...this._progressItems],
    });
  }

  async _convertSnapshot(snapshot: MithrilSnapshotItem | null): Promise<void> {
    this._updateStatus({
      status: 'converting',
    });

    const dbDirectory = await this._resolveDbDirectory(snapshot?.digest);
    const cardanoNodeVersion =
      snapshot?.cardanoNodeVersion || environment.nodeVersion || 'latest';

    const { exitCode, stderr } = await this._runCommand([
      'tools',
      'utxo-hd',
      'snapshot-converter',
      '--db-directory',
      dbDirectory,
      '--cardano-node-version',
      cardanoNodeVersion,
      '--utxo-hd-flavor',
      'LMDB',
      '--commit',
    ]);

    if (exitCode !== 0) {
      throw this._buildCommandFailure(
        'convert',
        `Mithril snapshot conversion failed with exit code ${exitCode}`,
        stderr
      );
    }

    this._updateStatus({
      status: 'finalizing',
    });
  }

  async _resolveDbDirectory(digest?: string): Promise<string> {
    const candidates = [
      path.join(this._workDir, 'db'),
      digest
        ? path.join(
            this._workDir,
            'data',
            String(environment.network),
            digest,
            'db'
          )
        : null,
      digest ? path.join(this._workDir, 'data', digest, 'db') : null,
    ].filter(Boolean) as Array<string>;

    for (const candidate of candidates) {
      if (await fs.pathExists(candidate)) {
        return candidate;
      }
    }

    throw new Error('Unable to locate downloaded Mithril snapshot database');
  }

  async _installSnapshot(dbDirectory: string): Promise<void> {
    const chainDir = path.join(stateDirectoryPath, 'chain');
    const resolvedDbDirectory = path.resolve(dbDirectory);
    let resolvedChainDir = path.resolve(chainDir);

    try {
      if (await fs.pathExists(chainDir)) {
        resolvedChainDir = await fs.realpath(chainDir);
      }
    } catch (error) {
      logger.warn(
        'MithrilBootstrapService: unable to resolve chain directory',
        {
          error,
          chainDir,
        }
      );
    }

    if (resolvedDbDirectory === resolvedChainDir) {
      return;
    }

    if (path.dirname(resolvedDbDirectory) === resolvedChainDir) {
      const entries = await fs.readdir(resolvedDbDirectory);
      for (const entry of entries) {
        const sourceEntry = path.join(resolvedDbDirectory, entry);
        const targetEntry = path.join(resolvedChainDir, entry);
        await fs.move(sourceEntry, targetEntry, { overwrite: true });
      }
      await fs.remove(resolvedDbDirectory);
      return;
    }

    if (await fs.pathExists(chainDir)) {
      const chainStats = await fs.lstat(chainDir);
      if (chainStats.isSymbolicLink()) {
        await fs.emptyDir(resolvedChainDir);
        const entries = await fs.readdir(resolvedDbDirectory);
        for (const entry of entries) {
          const sourceEntry = path.join(resolvedDbDirectory, entry);
          const targetEntry = path.join(resolvedChainDir, entry);
          await fs.move(sourceEntry, targetEntry, { overwrite: true });
        }
        await fs.remove(resolvedDbDirectory);
        return;
      }

      await fs.remove(chainDir);
    }
    await fs.move(dbDirectory, chainDir, { overwrite: true });
  }
}
