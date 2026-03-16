import path from 'path';
import https from 'https';
import fs from 'fs-extra';
import { spawn } from 'child_process';
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
} from '../../common/types/mithril-bootstrap.types';
import { parseMithrilProgressUpdate } from './mithrilProgress';

type MithrilNetworkConfig = {
  aggregatorEndpoint: string;
  genesisKeyUrl: string;
  ancillaryKeyUrl: string;
  genesisKey?: string;
  ancillaryKey?: string;
};

type RunCommandResult = {
  stdout: string;
  stderr: string;
  exitCode: number | null;
};

type RunCommandOptions = {
  onStdout?: (chunk: string) => void;
  onStderr?: (chunk: string) => void;
  allowJsonParseErrors?: boolean;
  requireKeys?: boolean;
};

const MITHRIL_NETWORK_CONFIG: Record<string, MithrilNetworkConfig> = {
  mainnet: {
    aggregatorEndpoint:
      'https://aggregator.release-mainnet.api.mithril.network/aggregator',
    genesisKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey',
    ancillaryKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/ancillary.vkey',
  },
  preprod: {
    aggregatorEndpoint:
      'https://aggregator.release-preprod.api.mithril.network/aggregator',
    genesisKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey',
    ancillaryKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/ancillary.vkey',
  },
  preview: {
    aggregatorEndpoint:
      'https://aggregator.pre-release-preview.api.mithril.network/aggregator',
    genesisKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey',
    ancillaryKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/ancillary.vkey',
  },
};

const DEFAULT_STATUS: MithrilBootstrapStatusUpdate = {
  status: 'idle',
  progress: 0,
  snapshot: null,
  error: null,
};

const STEP_PROGRESS = {
  preparing: 5,
  downloadingStart: 10,
  downloadingEnd: 90,
  finalizingStart: 92.5,
  finalizingProcessing: 95,
  finalizingEnd: 97.5,
  completed: 100,
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

class MithrilBootstrapStageError extends Error {
  stage: MithrilBootstrapErrorStage;
  code?: string;

  constructor(
    message: string,
    stage: MithrilBootstrapErrorStage,
    code?: string
  ) {
    super(message);
    this.name = 'MithrilBootstrapStageError';
    this.stage = stage;
    this.code = code;
  }
}

export class MithrilBootstrapService {
  _status: MithrilBootstrapStatusUpdate = { ...DEFAULT_STATUS };
  _statusEmitter = new EventEmitter();
  _currentProcess: ChildProcess | null = null;
  _logStream: WriteStream | null = null;
  _cardanoDbDownloadMode?: 'download' | 'snapshot';
  _lockFilePath: string;
  _workDir: string;
  _bootstrapStartedAt: number | null = null;

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
    this._bootstrapStartedAt = Date.now();
    await this._createLockFile();

    this._updateStatus({
      status: 'preparing',
      progress: STEP_PROGRESS.preparing,
      filesDownloaded: undefined,
      filesTotal: undefined,
      remainingSeconds: undefined,
      error: null,
    });

    const snapshotDigest = isNonEmptyString(digest) ? digest : 'latest';
    let snapshot: MithrilSnapshotItem | null = null;

    try {
      snapshot = await this.showSnapshot(snapshotDigest);
      if (snapshot) {
        this._updateStatus({ snapshot });
      }
    } catch (error) {
      logger.warn('MithrilBootstrapService: unable to fetch snapshot details', {
        error,
        snapshotDigest,
      });
    }

    try {
      await this._downloadSnapshot(snapshotDigest, snapshot);
      // Skip conversion - Mithril provides in-memory format snapshots, no conversion needed
      // await this._convertSnapshot(snapshot);
      const dbDirectory = await this._resolveDbDirectory(snapshot?.digest);

      this._updateStatus({
        status: 'unpacking',
        progress: STEP_PROGRESS.finalizingStart,
        filesDownloaded: undefined,
        filesTotal: undefined,
        remainingSeconds: undefined,
      });

      await this._installSnapshot(dbDirectory);

      this._updateStatus({
        status: 'finalizing',
        progress: STEP_PROGRESS.finalizingProcessing,
        filesDownloaded: undefined,
        filesTotal: undefined,
        remainingSeconds: undefined,
      });

      await this._cleanupSnapshotArtifacts({ preserveDb: true });
      await this.clearLockFile();

      this._updateStatus({
        status: 'finalizing',
        progress: STEP_PROGRESS.finalizingEnd,
        filesDownloaded: undefined,
        filesTotal: undefined,
        remainingSeconds: undefined,
      });

      this._updateStatus({
        status: 'completed',
        progress: STEP_PROGRESS.completed,
        filesDownloaded: undefined,
        filesTotal: undefined,
        remainingSeconds: undefined,
      });
    } catch (error) {
      await this._cleanupSnapshotArtifacts({ preserveDb: false });
      this._updateStatus({
        status: 'failed',
        filesDownloaded: undefined,
        filesTotal: undefined,
        remainingSeconds: undefined,
        error: this._buildError(
          error,
          this._inferErrorStageFromStatus(this._status.status)
        ),
      });
      throw error;
    }
  }

  async cancel(): Promise<void> {
    this._updateStatus({
      status: 'cancelled',
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

  _resolveNetworkConfig(): MithrilNetworkConfig {
    const network = String(environment.network);
    const config = MITHRIL_NETWORK_CONFIG[network];
    if (!config) {
      throw new Error(`Mithril not supported for network: ${network}`);
    }
    return config;
  }

  async _fetchText(url: string): Promise<string> {
    return new Promise((resolve, reject) => {
      const request = https.request(url, (response) => {
        const { statusCode } = response;
        if (!statusCode || statusCode < 200 || statusCode >= 300) {
          response.resume();
          reject(new Error(`Request failed with status ${statusCode}`));
          return;
        }

        let data = '';
        response.on('data', (chunk) => {
          data += chunk.toString();
        });
        response.on('end', () => resolve(data.trim()));
      });

      request.on('error', reject);
      request.end();
    });
  }

  _normalizeVerificationKey(key: string): string {
    const trimmed = key.trim();
    if (/^[0-9a-fA-F]+$/.test(trimmed) && trimmed.length % 2 === 0) {
      return trimmed;
    }

    if (trimmed.startsWith('[')) {
      try {
        const bytes = JSON.parse(trimmed);
        if (Array.isArray(bytes)) {
          return bytes
            .map((value) => {
              const byte = Number(value);
              if (!Number.isFinite(byte) || byte < 0 || byte > 255) {
                throw new Error('Invalid byte value');
              }
              return byte.toString(16).padStart(2, '0');
            })
            .join('');
        }
      } catch (error) {
        logger.warn(
          'MithrilBootstrapService: failed to parse verification key',
          {
            error,
          }
        );
      }
    }

    return trimmed;
  }

  async _resolveVerificationKeys(config: MithrilNetworkConfig): Promise<void> {
    if (!config.genesisKey) {
      const raw = await this._fetchText(config.genesisKeyUrl);
      config.genesisKey = this._normalizeVerificationKey(raw);
    }
    if (!config.ancillaryKey) {
      const raw = await this._fetchText(config.ancillaryKeyUrl);
      config.ancillaryKey = this._normalizeVerificationKey(raw);
    }
  }

  async _buildMithrilEnv(requireKeys: boolean): Promise<NodeJS.ProcessEnv> {
    const config = this._resolveNetworkConfig();
    if (requireKeys) {
      await this._resolveVerificationKeys(config);
    }
    const env = ({
      ...process.env,
      AGGREGATOR_ENDPOINT: config.aggregatorEndpoint,
    } as unknown) as NodeJS.ProcessEnv;

    if (requireKeys && config.genesisKey && config.ancillaryKey) {
      env.GENESIS_VERIFICATION_KEY = config.genesisKey;
      env.ANCILLARY_VERIFICATION_KEY = config.ancillaryKey;
    } else {
      delete env.GENESIS_VERIFICATION_KEY;
      delete env.ANCILLARY_VERIFICATION_KEY;
    }

    return env;
  }

  _openLogStream(): WriteStream {
    const logsDir = path.join(stateDirectoryPath, 'Logs');
    ensureDirectoryExists(logsDir);
    const logPath = path.join(logsDir, 'mithril-bootstrap.log');
    return fs.createWriteStream(logPath, { flags: 'a' });
  }

  _buildError(
    error: unknown,
    fallbackStage?: MithrilBootstrapErrorStage
  ): MithrilBootstrapError {
    if (error instanceof MithrilBootstrapStageError) {
      return {
        message: error.message,
        code: error.code,
        stage: error.stage,
      };
    }

    if (error instanceof Error) {
      return {
        message: error.message,
        stage: fallbackStage,
      };
    }

    return {
      message: 'Mithril bootstrap failed',
      stage: fallbackStage,
    };
  }

  _createStageError(
    stage: MithrilBootstrapErrorStage,
    message: string,
    code?: string
  ): MithrilBootstrapStageError {
    return new MithrilBootstrapStageError(message, stage, code);
  }

  _inferErrorStageFromStatus(
    status: MithrilBootstrapStatusUpdate['status']
  ): MithrilBootstrapErrorStage | undefined {
    switch (status) {
      case 'downloading':
        return 'download';
      case 'unpacking':
      case 'converting':
      case 'finalizing':
        return 'convert';
      default:
        return undefined;
    }
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

  _attachLogStream(child: ChildProcess, logStream: WriteStream) {
    if (child.stdout) {
      child.stdout.on('data', (chunk) => logStream.write(chunk));
    }
    if (child.stderr) {
      child.stderr.on('data', (chunk) => logStream.write(chunk));
    }
  }

  async _runCommand(
    args: Array<string>,
    options: RunCommandOptions = {}
  ): Promise<RunCommandResult> {
    const { onStdout, onStderr, requireKeys = true } = options;
    const logStream = this._openLogStream();
    this._logStream = logStream;

    const env = await this._buildMithrilEnv(requireKeys);

    return new Promise((resolve, reject) => {
      const child = spawn('mithril-client', args, {
        cwd: this._workDir,
        env,
      });

      this._currentProcess = child;
      this._attachLogStream(child, logStream);

      let stdout = '';
      let stderr = '';

      if (child.stdout) {
        child.stdout.on('data', (chunk) => {
          const text = chunk.toString();
          stdout += text;
          if (onStdout) onStdout(text);
        });
      }

      if (child.stderr) {
        child.stderr.on('data', (chunk) => {
          const text = chunk.toString();
          stderr += text;
          if (onStderr) onStderr(text);
        });
      }

      child.on('error', (error) => {
        this._currentProcess = null;
        logStream.end();
        reject(error);
      });

      child.on('close', (exitCode) => {
        this._currentProcess = null;
        logStream.end();
        resolve({ stdout, stderr, exitCode });
      });
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
    this._updateStatus({
      status: 'downloading',
      progress: STEP_PROGRESS.downloadingStart,
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
      const update = parseMithrilProgressUpdate(line);
      if (!update) return;
      const { progress } = update;
      if (progress != null && !Number.isNaN(progress)) {
        const mapped =
          STEP_PROGRESS.downloadingStart +
          ((STEP_PROGRESS.downloadingEnd - STEP_PROGRESS.downloadingStart) *
            Math.min(Math.max(progress, 0), 100)) /
            100;
        this._updateStatus({
          status: 'downloading',
          progress: mapped,
          snapshot: snapshot || undefined,
          filesDownloaded: update.filesDownloaded,
          filesTotal: update.filesTotal,
          elapsedSeconds: update.elapsedSeconds,
          remainingSeconds: update.remainingSeconds,
        });
        return;
      }

      if (update.elapsedSeconds != null || update.remainingSeconds != null) {
        this._updateStatus({
          status: 'downloading',
          snapshot: snapshot || undefined,
          filesDownloaded: update.filesDownloaded,
          filesTotal: update.filesTotal,
          elapsedSeconds: update.elapsedSeconds,
          remainingSeconds: update.remainingSeconds,
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
      const errorCode = stderr ? stderr.trim().slice(0, 200) : undefined;
      throw this._createStageError(
        'download',
        `Mithril download failed with exit code ${exitCode}`,
        errorCode
      );
    }

    this._updateStatus({
      status: 'downloading',
      progress: STEP_PROGRESS.downloadingEnd,
      snapshot: snapshot || undefined,
    });
  }

  async _convertSnapshot(snapshot: MithrilSnapshotItem | null): Promise<void> {
    this._updateStatus({
      status: 'converting',
      progress: STEP_PROGRESS.finalizingProcessing,
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
      const errorCode = stderr ? stderr.trim().slice(0, 200) : undefined;
      throw this._createStageError(
        'convert',
        `Mithril snapshot conversion failed with exit code ${exitCode}`,
        errorCode
      );
    }

    this._updateStatus({
      status: 'finalizing',
      progress: STEP_PROGRESS.finalizingEnd,
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
