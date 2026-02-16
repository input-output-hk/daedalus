import path from 'path';
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
  MithrilBootstrapStatusUpdate,
  MithrilSnapshotItem,
} from '../../common/types/mithril-bootstrap.types';

type MithrilNetworkConfig = {
  aggregatorEndpoint: string;
  genesisKeyUrl: string;
  ancillaryKeyUrl: string;
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
  currentStep: undefined,
  snapshot: null,
  error: null,
};

const STEP_PROGRESS = {
  preparing: 5,
  downloadingStart: 10,
  downloadingEnd: 80,
  verifying: 85,
  converting: 95,
  completed: 100,
};

const normalizeSnapshotItem = (
  raw: Record<string, any>
): MithrilSnapshotItem => {
  const digest = raw.digest || raw.snapshot_digest || raw.hash || '';
  const createdAt = raw.created_at || raw.createdAt || raw.timestamp || '';
  const size = Number(raw.size ?? raw.total_size ?? raw.size_bytes ?? 0);
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
  _lockFilePath: string;
  _workDir: string;

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

  onStatus(
    listener: (update: MithrilBootstrapStatusUpdate) => void
  ): () => void {
    this._statusEmitter.on('status', listener);
    return () => this._statusEmitter.removeListener('status', listener);
  }

  async listSnapshots(): Promise<Array<MithrilSnapshotItem>> {
    const { stdout } = await this._runCommand([
      'cardano-db',
      'snapshot',
      'list',
      '--json',
    ]);
    const parsed = this._safeJsonParse(stdout);
    if (!Array.isArray(parsed)) return [];
    return parsed.map((item) => normalizeSnapshotItem(item));
  }

  async showSnapshot(digest: string): Promise<MithrilSnapshotItem | null> {
    if (!isNonEmptyString(digest)) return null;
    const { stdout } = await this._runCommand([
      'cardano-db',
      'snapshot',
      'show',
      digest,
      '--json',
    ]);
    const parsed = this._safeJsonParse(stdout);
    if (!parsed || typeof parsed !== 'object') return null;
    return normalizeSnapshotItem(parsed);
  }

  async startBootstrap(digest?: string): Promise<void> {
    if (this._currentProcess) {
      throw new Error('Mithril bootstrap already in progress');
    }

    await this._createLockFile();

    this._updateStatus({
      status: 'preparing',
      progress: STEP_PROGRESS.preparing,
      currentStep: 'Preparing Mithril bootstrap',
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
      await this._convertSnapshot(snapshot);

      this._updateStatus({
        status: 'completed',
        progress: STEP_PROGRESS.completed,
        currentStep: 'Mithril bootstrap completed',
      });
      await this._cleanupSnapshotArtifacts();
      await this._removeLockFile();
    } catch (error) {
      await this._cleanupSnapshotArtifacts();
      this._updateStatus({
        status: 'failed',
        error: this._buildError(error),
      });
      throw error;
    }
  }

  async cancel(): Promise<void> {
    if (!this._currentProcess) return;

    this._updateStatus({
      status: 'cancelled',
      currentStep: 'Cancelling Mithril bootstrap',
    });

    try {
      this._currentProcess.kill();
    } catch (error) {
      logger.warn('MithrilBootstrapService: failed to kill process', { error });
    }

    this._updateStatus({
      status: 'cancelled',
      currentStep: 'Mithril bootstrap cancelled',
    });

    await this._cleanupSnapshotArtifacts();
  }

  _updateStatus(update: Partial<MithrilBootstrapStatusUpdate>) {
    this._status = {
      ...this._status,
      ...update,
    };
    this._statusEmitter.emit('status', { ...this._status });
  }

  _resolveNetworkConfig(): MithrilNetworkConfig {
    const network = String(environment.network);
    const config = MITHRIL_NETWORK_CONFIG[network];
    if (!config) {
      throw new Error(`Mithril not supported for network: ${network}`);
    }
    return config;
  }

  _buildMithrilEnv(): NodeJS.ProcessEnv {
    const config = this._resolveNetworkConfig();
    return Object.assign({}, process.env, {
      AGGREGATOR_ENDPOINT: config.aggregatorEndpoint,
      GENESIS_VERIFICATION_KEY: config.genesisKeyUrl,
      ANCILLARY_VERIFICATION_KEY: config.ancillaryKeyUrl,
    });
  }

  _openLogStream(): WriteStream {
    const logsDir = path.join(stateDirectoryPath, 'Logs');
    ensureDirectoryExists(logsDir);
    const logPath = path.join(logsDir, 'mithril-bootstrap.log');
    return fs.createWriteStream(logPath, { flags: 'a' });
  }

  _buildError(error: unknown): MithrilBootstrapError {
    if (error instanceof Error) {
      return {
        message: error.message,
      };
    }

    return {
      message: 'Mithril bootstrap failed',
    };
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

  async _removeLockFile(): Promise<void> {
    try {
      await fs.remove(this._lockFilePath);
    } catch (error) {
      logger.warn('MithrilBootstrapService: failed to remove lock file', {
        error,
      });
    }
  }

  async _cleanupSnapshotArtifacts(): Promise<void> {
    try {
      const dataDir = path.join(this._workDir, 'data');
      if (await fs.pathExists(dataDir)) {
        await fs.remove(dataDir);
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
    const { onStdout, onStderr } = options;
    const logStream = this._openLogStream();
    this._logStream = logStream;

    return new Promise((resolve, reject) => {
      const child = spawn('mithril-client', args, {
        cwd: this._workDir,
        env: this._buildMithrilEnv(),
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
      logger.warn('MithrilBootstrapService: JSON parse failed', {
        error,
        payload: payload?.slice(0, 200),
      });
      return null;
    }
  }

  _parseProgressLine(line: string): number | null {
    const trimmed = line.trim();
    if (!trimmed.startsWith('{')) return null;

    try {
      const parsed = JSON.parse(trimmed);
      const progress = parsed.progress ?? parsed.percentage ?? parsed.percent;
      if (typeof progress === 'number') return progress;
      if (typeof progress === 'string') return Number(progress);
      return null;
    } catch (error) {
      return null;
    }
  }

  async _downloadSnapshot(
    digest: string,
    snapshot: MithrilSnapshotItem | null
  ): Promise<void> {
    this._updateStatus({
      status: 'downloading',
      progress: STEP_PROGRESS.downloadingStart,
      currentStep: 'Downloading Mithril snapshot',
    });

    let buffered = '';
    let sawJsonProgress = false;
    const { exitCode, stderr } = await this._runCommand(
      [
        'cardano-db',
        'snapshot',
        'download',
        '--include-ancillary',
        digest,
        '--json',
      ],
      {
        onStdout: (chunk) => {
          buffered += chunk;
          const lines = buffered.split('\n');
          buffered = lines.pop() || '';
          lines.forEach((line) => {
            const progress = this._parseProgressLine(line);
            if (progress != null && !Number.isNaN(progress)) {
              sawJsonProgress = true;
              const mapped =
                STEP_PROGRESS.downloadingStart +
                ((STEP_PROGRESS.downloadingEnd -
                  STEP_PROGRESS.downloadingStart) *
                  Math.min(Math.max(progress, 0), 100)) /
                  100;
              this._updateStatus({
                status: 'downloading',
                progress: mapped,
                currentStep: 'Downloading Mithril snapshot',
                snapshot: snapshot || undefined,
              });
            }
          });
        },
      }
    );

    if (exitCode !== 0) {
      const error: MithrilBootstrapError = {
        message: `Mithril download failed with exit code ${exitCode}`,
      };
      if (stderr) error.code = stderr.trim().slice(0, 200);
      this._updateStatus({ status: 'failed', error });
      throw new Error(error.message);
    }

    if (!sawJsonProgress) {
      this._updateStatus({
        status: 'downloading',
        progress: STEP_PROGRESS.downloadingEnd,
        currentStep: 'Downloading Mithril snapshot',
      });
    }

    this._updateStatus({
      status: 'verifying',
      progress: STEP_PROGRESS.verifying,
      currentStep: 'Verifying Mithril snapshot',
    });
  }

  async _convertSnapshot(snapshot: MithrilSnapshotItem | null): Promise<void> {
    this._updateStatus({
      status: 'converting',
      progress: STEP_PROGRESS.converting,
      currentStep: 'Converting ledger snapshot',
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
      const error: MithrilBootstrapError = {
        message: `Mithril snapshot conversion failed with exit code ${exitCode}`,
      };
      if (stderr) error.code = stderr.trim().slice(0, 200);
      this._updateStatus({ status: 'failed', error });
      throw new Error(error.message);
    }
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
}
