import path from 'path';
import fs from 'fs-extra';
import { stateDirectoryPath } from '../config';
import {
  ChainStorageConfig,
  ChainStorageValidation,
} from '../../common/types/mithril-bootstrap.types';
import { logger } from './logging';
import { isSamePath } from './chainStorageValidation';
import {
  getManagedChainPath,
  resolveStateDirectoryPath,
  resolveChainStoragePath as resolveChainStoragePathFn,
  resolveMithrilWorkDir as resolveMithrilWorkDirFn,
} from './chainStoragePathResolver';
import type { ResolvedStateDirectory } from './chainStoragePathResolver';
import {
  getConfig as getChainStorageConfig,
  getDefaultStorageConfig,
  validate as validateChainStorageConfig,
} from './chainStorageManagerConfig';
import {
  adoptManagedLayout,
  assertNodeStopped,
  detectLayout,
  emptyManagedContents as emptyManagedChainContents,
  ensureManagedChainLayout as ensureManagedChainLayoutHelper,
  installSnapshot as installChainSnapshot,
  migrateLegacyCustomLayout,
  preflightLegacyMigration,
  readMigrationJournal,
  recoverInterruptedMigration,
  rollbackMigrationJournal,
  writeMigrationJournal,
} from './chainStorageManagerLayout';
import {
  captureChainPathState,
  CHAIN_DIRECTORY_NAME,
  CHAIN_STORAGE_MIGRATION_JOURNAL_FILE,
  createSymlink,
  ensureDefaultChainDirectory,
  getEntriesSizeBytes,
  getPathSizeBytes,
  listLegacyManagedEntries,
  movePath,
  pathExistsViaLstat,
  replaceCustomChainEntryPoint,
  resetToDefault,
  resolveExistingDirectory,
  resolveRealPathOrInput,
  rollbackSetDirectory,
  safeLstat,
  safeReadDir,
} from './chainStorageManagerShared';
import type {
  ChainPathState,
  ChainStorageDefaults,
  ChainStorageLayout,
  ChainStorageMigrationJournal,
  EmptyManagedContentsOptions,
  EnsureManagedLayoutOptions,
  ManagedChainLayoutResult,
} from './chainStorageManagerShared';

export { CHAIN_DIRECTORY_NAME };

export class ChainStorageManager {
  _stateDirectoryPath: string;
  _chainPath: string;
  _logsDirectoryPath: string;
  _migrationJournalPath: string;
  _isRecoveryFallback = false;
  _mutationQueue: Promise<void> = Promise.resolve();

  constructor(daedalusStateDirectoryPath: string = stateDirectoryPath) {
    this._stateDirectoryPath = daedalusStateDirectoryPath;
    this._chainPath = path.join(this._stateDirectoryPath, CHAIN_DIRECTORY_NAME);
    this._logsDirectoryPath = path.join(this._stateDirectoryPath, 'Logs');
    this._migrationJournalPath = path.join(
      this._logsDirectoryPath,
      CHAIN_STORAGE_MIGRATION_JOURNAL_FILE
    );
  }

  async setDirectory(
    targetDir: string | null
  ): Promise<ChainStorageValidation> {
    return this._withMutationLock('setDirectory', async () => {
      const normalizedTargetDir =
        typeof targetDir === 'string' && targetDir.trim().length > 0
          ? targetDir.trim()
          : null;

      if (normalizedTargetDir == null) {
        const validation = await this._resetToDefault();
        this._isRecoveryFallback = false;
        return validation;
      }

      const validation = await this.validate(normalizedTargetDir);
      if (!validation.isValid) {
        return validation;
      }

      if (validation.path == null) {
        const validation = await this._resetToDefault();
        this._isRecoveryFallback = false;
        return validation;
      }

      const previousConfig = await this.getConfig();

      const nextManagedChainPath = this._getManagedChainPath(validation.path);
      const resolvedTargetPath = await this._resolveRealPathOrInput(
        nextManagedChainPath
      );
      const previousState = await this._captureChainPathState();

      if (
        previousConfig.customPath == null &&
        previousState.type === 'directory'
      ) {
        const entries = await this._safeReadDir(this._chainPath);
        if (entries.length > 0) {
          return {
            isValid: false,
            path: validation.path,
            resolvedPath: validation.resolvedPath,
            chainSubdirectoryStatus: validation.chainSubdirectoryStatus,
            reason: 'unknown',
            message:
              'Existing blockchain data must be cleared before switching storage locations.',
          };
        }
      }

      try {
        await fs.ensureDir(resolvedTargetPath);
        await this._replaceCustomChainEntryPoint(resolvedTargetPath);
        this._isRecoveryFallback = false;
      } catch (error) {
        await this._rollbackSetDirectory({
          previousState,
          targetPath: resolvedTargetPath,
        });
        throw error;
      }

      return {
        ...validation,
        resolvedPath: validation.resolvedPath,
      };
    });
  }

  async resetToDefault(): Promise<ChainStorageValidation> {
    return this._withMutationLock('resetToDefault', async () => {
      const validation = await this._resetToDefault();
      this._isRecoveryFallback = false;
      return validation;
    });
  }

  async ensureManagedChainLayout(
    options: EnsureManagedLayoutOptions = {}
  ): Promise<ManagedChainLayoutResult> {
    return this._withMutationLock('ensureManagedChainLayout', async () => {
      const layoutResult = await this._ensureManagedChainLayout(options);
      this._isRecoveryFallback =
        this._isRecoveryFallback || layoutResult.isRecoveryFallback;
      return layoutResult;
    });
  }

  async resolveChainStoragePath(): Promise<string> {
    return resolveChainStoragePathFn(this._stateDirectoryPath);
  }

  async resolveMithrilWorkDir(): Promise<string> {
    return resolveMithrilWorkDirFn(this._stateDirectoryPath);
  }

  async resolveDiskSpaceCheckPath(): Promise<string> {
    const config = await this.getConfig();
    const managedChainPath = this._getManagedChainPath(config.customPath);
    const managedChainExists = await fs.pathExists(managedChainPath);

    return managedChainExists
      ? managedChainPath
      : path.dirname(managedChainPath);
  }

  async getManagedChainPath(): Promise<string> {
    const config = await this.getConfig();
    return this._getManagedChainPath(config.customPath);
  }

  async getResolvedManagedChainPath(): Promise<string> {
    return this._resolveRealPathOrInput(await this.getManagedChainPath());
  }

  async getManagedParentPath(): Promise<string> {
    const config = await this.getConfig();
    return config.customPath
      ? path.resolve(config.customPath)
      : this._stateDirectoryPath;
  }

  async isManagedChainEmpty(): Promise<boolean> {
    const managedChainPath = await this.getManagedChainPath();
    const managedChainExists = await fs.pathExists(managedChainPath);

    if (!managedChainExists) {
      return true;
    }

    const entries = await this._safeReadDir(managedChainPath);
    return entries.length === 0;
  }

  async prepareForLocationChange(): Promise<ChainStorageValidation | null> {
    return this._withMutationLock('prepareForLocationChange', async () => {
      const config = await this.getConfig();

      if (config.customPath == null) {
        return null;
      }

      const managedChainPath = this._getManagedChainPath(config.customPath);
      const managedEntries = await this._safeReadDir(managedChainPath);

      if (managedEntries.length > 0) {
        return null;
      }

      const validation = await this._resetToDefault();

      if (await this._pathExistsViaLstat(managedChainPath)) {
        await fs.remove(managedChainPath);
      }

      return validation;
    });
  }

  async unlinkChainEntryPoint(): Promise<void> {
    return this._withMutationLock('unlinkChainEntryPoint', async () => {
      const chainState = await this._captureChainPathState();
      if (chainState.type === 'missing') {
        return;
      }

      await fs.remove(this._chainPath);
    });
  }

  async removeManagedDirectory(): Promise<void> {
    return this._withMutationLock('removeManagedDirectory', async () => {
      const managedChainPath = await this.getManagedChainPath();
      if (await this._pathExistsViaLstat(managedChainPath)) {
        await fs.remove(managedChainPath);
      }
    });
  }

  async emptyManagedContents(
    options: EmptyManagedContentsOptions = {}
  ): Promise<void> {
    return this._withMutationLock('emptyManagedContents', async () => {
      const managedChainPath = await this.getManagedChainPath();
      await this._emptyManagedContents(managedChainPath, options);
    });
  }

  async installSnapshot(dbDirectory: string): Promise<void> {
    return this._withMutationLock('installSnapshot', async () => {
      await installChainSnapshot(this, dbDirectory);
    });
  }

  async migrateData(
    fromPath: string,
    toPath: string,
    options: {
      preserveSourceRoot?: boolean;
    } = {}
  ): Promise<void> {
    const source = path.resolve(fromPath);
    const target = path.resolve(toPath);
    const { preserveSourceRoot = false } = options;

    if (source === target) {
      return;
    }

    const sourceStats = await this._safeLstat(source);
    if (!sourceStats) {
      return;
    }

    if (!sourceStats.isDirectory()) {
      throw new Error('Chain storage source must be a directory.');
    }

    await fs.ensureDir(target);
    const entries = await this._safeReadDir(source);

    for (const entry of entries) {
      await this._movePath(path.join(source, entry), path.join(target, entry));
    }

    if (!preserveSourceRoot) {
      await fs.remove(source);
    }
  }

  async _getDefaultStorageConfig(): Promise<ChainStorageDefaults> {
    return getDefaultStorageConfig(this);
  }

  async getConfig(): Promise<ChainStorageConfig> {
    return getChainStorageConfig(this);
  }

  async validate(targetDir: string | null): Promise<ChainStorageValidation> {
    return validateChainStorageConfig(this, targetDir);
  }

  _isSamePath(firstPath: string, secondPath: string): boolean {
    return isSamePath(firstPath, secondPath);
  }

  async _resolveStateDirectoryPath(): Promise<ResolvedStateDirectory> {
    return resolveStateDirectoryPath(this._stateDirectoryPath);
  }

  async _captureChainPathState(): Promise<ChainPathState> {
    return captureChainPathState(this);
  }

  async _rollbackSetDirectory({
    previousState,
    targetPath,
  }: {
    previousState: ChainPathState;
    targetPath: string;
  }): Promise<void> {
    return rollbackSetDirectory(this, {
      previousState,
      targetPath,
    });
  }

  async _resetToDefault(): Promise<ChainStorageValidation> {
    return resetToDefault(this);
  }

  async _ensureDefaultChainDirectory(): Promise<void> {
    return ensureDefaultChainDirectory(this);
  }

  async _ensureManagedChainLayout(
    options: EnsureManagedLayoutOptions = {}
  ): Promise<ManagedChainLayoutResult> {
    return ensureManagedChainLayoutHelper(this, options);
  }

  async _detectLayout(customPath: string): Promise<ChainStorageLayout> {
    return detectLayout(this, customPath);
  }

  async _adoptManagedLayout(layout: ChainStorageLayout): Promise<void> {
    return adoptManagedLayout(this, layout);
  }

  async _migrateLegacyCustomLayout(layout: ChainStorageLayout): Promise<void> {
    return migrateLegacyCustomLayout(this, layout);
  }

  async _recoverInterruptedMigration(
    options: EnsureManagedLayoutOptions
  ): Promise<void> {
    return recoverInterruptedMigration(this, options);
  }

  async _rollbackMigrationJournal(
    journal: ChainStorageMigrationJournal
  ): Promise<void> {
    return rollbackMigrationJournal(this, journal);
  }

  async _preflightLegacyMigration({
    legacyRootPath,
    managedChainPath,
    managedEntries,
  }: {
    legacyRootPath: string;
    managedChainPath: string;
    managedEntries: string[];
  }): Promise<void> {
    return preflightLegacyMigration(this, {
      legacyRootPath,
      managedChainPath,
      managedEntries,
    });
  }

  async _writeMigrationJournal(
    journal: ChainStorageMigrationJournal
  ): Promise<void> {
    return writeMigrationJournal(this, journal);
  }

  async _readMigrationJournal(): Promise<ChainStorageMigrationJournal | null> {
    return readMigrationJournal(this);
  }

  async _assertNodeStopped(
    nodeState: string | null | undefined,
    reason: string
  ): Promise<void> {
    return assertNodeStopped(this, nodeState, reason);
  }

  async _emptyManagedContents(
    managedChainPath: string,
    options: EmptyManagedContentsOptions = {}
  ): Promise<void> {
    return emptyManagedChainContents(this, managedChainPath, options);
  }

  async _replaceCustomChainEntryPoint(targetPath: string): Promise<void> {
    return replaceCustomChainEntryPoint(this, targetPath);
  }

  async _createSymlink(targetPath: string, symlinkPath: string): Promise<void> {
    return createSymlink(this, targetPath, symlinkPath);
  }

  async _movePath(sourcePath: string, targetPath: string): Promise<void> {
    return movePath(this, sourcePath, targetPath);
  }

  async _safeReadDir(targetPath: string): Promise<string[]> {
    return safeReadDir(this, targetPath);
  }

  async _safeLstat(targetPath: string): Promise<fs.Stats | null> {
    return safeLstat(this, targetPath);
  }

  async _pathExistsViaLstat(targetPath: string): Promise<boolean> {
    return pathExistsViaLstat(this, targetPath);
  }

  async _resolveExistingDirectory(
    directoryPath: string
  ): Promise<string | undefined> {
    return resolveExistingDirectory(this, directoryPath);
  }

  async _resolveRealPathOrInput(targetPath: string): Promise<string> {
    return resolveRealPathOrInput(this, targetPath);
  }

  _getManagedChainPath(customPath: string | null): string {
    return getManagedChainPath(this._stateDirectoryPath, customPath);
  }

  async _listLegacyManagedEntries(
    legacyRootPath: string,
    managedChainPath: string
  ): Promise<{ managedEntries: string[]; ignoredEntries: string[] }> {
    return listLegacyManagedEntries(this, legacyRootPath, managedChainPath);
  }

  async _getEntriesSizeBytes(
    rootPath: string,
    entries: string[]
  ): Promise<number> {
    return getEntriesSizeBytes(this, rootPath, entries);
  }

  async _getPathSizeBytes(targetPath: string): Promise<number> {
    return getPathSizeBytes(this, targetPath);
  }

  async _withMutationLock<T>(
    label: string,
    operation: () => Promise<T>
  ): Promise<T> {
    const previousMutation = this._mutationQueue;
    let releaseLock: (() => void) | undefined;

    this._mutationQueue = new Promise<void>((resolve) => {
      releaseLock = resolve;
    });

    await previousMutation.catch(() => undefined);

    try {
      return await operation();
    } catch (error) {
      logger.warn('ChainStorageManager: serialized mutation failed', {
        error,
        label,
      });
      throw error;
    } finally {
      releaseLock?.();
    }
  }
}
