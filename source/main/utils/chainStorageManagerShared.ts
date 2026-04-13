import path from 'path';
import fs from 'fs-extra';
import type {
  ChainStorageConfig,
  ChainStorageValidation,
} from '../../common/types/mithril-bootstrap.types';
import { logger } from './logging';
import type { ResolvedStateDirectory } from './chainStoragePathResolver';

/** @deprecated Retained solely for legacy config cleanup on startup. See cleanupLegacyConfigIfPresent(). */
export const CHAIN_STORAGE_CONFIG_FILE = 'chain-storage-config.json';
export const CHAIN_DIRECTORY_NAME = 'chain';

export const CHAIN_STORAGE_MIGRATION_JOURNAL_FILE =
  'chain-storage-migration-journal.json';

export const LEGACY_MANAGED_CHAIN_ENTRIES = new Set([
  'clean',
  'data',
  'db',
  'immutable',
  'ledger',
  'volatile',
]);

export type ChainPathState = {
  type: 'missing' | 'directory' | 'symlink';
  resolvedPath?: string;
  linkTargetPath?: string;
};

export type ManagedChainLayoutResult = {
  managedChainPath: string;
  isRecoveryFallback: boolean;
};

export type ChainStorageLayoutKind =
  | 'default'
  | 'inconsistent'
  | 'legacy-custom-root'
  | 'managed-custom-root'
  | 'broken-link';

export type ChainStorageLayout = {
  kind: ChainStorageLayoutKind;
  customPath: string | null;
  resolvedCustomPath?: string;
  managedChainPath: string;
  resolvedManagedChainPath?: string;
  currentChainSource: string | null;
  entryPointState: ChainPathState;
  managedChainExists: boolean;
  managedChainIsDirectory: boolean;
  managedLegacyEntries: string[];
  ignoredLegacyEntries: string[];
};

export type ChainStorageMigrationJournalState =
  | 'start'
  | 'progress'
  | 'cutover'
  | 'rollback'
  | 'completion';

export type ChainStorageMigrationJournal = {
  state: ChainStorageMigrationJournalState;
  customPath: string;
  legacyRootPath: string;
  managedChainPath: string;
  movedEntries: string[];
  ignoredEntries: string[];
  backupEntryPointPath: string;
  tempEntryPointPath: string;
  createdAt: string;
  updatedAt: string;
};

export type EnsureManagedLayoutOptions = {
  nodeState?: string | null;
};

export type EmptyManagedContentsOptions = {
  excludeTopLevelEntries?: string[];
};

export type ChainStorageDefaults = Pick<
  ChainStorageConfig,
  'defaultPath' | 'availableSpaceBytes' | 'requiredSpaceBytes'
>;

export interface ChainStorageManagerContext {
  _stateDirectoryPath: string;
  _configPath: string;
  _chainPath: string;
  _logsDirectoryPath: string;
  _migrationJournalPath: string;
  _isRecoveryFallback: boolean;
  getConfig(): Promise<ChainStorageConfig>;
  getManagedChainPath(): Promise<string>;
  _getDefaultStorageConfig(): Promise<ChainStorageDefaults>;
  _resolveStateDirectoryPath(): Promise<ResolvedStateDirectory>;
  _isSamePath(firstPath: string, secondPath: string): boolean;
  _captureChainPathState(): Promise<ChainPathState>;
  _resolveRealPathOrInput(targetPath: string): Promise<string>;
  _getManagedChainPath(customPath: string | null): string;
  _safeReadDir(targetPath: string): Promise<string[]>;
  _safeLstat(targetPath: string): Promise<fs.Stats | null>;
  _pathExistsViaLstat(targetPath: string): Promise<boolean>;
  _replaceCustomChainEntryPoint(targetPath: string): Promise<void>;
  _movePath(sourcePath: string, targetPath: string): Promise<void>;
  _createSymlink(targetPath: string, symlinkPath: string): Promise<void>;
  _resolveExistingDirectory(directoryPath: string): Promise<string | undefined>;
  _canonicalizeManagedChildSelection(
    targetDir: string,
    currentCustomPath: string | null
  ): Promise<string>;
  _listLegacyManagedEntries(
    legacyRootPath: string,
    managedChainPath: string
  ): Promise<{ managedEntries: string[]; ignoredEntries: string[] }>;
  _getEntriesSizeBytes(rootPath: string, entries: string[]): Promise<number>;
  _getPathSizeBytes(targetPath: string): Promise<number>;
  _rollbackMigrationJournal(
    journal: ChainStorageMigrationJournal
  ): Promise<void>;
  _recoverInterruptedMigration(
    options: EnsureManagedLayoutOptions
  ): Promise<void>;
  _detectLayout(customPath: string): Promise<ChainStorageLayout>;
  _adoptManagedLayout(layout: ChainStorageLayout): Promise<void>;
  _migrateLegacyCustomLayout(layout: ChainStorageLayout): Promise<void>;
  _assertNodeStopped(
    nodeState: string | null | undefined,
    reason: string
  ): Promise<void>;
  _ensureDefaultChainDirectory(): Promise<void>;
  _emptyManagedContents(
    managedChainPath: string,
    options?: EmptyManagedContentsOptions
  ): Promise<void>;
  _writeMigrationJournal(journal: ChainStorageMigrationJournal): Promise<void>;
  _readMigrationJournal(): Promise<ChainStorageMigrationJournal | null>;
  _preflightLegacyMigration(params: {
    legacyRootPath: string;
    managedChainPath: string;
    managedEntries: string[];
  }): Promise<void>;
  _ensureManagedChainLayout(
    options?: EnsureManagedLayoutOptions
  ): Promise<ManagedChainLayoutResult>;
}

export const toIsoString = () => new Date().toISOString();

export const isPathNotFoundError = (error: unknown): boolean => {
  const code = (error as NodeJS.ErrnoException)?.code;
  return code === 'ENOENT' || code === 'ENOTDIR';
};

export const isPathWithin = (
  parentPath: string,
  targetPath: string
): boolean => {
  const relativePath = path.relative(parentPath, targetPath);
  return (
    relativePath === '' ||
    (!relativePath.startsWith('..') && !path.isAbsolute(relativePath))
  );
};

export async function captureChainPathState(
  ctx: ChainStorageManagerContext
): Promise<ChainPathState> {
  const stats = await ctx._safeLstat(ctx._chainPath);
  if (!stats) {
    return { type: 'missing' };
  }

  if (stats.isSymbolicLink()) {
    let linkTargetPath: string | undefined;

    try {
      const rawLinkTarget = await fs.readlink(ctx._chainPath);
      linkTargetPath = path.isAbsolute(rawLinkTarget)
        ? rawLinkTarget
        : path.resolve(path.dirname(ctx._chainPath), rawLinkTarget);
    } catch (error) {
      if (!isPathNotFoundError(error)) {
        logger.warn(
          'ChainStorageManager: failed to read chain entry point target',
          {
            error,
            chainPath: ctx._chainPath,
          }
        );
      }
    }

    try {
      return {
        type: 'symlink',
        linkTargetPath,
        resolvedPath: await fs.realpath(ctx._chainPath),
      };
    } catch (error) {
      logger.warn('ChainStorageManager: failed to snapshot symlink state', {
        error,
        chainPath: ctx._chainPath,
      });
      return { type: 'symlink', linkTargetPath };
    }
  }

  if (process.platform === 'win32' && stats.isDirectory()) {
    try {
      const rawLinkTarget = await fs.readlink(ctx._chainPath);
      const linkTargetPath = path.isAbsolute(rawLinkTarget)
        ? rawLinkTarget
        : path.resolve(path.dirname(ctx._chainPath), rawLinkTarget);

      try {
        return {
          type: 'symlink',
          linkTargetPath,
          resolvedPath: await fs.realpath(ctx._chainPath),
        };
      } catch (error) {
        logger.warn(
          'ChainStorageManager: failed to snapshot Windows junction state',
          {
            error,
            chainPath: ctx._chainPath,
          }
        );
        return { type: 'symlink', linkTargetPath };
      }
    } catch (error) {
      const code = (error as NodeJS.ErrnoException)?.code;
      if (
        !isPathNotFoundError(error) &&
        code !== 'EINVAL' &&
        code !== 'UNKNOWN'
      ) {
        logger.warn(
          'ChainStorageManager: failed to inspect Windows junction target',
          {
            error,
            chainPath: ctx._chainPath,
          }
        );
      }
    }
  }

  if (stats.isDirectory()) {
    return {
      type: 'directory',
      resolvedPath: ctx._chainPath,
    };
  }

  return { type: 'missing' };
}

export async function rollbackSetDirectory(
  ctx: ChainStorageManagerContext,
  {
    previousState,
    targetPath,
  }: {
    previousState: ChainPathState;
    targetPath: string;
  }
): Promise<void> {
  logger.warn('ChainStorageManager: rolling back setDirectory change', {
    chainPath: ctx._chainPath,
    targetPath,
    previousState: previousState.type,
  });

  try {
    switch (previousState.type) {
      case 'symlink': {
        const rollbackTarget = previousState.resolvedPath;
        if (rollbackTarget) {
          await fs.ensureDir(rollbackTarget);
          await ctx._replaceCustomChainEntryPoint(rollbackTarget);
        }
        break;
      }

      case 'directory': {
        await fs.remove(ctx._chainPath);
        await fs.ensureDir(ctx._chainPath);
        break;
      }

      case 'missing':
      default: {
        await fs.remove(ctx._chainPath);
        break;
      }
    }
  } catch (rollbackError) {
    logger.error('ChainStorageManager: failed to rollback setDirectory', {
      rollbackError,
    });
  }
}

export async function resetToDefault(
  ctx: ChainStorageManagerContext
): Promise<ChainStorageValidation> {
  await fs.remove(ctx._chainPath);
  await fs.ensureDir(ctx._chainPath);

  const defaultStorageConfig = await ctx._getDefaultStorageConfig();

  return {
    isValid: true,
    path: null,
    resolvedPath: defaultStorageConfig.defaultPath,
    availableSpaceBytes: defaultStorageConfig.availableSpaceBytes,
    requiredSpaceBytes: defaultStorageConfig.requiredSpaceBytes,
  };
}

export async function ensureDefaultChainDirectory(
  ctx: ChainStorageManagerContext
): Promise<void> {
  const chainState = await ctx._captureChainPathState();
  if (chainState.type === 'directory') {
    return;
  }

  await fs.remove(ctx._chainPath);
  await fs.ensureDir(ctx._chainPath);
}

export async function replaceCustomChainEntryPoint(
  ctx: ChainStorageManagerContext,
  targetPath: string
): Promise<void> {
  await fs.ensureDir(targetPath);
  await fs.remove(ctx._chainPath);
  await ctx._createSymlink(targetPath, ctx._chainPath);
}

export async function createSymlink(
  _ctx: ChainStorageManagerContext,
  targetPath: string,
  symlinkPath: string
): Promise<void> {
  await fs.symlink(
    targetPath,
    symlinkPath,
    process.platform === 'win32' ? 'junction' : 'dir'
  );
}

export async function movePath(
  _ctx: ChainStorageManagerContext,
  sourcePath: string,
  targetPath: string
): Promise<void> {
  try {
    await fs.move(sourcePath, targetPath, { overwrite: true });
  } catch (error) {
    if ((error as NodeJS.ErrnoException)?.code !== 'EXDEV') {
      throw error;
    }

    await fs.copy(sourcePath, targetPath, { overwrite: true });
    await fs.remove(sourcePath);
  }
}

export async function safeReadDir(
  _ctx: ChainStorageManagerContext,
  targetPath: string
): Promise<string[]> {
  try {
    return await fs.readdir(targetPath);
  } catch (error) {
    if (isPathNotFoundError(error)) {
      return [];
    }
    throw error;
  }
}

export async function safeLstat(
  _ctx: ChainStorageManagerContext,
  targetPath: string
): Promise<fs.Stats | null> {
  try {
    return await fs.lstat(targetPath);
  } catch (error) {
    if (isPathNotFoundError(error)) {
      return null;
    }
    throw error;
  }
}

export async function pathExistsViaLstat(
  ctx: ChainStorageManagerContext,
  targetPath: string
): Promise<boolean> {
  return Boolean(await ctx._safeLstat(targetPath));
}

export async function resolveExistingDirectory(
  _ctx: ChainStorageManagerContext,
  directoryPath: string
): Promise<string | undefined> {
  try {
    const stats = await fs.stat(directoryPath);
    if (!stats.isDirectory()) {
      return undefined;
    }
    return await fs.realpath(directoryPath);
  } catch (error) {
    if (isPathNotFoundError(error)) {
      return undefined;
    }
    logger.warn('ChainStorageManager: failed to resolve existing directory', {
      directoryPath,
      error,
    });
    return undefined;
  }
}

export async function resolveRealPathOrInput(
  _ctx: ChainStorageManagerContext,
  targetPath: string
): Promise<string> {
  try {
    return await fs.realpath(targetPath);
  } catch (error) {
    if (isPathNotFoundError(error)) {
      return path.resolve(targetPath);
    }
    throw error;
  }
}

export async function canonicalizeManagedChildSelection(
  ctx: ChainStorageManagerContext,
  targetDir: string,
  currentCustomPath: string | null
): Promise<string> {
  if (!currentCustomPath) {
    return targetDir;
  }

  const resolvedTargetPath = await ctx._resolveRealPathOrInput(targetDir);
  const resolvedManagedChainPath = await ctx._resolveRealPathOrInput(
    ctx._getManagedChainPath(currentCustomPath)
  );

  return ctx._isSamePath(resolvedTargetPath, resolvedManagedChainPath)
    ? currentCustomPath
    : targetDir;
}

export async function listLegacyManagedEntries(
  ctx: ChainStorageManagerContext,
  legacyRootPath: string,
  managedChainPath: string
): Promise<{ managedEntries: string[]; ignoredEntries: string[] }> {
  const legacyRootStats = await ctx._safeLstat(legacyRootPath);
  if (!legacyRootStats || !legacyRootStats.isDirectory()) {
    return { managedEntries: [], ignoredEntries: [] };
  }

  const entries = await ctx._safeReadDir(legacyRootPath);
  const managedEntries: string[] = [];
  const ignoredEntries: string[] = [];

  for (const entry of entries) {
    const entryPath = path.join(legacyRootPath, entry);
    if (!ctx._isSamePath(entryPath, managedChainPath)) {
      if (LEGACY_MANAGED_CHAIN_ENTRIES.has(entry)) {
        managedEntries.push(entry);
      } else {
        ignoredEntries.push(entry);
      }
    }
  }

  return { managedEntries, ignoredEntries };
}

export async function getEntriesSizeBytes(
  ctx: ChainStorageManagerContext,
  rootPath: string,
  entries: string[]
): Promise<number> {
  let totalSizeBytes = 0;

  for (const entry of entries) {
    totalSizeBytes += await ctx._getPathSizeBytes(path.join(rootPath, entry));
  }

  return totalSizeBytes;
}

export async function getPathSizeBytes(
  ctx: ChainStorageManagerContext,
  targetPath: string
): Promise<number> {
  const stats = await ctx._safeLstat(targetPath);
  if (!stats) {
    return 0;
  }

  if (!stats.isDirectory() || stats.isSymbolicLink()) {
    return stats.size;
  }

  const entries = await ctx._safeReadDir(targetPath);
  let totalSizeBytes = 0;

  for (const entry of entries) {
    totalSizeBytes += await ctx._getPathSizeBytes(path.join(targetPath, entry));
  }

  return totalSizeBytes;
}
