import path from 'path';
import fs from 'fs-extra';
import checkDiskSpace from 'check-disk-space';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import { logger } from './logging';
import type {
  ChainStorageLayout,
  ChainStorageManagerContext,
  ChainStorageMigrationJournal,
  EmptyManagedContentsOptions,
  EnsureManagedLayoutOptions,
  ManagedChainLayoutResult,
} from './chainStorageManagerShared';
import { isPathWithin, toIsoString } from './chainStorageManagerShared';

const toLayoutResult = (
  managedChainPath: string,
  isRecoveryFallback = false
): ManagedChainLayoutResult => ({
  managedChainPath,
  isRecoveryFallback,
});

const cleanupLegacyConfigIfPresent = async (
  ctx: ChainStorageManagerContext
): Promise<void> => {
  try {
    if (await fs.pathExists(ctx._configPath)) {
      await fs.remove(ctx._configPath);
    }
  } catch (error) {
    logger.warn(
      'ChainStorageManager: failed to remove legacy chain storage config',
      {
        error,
        configPath: ctx._configPath,
      }
    );
  }
};

export async function ensureManagedChainLayout(
  ctx: ChainStorageManagerContext,
  options: EnsureManagedLayoutOptions = {}
): Promise<ManagedChainLayoutResult> {
  await ctx._recoverInterruptedMigration(options);
  await cleanupLegacyConfigIfPresent(ctx);
  const entryPointState = await ctx._captureChainPathState();

  if (entryPointState.type !== 'symlink') {
    await ctx._ensureDefaultChainDirectory();
    return toLayoutResult(ctx._getManagedChainPath(null));
  }

  const currentChainTarget =
    entryPointState.resolvedPath || entryPointState.linkTargetPath;
  const customPath = currentChainTarget
    ? path.dirname(currentChainTarget)
    : null;

  if (!customPath) {
    await ctx._ensureDefaultChainDirectory();
    logger.warn(
      'ChainStorageManager: recovered missing custom chain target by falling back to default storage',
      {
        chainPath: ctx._chainPath,
      }
    );
    return toLayoutResult(ctx._getManagedChainPath(null), true);
  }

  const layout = await ctx._detectLayout(customPath);

  switch (layout.kind) {
    case 'managed-custom-root': {
      await ctx._adoptManagedLayout(layout);
      return toLayoutResult(layout.managedChainPath);
    }

    case 'inconsistent': {
      await ctx._adoptManagedLayout(layout);
      return toLayoutResult(layout.managedChainPath);
    }

    case 'legacy-custom-root': {
      await ctx._assertNodeStopped(
        options.nodeState,
        'migrate legacy chain storage layout'
      );
      await ctx._migrateLegacyCustomLayout(layout);
      return toLayoutResult(layout.managedChainPath);
    }

    case 'broken-link': {
      if (layout.managedChainIsDirectory) {
        await ctx._adoptManagedLayout(layout);
        return toLayoutResult(layout.managedChainPath);
      }

      if (layout.managedLegacyEntries.length > 0) {
        await ctx._assertNodeStopped(
          options.nodeState,
          'recover legacy chain storage layout'
        );
        await ctx._migrateLegacyCustomLayout(layout);
        return toLayoutResult(layout.managedChainPath);
      }

      await ctx._ensureDefaultChainDirectory();
      logger.warn(
        'ChainStorageManager: fell back to default storage after unrecoverable custom chain target failure',
        {
          chainPath: ctx._chainPath,
          customPath: layout.customPath,
          managedChainPath: layout.managedChainPath,
        }
      );
      return toLayoutResult(ctx._getManagedChainPath(null), true);
    }

    case 'default':
    default: {
      await ctx._ensureDefaultChainDirectory();
      return toLayoutResult(ctx._getManagedChainPath(null));
    }
  }
}

export async function detectLayout(
  ctx: ChainStorageManagerContext,
  customPath: string
): Promise<ChainStorageLayout> {
  const resolvedCustomPath = await ctx._resolveExistingDirectory(customPath);
  const normalizedCustomPath = resolvedCustomPath || path.resolve(customPath);
  const managedChainPath = ctx._getManagedChainPath(normalizedCustomPath);
  const managedChainStats = await ctx._safeLstat(managedChainPath);
  const managedChainExists = Boolean(managedChainStats);
  const managedChainIsDirectory = managedChainStats?.isDirectory() ?? false;
  const resolvedManagedChainPath = managedChainIsDirectory
    ? await ctx._resolveRealPathOrInput(managedChainPath)
    : undefined;
  const entryPointState = await ctx._captureChainPathState();
  const currentChainSource =
    entryPointState.type === 'directory'
      ? ctx._chainPath
      : entryPointState.resolvedPath || null;
  const {
    managedEntries,
    ignoredEntries,
  } = await ctx._listLegacyManagedEntries(
    normalizedCustomPath,
    managedChainPath
  );

  if (currentChainSource) {
    if (ctx._isSamePath(currentChainSource, managedChainPath)) {
      return {
        kind: 'managed-custom-root',
        customPath,
        resolvedCustomPath: normalizedCustomPath,
        managedChainPath,
        resolvedManagedChainPath,
        currentChainSource,
        entryPointState,
        managedChainExists,
        managedChainIsDirectory,
        managedLegacyEntries: managedEntries,
        ignoredLegacyEntries: ignoredEntries,
      };
    }

    if (ctx._isSamePath(currentChainSource, normalizedCustomPath)) {
      return {
        kind: 'legacy-custom-root',
        customPath,
        resolvedCustomPath: normalizedCustomPath,
        managedChainPath,
        resolvedManagedChainPath,
        currentChainSource,
        entryPointState,
        managedChainExists,
        managedChainIsDirectory,
        managedLegacyEntries: managedEntries,
        ignoredLegacyEntries: ignoredEntries,
      };
    }
  }

  if (managedChainIsDirectory) {
    if (entryPointState.type === 'directory') {
      return {
        kind: 'inconsistent',
        customPath,
        resolvedCustomPath: normalizedCustomPath,
        managedChainPath,
        resolvedManagedChainPath,
        currentChainSource,
        entryPointState,
        managedChainExists,
        managedChainIsDirectory,
        managedLegacyEntries: managedEntries,
        ignoredLegacyEntries: ignoredEntries,
      };
    }

    return {
      kind: 'managed-custom-root',
      customPath,
      resolvedCustomPath: normalizedCustomPath,
      managedChainPath,
      resolvedManagedChainPath,
      currentChainSource,
      entryPointState,
      managedChainExists,
      managedChainIsDirectory,
      managedLegacyEntries: managedEntries,
      ignoredLegacyEntries: ignoredEntries,
    };
  }

  if (managedEntries.length > 0) {
    return {
      kind: 'legacy-custom-root',
      customPath,
      resolvedCustomPath: normalizedCustomPath,
      managedChainPath,
      resolvedManagedChainPath,
      currentChainSource,
      entryPointState,
      managedChainExists,
      managedChainIsDirectory,
      managedLegacyEntries: managedEntries,
      ignoredLegacyEntries: ignoredEntries,
    };
  }

  return {
    kind: 'broken-link',
    customPath,
    resolvedCustomPath: normalizedCustomPath,
    managedChainPath,
    resolvedManagedChainPath,
    currentChainSource,
    entryPointState,
    managedChainExists,
    managedChainIsDirectory,
    managedLegacyEntries: managedEntries,
    ignoredLegacyEntries: ignoredEntries,
  };
}

export async function adoptManagedLayout(
  ctx: ChainStorageManagerContext,
  layout: ChainStorageLayout
): Promise<void> {
  await fs.ensureDir(layout.managedChainPath);

  if (
    layout.entryPointState.type === 'symlink' &&
    layout.currentChainSource &&
    ctx._isSamePath(layout.currentChainSource, layout.managedChainPath)
  ) {
    return;
  }

  if (layout.entryPointState.type === 'directory') {
    const localEntries = await ctx._safeReadDir(ctx._chainPath);

    if (localEntries.length > 0) {
      throw new Error(
        'Configured chain storage entry point is a non-empty local directory and could not be safely replaced automatically.'
      );
    }
  }

  await ctx._replaceCustomChainEntryPoint(layout.managedChainPath);
}

export async function migrateLegacyCustomLayout(
  ctx: ChainStorageManagerContext,
  layout: ChainStorageLayout
): Promise<void> {
  const legacyRootPath =
    layout.resolvedCustomPath || path.resolve(layout.customPath || '');
  const managedChainPath = layout.managedChainPath;

  await ctx._preflightLegacyMigration({
    legacyRootPath,
    managedChainPath,
    managedEntries: layout.managedLegacyEntries,
  });

  const journal: ChainStorageMigrationJournal = {
    state: 'start',
    customPath: layout.customPath || legacyRootPath,
    legacyRootPath,
    managedChainPath,
    movedEntries: [],
    ignoredEntries: layout.ignoredLegacyEntries,
    backupEntryPointPath: `${ctx._chainPath}.legacy-backup`,
    tempEntryPointPath: `${ctx._chainPath}.managed-next`,
    createdAt: toIsoString(),
    updatedAt: toIsoString(),
  };

  await ctx._writeMigrationJournal(journal);

  try {
    await fs.ensureDir(managedChainPath);

    for (const entry of layout.managedLegacyEntries) {
      await ctx._movePath(
        path.join(legacyRootPath, entry),
        path.join(managedChainPath, entry)
      );
      journal.movedEntries.push(entry);
      journal.state = 'progress';
      journal.updatedAt = toIsoString();
      await ctx._writeMigrationJournal(journal);
    }

    await fs.remove(journal.tempEntryPointPath);
    await fs.remove(journal.backupEntryPointPath);
    await ctx._createSymlink(managedChainPath, journal.tempEntryPointPath);

    journal.state = 'cutover';
    journal.updatedAt = toIsoString();
    await ctx._writeMigrationJournal(journal);

    if (await ctx._pathExistsViaLstat(ctx._chainPath)) {
      await fs.rename(ctx._chainPath, journal.backupEntryPointPath);
    }
    await fs.rename(journal.tempEntryPointPath, ctx._chainPath);
    if (await ctx._pathExistsViaLstat(journal.backupEntryPointPath)) {
      await fs.remove(journal.backupEntryPointPath);
    }

    journal.state = 'completion';
    journal.updatedAt = toIsoString();
    await ctx._writeMigrationJournal(journal);
    await fs.remove(ctx._migrationJournalPath);

    if (layout.ignoredLegacyEntries.length > 0) {
      logger.warn(
        'ChainStorageManager: left non-managed entries in custom chain parent during migration',
        {
          customPath: layout.customPath,
          ignoredEntries: layout.ignoredLegacyEntries,
        }
      );
    }
  } catch (error) {
    logger.warn('ChainStorageManager: legacy migration failed, rolling back', {
      error,
      customPath: layout.customPath,
    });
    journal.state = 'rollback';
    journal.updatedAt = toIsoString();
    await ctx._writeMigrationJournal(journal);
    await ctx._rollbackMigrationJournal(journal);
    throw error;
  }
}

export async function recoverInterruptedMigration(
  ctx: ChainStorageManagerContext,
  options: EnsureManagedLayoutOptions
): Promise<void> {
  const journal = await ctx._readMigrationJournal();
  if (!journal) {
    return;
  }

  if (journal.state === 'completion') {
    await fs.remove(ctx._migrationJournalPath);
    return;
  }

  if (journal.state === 'cutover') {
    const chainState = await ctx._captureChainPathState();
    if (
      chainState.type === 'symlink' &&
      chainState.resolvedPath &&
      ctx._isSamePath(chainState.resolvedPath, journal.managedChainPath)
    ) {
      if (await ctx._pathExistsViaLstat(journal.backupEntryPointPath)) {
        await fs.remove(journal.backupEntryPointPath);
      }
      if (await ctx._pathExistsViaLstat(journal.tempEntryPointPath)) {
        await fs.remove(journal.tempEntryPointPath);
      }
      await fs.remove(ctx._migrationJournalPath);
      return;
    }
  }

  await ctx._assertNodeStopped(
    options.nodeState,
    'recover interrupted chain storage migration'
  );

  const rollbackJournal = {
    ...journal,
    state: 'rollback' as const,
    updatedAt: toIsoString(),
  };
  await ctx._writeMigrationJournal(rollbackJournal);
  await ctx._rollbackMigrationJournal(rollbackJournal);
}

export async function rollbackMigrationJournal(
  ctx: ChainStorageManagerContext,
  journal: ChainStorageMigrationJournal
): Promise<void> {
  try {
    if (await ctx._pathExistsViaLstat(journal.tempEntryPointPath)) {
      await fs.remove(journal.tempEntryPointPath);
    }

    if (await ctx._pathExistsViaLstat(journal.backupEntryPointPath)) {
      if (await ctx._pathExistsViaLstat(ctx._chainPath)) {
        await fs.remove(ctx._chainPath);
      }
      await fs.rename(journal.backupEntryPointPath, ctx._chainPath);
    }

    for (const entry of [...journal.movedEntries].reverse()) {
      const sourcePath = path.join(journal.managedChainPath, entry);
      const targetPath = path.join(journal.legacyRootPath, entry);

      if (await fs.pathExists(sourcePath)) {
        await ctx._movePath(sourcePath, targetPath);
      }
    }

    if (await fs.pathExists(journal.managedChainPath)) {
      const remainingEntries = await ctx._safeReadDir(journal.managedChainPath);
      if (remainingEntries.length === 0) {
        await fs.remove(journal.managedChainPath);
      }
    }
  } finally {
    await fs.remove(ctx._migrationJournalPath);
  }
}

export async function preflightLegacyMigration(
  ctx: ChainStorageManagerContext,
  {
    legacyRootPath,
    managedChainPath,
    managedEntries,
  }: {
    legacyRootPath: string;
    managedChainPath: string;
    managedEntries: string[];
  }
): Promise<void> {
  const managedChainStats = await ctx._safeLstat(managedChainPath);
  if (managedChainStats && !managedChainStats.isDirectory()) {
    throw new Error(
      'Daedalus cannot migrate chain storage because the managed chain subdirectory already exists as a file.'
    );
  }

  const legacyRootStats = await ctx._safeLstat(legacyRootPath);
  if (!legacyRootStats || !legacyRootStats.isDirectory()) {
    throw new Error('Configured custom chain storage parent is unavailable.');
  }

  const legacySourceSizeBytes = await ctx._getEntriesSizeBytes(
    legacyRootPath,
    managedEntries
  );
  const { free } = await checkDiskSpace(legacyRootPath);

  if (
    legacySourceSizeBytes > 0 &&
    free < legacySourceSizeBytes &&
    !isPathWithin(legacyRootPath, managedChainPath)
  ) {
    throw new Error(
      'Daedalus cannot migrate chain storage because the destination does not have enough free space.'
    );
  }
}

export async function writeMigrationJournal(
  ctx: ChainStorageManagerContext,
  journal: ChainStorageMigrationJournal
): Promise<void> {
  await fs.ensureDir(ctx._logsDirectoryPath);
  await fs.writeJson(ctx._migrationJournalPath, journal, { spaces: 2 });
}

export async function readMigrationJournal(
  ctx: ChainStorageManagerContext
): Promise<ChainStorageMigrationJournal | null> {
  try {
    const exists = await fs.pathExists(ctx._migrationJournalPath);
    if (!exists) {
      return null;
    }

    const journal = await fs.readJson(ctx._migrationJournalPath);
    if (!journal || typeof journal !== 'object') {
      return null;
    }

    return journal as ChainStorageMigrationJournal;
  } catch (error) {
    logger.warn('ChainStorageManager: failed to read migration journal', {
      error,
      migrationJournalPath: ctx._migrationJournalPath,
    });
    return null;
  }
}

export async function assertNodeStopped(
  _ctx: ChainStorageManagerContext,
  nodeState: string | null | undefined,
  reason: string
): Promise<void> {
  if (
    nodeState != null &&
    nodeState !== CardanoNodeStates.STOPPED &&
    nodeState !== CardanoNodeStates.STOPPING
  ) {
    throw new Error(
      `Daedalus can only ${reason} while cardano-node is stopped.`
    );
  }
}

export async function emptyManagedContents(
  ctx: ChainStorageManagerContext,
  managedChainPath: string,
  options: EmptyManagedContentsOptions = {}
): Promise<void> {
  await fs.ensureDir(managedChainPath);
  const excludedEntries = new Set(options.excludeTopLevelEntries || []);
  const entries = await ctx._safeReadDir(managedChainPath);

  for (const entry of entries) {
    if (!excludedEntries.has(entry)) {
      await fs.remove(path.join(managedChainPath, entry));
    }
  }
}

export async function installSnapshot(
  ctx: ChainStorageManagerContext,
  dbDirectory: string
): Promise<void> {
  await ctx._ensureManagedChainLayout({
    nodeState: CardanoNodeStates.STOPPED,
  });

  const managedChainPath = await ctx.getManagedChainPath();
  const resolvedManagedChainPath = await ctx._resolveRealPathOrInput(
    managedChainPath
  );
  const resolvedDbDirectory = path.resolve(dbDirectory);

  if (ctx._isSamePath(resolvedDbDirectory, resolvedManagedChainPath)) {
    return;
  }

  let excludedTopLevelEntries: string[] = [];
  if (isPathWithin(resolvedManagedChainPath, resolvedDbDirectory)) {
    const relativeDbPath = path.relative(
      resolvedManagedChainPath,
      resolvedDbDirectory
    );
    const [topLevelEntry] = relativeDbPath.split(path.sep);
    if (topLevelEntry) {
      excludedTopLevelEntries = [topLevelEntry];
    }
  }

  await ctx._emptyManagedContents(resolvedManagedChainPath, {
    excludeTopLevelEntries: excludedTopLevelEntries,
  });

  const entries = await ctx._safeReadDir(resolvedDbDirectory);
  for (const entry of entries) {
    await ctx._movePath(
      path.join(resolvedDbDirectory, entry),
      path.join(resolvedManagedChainPath, entry)
    );
  }

  if (!ctx._isSamePath(resolvedDbDirectory, resolvedManagedChainPath)) {
    await fs.remove(resolvedDbDirectory);
  }
}
