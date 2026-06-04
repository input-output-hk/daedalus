import path from 'path';
import fs from 'fs-extra';
import { isPathWithin } from '../utils/chainStorageManagerShared';
import {
  PARTIAL_SYNC_STAGED_DB_INVALID_CODE,
  PartialSyncStageErrorFactory,
  statRequiredPath,
} from './mithrilPartialSyncPreflight';

export type PartialSyncStagingPaths = {
  rootPath: string;
  downloadParentPath: string;
  dbPath: string;
};

export const PARTIAL_SYNC_ALLOWED_INSTALL_ENTRIES = [
  'clean',
  'immutable',
  'ledger',
  'lsm',
  'protocolMagicId',
];

export async function preparePartialSyncStagingDirectory(
  rootPath: string,
  managedChainPath: string,
  createStageError: PartialSyncStageErrorFactory
): Promise<PartialSyncStagingPaths> {
  const resolvedManagedChainPath = path.resolve(managedChainPath);
  const resolvedRootPath = path.resolve(rootPath);

  if (isPathWithin(resolvedManagedChainPath, resolvedRootPath)) {
    throw createStageError(
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

export async function validateStagedDownloadOutput(
  stagingPaths: PartialSyncStagingPaths,
  createStageError: PartialSyncStageErrorFactory
): Promise<void> {
  const dbStats = await statRequiredPath(
    stagingPaths.dbPath,
    'Mithril partial sync did not produce the expected staged db directory.',
    createStageError,
    'verifying',
    PARTIAL_SYNC_STAGED_DB_INVALID_CODE
  );

  if (!dbStats.isDirectory()) {
    throw createStageError(
      'verifying',
      'Mithril partial sync did not produce the expected staged db directory.',
      PARTIAL_SYNC_STAGED_DB_INVALID_CODE
    );
  }

  const requiredEntries: Array<{
    relativePath: string;
    type: 'directory' | 'file';
  }> = [
    { relativePath: 'clean', type: 'file' },
    { relativePath: 'immutable', type: 'directory' },
    { relativePath: 'ledger', type: 'directory' },
    { relativePath: 'protocolMagicId', type: 'file' },
  ];

  for (const entry of requiredEntries) {
    const entryPath = path.join(stagingPaths.dbPath, entry.relativePath);
    const entryStats = await statRequiredPath(
      entryPath,
      `Mithril partial sync staged output is missing ${entry.relativePath}.`,
      createStageError,
      'verifying',
      PARTIAL_SYNC_STAGED_DB_INVALID_CODE
    );

    const isExpectedType =
      entry.type === 'directory'
        ? entryStats.isDirectory()
        : entryStats.isFile();

    if (!isExpectedType) {
      throw createStageError(
        'verifying',
        `Mithril partial sync staged output has an invalid ${entry.relativePath} entry.`,
        PARTIAL_SYNC_STAGED_DB_INVALID_CODE
      );
    }
  }
}

export async function validateConvertedStagedOutput(
  dbPath: string,
  createStageError: PartialSyncStageErrorFactory
): Promise<void> {
  const topLevelEntries = (await fs.readdir(dbPath)).sort();
  const expectedEntries = [...PARTIAL_SYNC_ALLOWED_INSTALL_ENTRIES].sort();

  if (
    topLevelEntries.length !== expectedEntries.length ||
    topLevelEntries.some((entry, index) => entry !== expectedEntries[index])
  ) {
    throw createStageError(
      'installing',
      `Mithril partial sync staged output must contain exactly ${expectedEntries.join(', ')}.`,
      PARTIAL_SYNC_STAGED_DB_INVALID_CODE
    );
  }
}
