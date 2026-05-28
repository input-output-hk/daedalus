import path from 'path';
import fs from 'fs-extra';
import type { Stats } from 'fs';
import type { MithrilPartialSyncErrorStage } from '../../common/types/mithril-partial-sync.types';

export const PARTIAL_SYNC_STAGED_DB_INVALID_CODE =
  'PARTIAL_SYNC_STAGED_DB_INVALID';

export type PartialSyncRange = { start: number; end: number };

export type PartialSyncStageErrorFactory = (
  stage: MithrilPartialSyncErrorStage,
  message: string,
  code?: string
) => Error;

const parseImmutableFileNumber = (entryName: string): number | null => {
  const stem = entryName.split('.')[0]?.trim();
  return stem && /^\d+$/.test(stem) ? Number(stem) : null;
};

export async function statRequiredPath(
  targetPath: string,
  message: string,
  createStageError: PartialSyncStageErrorFactory,
  stage: MithrilPartialSyncErrorStage = 'preparing',
  code?: string
): Promise<Stats> {
  try {
    return await fs.stat(targetPath);
  } catch (error) {
    throw createStageError(stage, message, code);
  }
}

export async function ensureReadablePath(
  targetPath: string,
  message: string,
  createStageError: PartialSyncStageErrorFactory,
  mode: number
): Promise<void> {
  try {
    await fs.access(targetPath, mode);
  } catch (error) {
    throw createStageError('preparing', message);
  }
}

export async function readImmutableDirectory(
  immutablePath: string,
  createStageError: PartialSyncStageErrorFactory
): Promise<Array<string>> {
  try {
    return await fs.readdir(immutablePath);
  } catch (error) {
    throw createStageError(
      'preparing',
      'Unable to read the immutable directory for Mithril partial sync preflight.'
    );
  }
}

export async function resolveLocalImmutableNumber(
  managedChainPath: string,
  createStageError: PartialSyncStageErrorFactory
): Promise<number> {
  const managedChainStats = await statRequiredPath(
    managedChainPath,
    'Unable to read the managed chain directory for Mithril partial sync preflight.',
    createStageError
  );

  if (!managedChainStats.isDirectory()) {
    throw createStageError(
      'preparing',
      'The managed chain path is not a directory.',
      'PARTIAL_SYNC_MANAGED_CHAIN_INVALID'
    );
  }

  const immutablePath = path.join(managedChainPath, 'immutable');
  const immutableStats = await statRequiredPath(
    immutablePath,
    'Unable to read the immutable directory for Mithril partial sync preflight.',
    createStageError
  );

  if (!immutableStats.isDirectory()) {
    throw createStageError(
      'preparing',
      'The managed chain immutable path is not a directory.',
      'PARTIAL_SYNC_IMMUTABLE_INVALID'
    );
  }

  await ensureReadablePath(
    immutablePath,
    'Unable to read the immutable directory for Mithril partial sync preflight.',
    createStageError,
    fs.constants.R_OK | fs.constants.X_OK
  );

  const protocolMagicIdPath = path.join(managedChainPath, 'protocolMagicId');
  const protocolMagicIdStats = await statRequiredPath(
    protocolMagicIdPath,
    'Unable to read protocolMagicId for Mithril partial sync preflight.',
    createStageError
  );

  if (!protocolMagicIdStats.isFile()) {
    throw createStageError(
      'preparing',
      'The managed chain protocolMagicId path is not a file.',
      'PARTIAL_SYNC_PROTOCOL_MAGIC_INVALID'
    );
  }

  await ensureReadablePath(
    protocolMagicIdPath,
    'Unable to read protocolMagicId for Mithril partial sync preflight.',
    createStageError,
    fs.constants.R_OK
  );

  const localImmutableNumber = (
    await readImmutableDirectory(immutablePath, createStageError)
  )
    .map((entryName) => parseImmutableFileNumber(entryName))
    .filter((value): value is number => value != null)
    .sort((left, right) => right - left)[0];

  if (localImmutableNumber == null) {
    throw createStageError(
      'preparing',
      'Unable to determine the local immutable position from the managed chain immutable directory.',
      'PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE'
    );
  }

  return localImmutableNumber;
}

export function derivePartialSyncRange(
  localImmutableNumber: number,
  latestCertifiedImmutableNumber: number,
  createStageError: PartialSyncStageErrorFactory
): PartialSyncRange {
  if (localImmutableNumber >= latestCertifiedImmutableNumber) {
    throw createStageError(
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
