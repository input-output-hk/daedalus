import path from 'path';
import fs from 'fs-extra';
import type { Stats } from 'fs';
import type {
  MithrilPartialSyncErrorCode,
  MithrilPartialSyncErrorStage,
} from '../../common/types/mithril-partial-sync.types';
import { createPartialSyncStageError } from './mithrilErrors';

export const PARTIAL_SYNC_STAGED_DB_INVALID_CODE =
  'PARTIAL_SYNC_STAGED_DB_INVALID';

export type PartialSyncRange = { start: number; end: number };

const parseImmutableFileNumber = (entryName: string): number | null => {
  const stem = entryName.split('.')[0]?.trim();
  return stem && /^\d+$/.test(stem) ? Number(stem) : null;
};

export async function statRequiredPath(
  targetPath: string,
  message: string,
  stage: MithrilPartialSyncErrorStage = 'preparing',
  code?: MithrilPartialSyncErrorCode
): Promise<Stats> {
  try {
    return await fs.stat(targetPath);
  } catch (error) {
    throw createPartialSyncStageError(stage, message, code);
  }
}

export async function ensureReadablePath(
  targetPath: string,
  message: string,
  mode: number
): Promise<void> {
  try {
    await fs.access(targetPath, mode);
  } catch (error) {
    throw createPartialSyncStageError('preparing', message);
  }
}

export async function readImmutableDirectory(
  immutablePath: string
): Promise<Array<string>> {
  try {
    return await fs.readdir(immutablePath);
  } catch (error) {
    throw createPartialSyncStageError(
      'preparing',
      'Unable to read the immutable directory for Mithril partial sync preflight.'
    );
  }
}

export async function resolveLocalImmutableNumber(
  managedChainPath: string
): Promise<number> {
  const managedChainStats = await statRequiredPath(
    managedChainPath,
    'Unable to read the managed chain directory for Mithril partial sync preflight.'
  );

  if (!managedChainStats.isDirectory()) {
    throw createPartialSyncStageError(
      'preparing',
      'The managed chain path is not a directory.',
      'PARTIAL_SYNC_MANAGED_CHAIN_INVALID'
    );
  }

  const immutablePath = path.join(managedChainPath, 'immutable');
  const immutableStats = await statRequiredPath(
    immutablePath,
    'Unable to read the immutable directory for Mithril partial sync preflight.'
  );

  if (!immutableStats.isDirectory()) {
    throw createPartialSyncStageError(
      'preparing',
      'The managed chain immutable path is not a directory.',
      'PARTIAL_SYNC_IMMUTABLE_INVALID'
    );
  }

  await ensureReadablePath(
    immutablePath,
    'Unable to read the immutable directory for Mithril partial sync preflight.',
    fs.constants.R_OK | fs.constants.X_OK
  );

  const protocolMagicIdPath = path.join(managedChainPath, 'protocolMagicId');
  const protocolMagicIdStats = await statRequiredPath(
    protocolMagicIdPath,
    'Unable to read protocolMagicId for Mithril partial sync preflight.'
  );

  if (!protocolMagicIdStats.isFile()) {
    throw createPartialSyncStageError(
      'preparing',
      'The managed chain protocolMagicId path is not a file.',
      'PARTIAL_SYNC_PROTOCOL_MAGIC_INVALID'
    );
  }

  await ensureReadablePath(
    protocolMagicIdPath,
    'Unable to read protocolMagicId for Mithril partial sync preflight.',
    fs.constants.R_OK
  );

  const localImmutableNumber = (await readImmutableDirectory(immutablePath))
    .map((entryName) => parseImmutableFileNumber(entryName))
    .filter((value): value is number => value != null)
    .sort((left, right) => right - left)[0];

  if (localImmutableNumber == null) {
    throw createPartialSyncStageError(
      'preparing',
      'Unable to determine the local immutable position from the managed chain immutable directory.',
      'PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE'
    );
  }

  return localImmutableNumber;
}

export function derivePartialSyncRange(
  localImmutableNumber: number,
  latestCertifiedImmutableNumber: number
): PartialSyncRange {
  if (localImmutableNumber >= latestCertifiedImmutableNumber) {
    // Immutables are at or ahead of the certified tip — the ledger state may
    // still be missing or from an incompatible snapshot (e.g. the user deleted
    // it, or the node fell into a full replay). Download just the ledger state
    // at the certified position; --allow-override handles the one overlapping
    // immutable chunk harmlessly. The node replays the small trailing gap.
    return {
      start: latestCertifiedImmutableNumber,
      end: latestCertifiedImmutableNumber,
    };
  }

  return {
    start: localImmutableNumber + 1,
    end: latestCertifiedImmutableNumber,
  };
}
