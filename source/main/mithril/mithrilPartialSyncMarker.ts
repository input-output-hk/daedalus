import path from 'path';
import fs from 'fs-extra';
import { stateDirectoryPath } from '../config';
import { logger } from '../utils/logging';

export type MithrilPartialSyncMarkerState =
  | 'cutover-in-progress'
  | 'installed-awaiting-node-start'
  | 'node-start-verified';

export type MithrilPartialSyncMarker = {
  state: MithrilPartialSyncMarkerState;
  updatedAt: string;
  managedChainPath?: string;
};

const PARTIAL_SYNC_MARKER_FILE_NAME = 'mithril-partial-sync.lock';

export const getMithrilPartialSyncMarkerPath = (): string =>
  path.join(stateDirectoryPath, 'Logs', PARTIAL_SYNC_MARKER_FILE_NAME);

export async function writeMithrilPartialSyncMarker(
  state: MithrilPartialSyncMarkerState,
  options: { managedChainPath?: string } = {}
): Promise<MithrilPartialSyncMarker> {
  const marker = {
    state,
    updatedAt: new Date().toISOString(),
    ...(options.managedChainPath ? { managedChainPath: options.managedChainPath } : {}),
  };
  const markerPath = getMithrilPartialSyncMarkerPath();

  await fs.ensureDir(path.dirname(markerPath));
  await fs.writeJson(markerPath, marker, { spaces: 2 });

  return marker;
}

export async function readMithrilPartialSyncMarker(): Promise<MithrilPartialSyncMarker | null> {
  const markerPath = getMithrilPartialSyncMarkerPath();

  try {
    const exists = await fs.pathExists(markerPath);
    if (!exists) {
      return null;
    }

    const marker = await fs.readJson(markerPath);
    if (
      marker &&
      typeof marker === 'object' &&
      [
        'cutover-in-progress',
        'installed-awaiting-node-start',
        'node-start-verified',
      ].includes((marker as MithrilPartialSyncMarker).state)
    ) {
      return marker as MithrilPartialSyncMarker;
    }
  } catch (error) {
    logger.warn('Mithril partial sync marker read failed; treating as unsafe cutover state', {
      error,
      markerPath,
    });
  }

  return {
    state: 'cutover-in-progress',
    updatedAt: '',
  };
}

export async function clearMithrilPartialSyncMarker(): Promise<void> {
  await fs.remove(getMithrilPartialSyncMarkerPath());
}
