import fs from 'fs';
import path from 'path';
import { stateDirectoryPath } from '../config';
import { ANCHOR_MAX_BYTES } from './AnchorFetchService';
import { logger } from '../utils/logging';

const ANCHOR_CACHE_DIRECTORY_NAME = 'DRep-anchor-cache';
const ANCHOR_HASH_PATTERN = /^[0-9a-f]{64}$/;

/**
 * Sized for the chain rather than for a session.
 *
 * Mainnet carries roughly 1,000 registered DReps and 3,000 stake pools, and
 * governance actions will want anchors of their own. A cache that holds 500
 * entries evicts DReps a user has already looked at simply because they looked
 * at others, which defeats the point of caching a document whose key is its
 * own hash and which therefore never goes stale.
 *
 * The byte bound is the one that matters in the pathological case: entries are
 * capped at 1 MB apiece upstream, and real DRep metadata runs to a few
 * kilobytes, so the typical full cache is tens of megabytes rather than the
 * bound below.
 */
export const ANCHOR_CACHE_MAX_ENTRIES = 8000;
export const ANCHOR_CACHE_MAX_BYTES = 128 * 1024 * 1024;

// Below this many files neither bound can bite, because no entry exceeds the
// fetch layer's ~1 MB cap. Derived, not typed, so the two caps cannot drift.
const ANCHOR_CACHE_SWEEP_FLOOR = Math.floor(
  ANCHOR_CACHE_MAX_BYTES / ANCHOR_MAX_BYTES
);

export const isValidAnchorHash = (hash: unknown): hash is string =>
  typeof hash === 'string' && ANCHOR_HASH_PATTERN.test(hash);

export const anchorCacheDirectoryPath = (): string =>
  path.join(stateDirectoryPath, ANCHOR_CACHE_DIRECTORY_NAME);

const entryPath = (hash: string): string =>
  path.join(anchorCacheDirectoryPath(), `${hash}.json`);

export function readVerifiedAnchorBytes(hash: string): Buffer | null {
  if (!isValidAnchorHash(hash)) return null;
  try {
    return fs.readFileSync(entryPath(hash));
  } catch {
    return null;
  }
}

export function deleteVerifiedAnchorBytes(hash: string): void {
  if (!isValidAnchorHash(hash)) return;
  try {
    fs.unlinkSync(entryPath(hash));
  } catch {
    // A missing or already-removed entry is the desired end state.
  }
}

export function writeVerifiedAnchorBytes(hash: string, bytes: Buffer): void {
  if (!isValidAnchorHash(hash)) return;
  const directoryPath = anchorCacheDirectoryPath();
  try {
    fs.mkdirSync(directoryPath, { recursive: true });
    fs.writeFileSync(entryPath(hash), bytes, { flag: 'wx' });
  } catch (err) {
    const errorCode = (err as NodeJS.ErrnoException).code ?? 'UNKNOWN';
    // Entries are immutable, so a concurrent writer that won the race left
    // byte-identical content behind.
    if (errorCode !== 'EEXIST') {
      logger.warn('Anchor cache: write failed', { errorCode });
      return;
    }
  }
  enforceCacheBound(directoryPath);
}

type CacheEntryStat = { filePath: string; mtimeMs: number; size: number };

function enforceCacheBound(directoryPath: string): void {
  let fileNames: string[];
  try {
    fileNames = fs.readdirSync(directoryPath);
  } catch {
    return;
  }
  // Every entry is capped at ~1 MB upstream, so below this many files neither
  // bound can be exceeded and the sweep would stat the whole directory for
  // nothing. This is the common case: a wallet holds a handful of anchors.
  if (fileNames.length <= ANCHOR_CACHE_SWEEP_FLOOR) return;

  const entries: CacheEntryStat[] = [];
  fileNames.forEach((name) => {
    if (
      !name.endsWith('.json') ||
      !isValidAnchorHash(path.basename(name, '.json'))
    ) {
      return;
    }
    const filePath = path.join(directoryPath, name);
    try {
      const stats = fs.statSync(filePath);
      entries.push({ filePath, mtimeMs: stats.mtimeMs, size: stats.size });
    } catch {
      // The entry vanished between readdir and stat.
    }
  });
  entries.sort((a, b) => a.mtimeMs - b.mtimeMs);

  let count = entries.length;
  let totalBytes = entries.reduce((sum, entry) => sum + entry.size, 0);

  entries.forEach((entry) => {
    if (
      count <= ANCHOR_CACHE_MAX_ENTRIES &&
      totalBytes <= ANCHOR_CACHE_MAX_BYTES
    ) {
      return;
    }
    try {
      fs.unlinkSync(entry.filePath);
      count -= 1;
      totalBytes -= entry.size;
    } catch {
      // Another process removed it first; the bound still converges.
    }
  });
}
