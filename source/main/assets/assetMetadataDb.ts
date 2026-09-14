import fs from 'fs';
import path from 'path';
import { DatabaseSync } from 'node:sqlite';
import { stateDirectoryPath } from '../config';
import type {
  AssetMetadataSource,
  AssetResolutionState,
} from '../../common/types/asset-metadata.types';
import { logger } from '../utils/logging';

const ASSET_METADATA_DIRECTORY_NAME = 'asset-metadata-cache';
const ASSET_METADATA_DATABASE_FILE_NAME = 'assets.sqlite';

/**
 * Bumped only when the schema below changes shape. A file stamped with any
 * other value is deleted rather than migrated: every row in it can be fetched
 * again, so a migration would be code written to preserve nothing.
 */
export const ASSET_METADATA_DB_VERSION = 1;

/**
 * One bound parameter per subject, and `SQLITE_MAX_VARIABLE_NUMBER` is a
 * compile-time constant of whichever build is loaded. Reads degrade to a miss
 * on a throw, so an unchunked read that crossed the limit would look exactly
 * like a cold cache rather than like a bug.
 */
const SUBJECT_CHUNK_SIZE = 500;

/**
 * The largest single image the table will hold. Declared here rather than in
 * the image store because `writeImage` enforces it and `enforceImageBounds`
 * derives its sweep floor from it.
 */
export const ASSET_IMAGE_MAX_ENTRY_BYTES = 256 * 1024;

const SCHEMA = `
CREATE TABLE IF NOT EXISTS asset_metadata (
  subject          TEXT    NOT NULL PRIMARY KEY,
  policy_id        TEXT    NOT NULL,
  asset_name       TEXT    NOT NULL,
  ticker           TEXT,
  name             TEXT,
  decimals         INTEGER,
  verified         INTEGER NOT NULL DEFAULT 0,
  metadata         TEXT,
  source           TEXT    NOT NULL,
  sequence_number  INTEGER,
  slot             INTEGER,
  updated_at       INTEGER NOT NULL,
  CHECK (subject = policy_id || asset_name),
  CHECK (verified IN (0, 1)),
  CHECK (decimals IS NULL OR (decimals >= 0 AND decimals <= 20)),
  CHECK (source IN ('registry', 'chain')),
  CHECK (source <> 'registry' OR slot IS NULL),
  CHECK (source <> 'chain' OR sequence_number IS NULL)
) STRICT;

CREATE INDEX IF NOT EXISTS asset_metadata_policy_id ON asset_metadata (policy_id);

CREATE TABLE IF NOT EXISTS asset_image (
  subject      TEXT    NOT NULL PRIMARY KEY
               REFERENCES asset_metadata (subject) ON DELETE CASCADE,
  media_type   TEXT    NOT NULL,
  bytes        BLOB    NOT NULL,
  byte_length  INTEGER NOT NULL,
  fetched_at   INTEGER NOT NULL
) STRICT;

CREATE INDEX IF NOT EXISTS asset_image_fetched_at ON asset_image (fetched_at);

CREATE TABLE IF NOT EXISTS asset_resolution (
  subject        TEXT    NOT NULL PRIMARY KEY,
  state          TEXT    NOT NULL,
  attempted_at   INTEGER NOT NULL,
  retry_after    INTEGER NOT NULL,
  failure_count  INTEGER NOT NULL DEFAULT 0,
  CHECK (state IN ('pending', 'resolved', 'unregistered', 'failed'))
) STRICT;
`;

const METADATA_COLUMNS =
  'subject, policy_id, asset_name, ticker, name, decimals, verified, metadata, source, sequence_number, slot, updated_at';

const METADATA_SELECT = `SELECT ${METADATA_COLUMNS} FROM asset_metadata WHERE subject IN`;

const METADATA_UPSERT = `
INSERT INTO asset_metadata (${METADATA_COLUMNS})
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT (subject) DO UPDATE SET
  policy_id = excluded.policy_id,
  asset_name = excluded.asset_name,
  ticker = excluded.ticker,
  name = excluded.name,
  decimals = excluded.decimals,
  verified = excluded.verified,
  metadata = excluded.metadata,
  source = excluded.source,
  sequence_number = excluded.sequence_number,
  slot = excluded.slot,
  updated_at = excluded.updated_at`;

const IMAGE_TOUCH = 'UPDATE asset_image SET fetched_at = ? WHERE subject = ?';

const IMAGE_COUNT = 'SELECT count(*) AS entries FROM asset_image';

const IMAGE_TOTALS =
  'SELECT count(*) AS entries, coalesce(sum(byte_length), 0) AS bytes FROM asset_image';

const IMAGE_OLDEST_FIRST =
  'SELECT subject, byte_length FROM asset_image ORDER BY fetched_at ASC';

const RESOLUTION_COLUMNS =
  'subject, state, attempted_at, retry_after, failure_count';

const RESOLUTION_SELECT = `SELECT ${RESOLUTION_COLUMNS} FROM asset_resolution WHERE subject IN`;

const IMAGE_SELECT =
  'SELECT subject, media_type, bytes, byte_length, fetched_at FROM asset_image WHERE subject = ?';

const IMAGE_UPSERT = `
INSERT INTO asset_image (subject, media_type, bytes, byte_length, fetched_at)
VALUES (?, ?, ?, ?, ?)
ON CONFLICT (subject) DO UPDATE SET
  media_type = excluded.media_type,
  bytes = excluded.bytes,
  byte_length = excluded.byte_length,
  fetched_at = excluded.fetched_at`;

const RESOLUTION_UPSERT = `
INSERT INTO asset_resolution (${RESOLUTION_COLUMNS})
VALUES (?, ?, ?, ?, ?)
ON CONFLICT (subject) DO UPDATE SET
  state = excluded.state,
  attempted_at = excluded.attempted_at,
  retry_after = excluded.retry_after,
  failure_count = excluded.failure_count`;

// Declared in `source/common/types/asset-metadata.types.ts` and re-exported
// here under the names this module's callers already use. The renderer needs
// both unions to read what the cache sends it, and a second copy of a union the
// schema constrains with a CHECK would be free to drift from the one the engine
// enforces.
export type { AssetMetadataSource, AssetResolutionState };

export type AssetMetadataWrite = {
  subject: string;
  policyId: string;
  assetName: string;
  ticker: string | null;
  name: string | null;
  decimals: number | null;
  verified: boolean;
  metadata: string | null;
  source: AssetMetadataSource;
  sequenceNumber: number | null;
  slot: number | null;
};

export type AssetMetadataRow = AssetMetadataWrite & {
  updatedAt: number;
};

export type AssetResolutionWrite = {
  subject: string;
  state: AssetResolutionState;
  retryAfter: number;
  failureCount: number;
};

export type AssetResolutionRow = AssetResolutionWrite & {
  attemptedAt: number;
};

export type AssetImageWrite = {
  subject: string;
  mediaType: string;
  bytes: Uint8Array;
};

export type AssetImageRow = AssetImageWrite & {
  byteLength: number;
  fetchedAt: number;
};

export const assetMetadataDirectoryPath = (): string =>
  path.join(stateDirectoryPath, ASSET_METADATA_DIRECTORY_NAME);

export const assetMetadataDatabasePath = (): string =>
  path.join(assetMetadataDirectoryPath(), ASSET_METADATA_DATABASE_FILE_NAME);

const asText = (value: unknown): string | null =>
  typeof value === 'string' ? value : null;

const asNumber = (value: unknown): number | null =>
  typeof value === 'number' ? value : null;

const asInteger = (value: number | null | undefined): number | null =>
  typeof value === 'number' ? value : null;

const toMetadataRow = (row: Record<string, unknown>): AssetMetadataRow => ({
  subject: String(row.subject),
  policyId: String(row.policy_id),
  assetName: String(row.asset_name),
  ticker: asText(row.ticker),
  name: asText(row.name),
  decimals: asNumber(row.decimals),
  verified: row.verified === 1,
  metadata: asText(row.metadata),
  source: String(row.source) as AssetMetadataSource,
  sequenceNumber: asNumber(row.sequence_number),
  slot: asNumber(row.slot),
  updatedAt: Number(row.updated_at),
});

const toResolutionRow = (row: Record<string, unknown>): AssetResolutionRow => ({
  subject: String(row.subject),
  state: String(row.state) as AssetResolutionState,
  attemptedAt: Number(row.attempted_at),
  retryAfter: Number(row.retry_after),
  failureCount: Number(row.failure_count),
});

const reasonOf = (error: unknown): string =>
  error instanceof Error ? error.message : 'unknown';

const removeDatabaseFiles = (filePath: string): void => {
  // WAL leaves two siblings. Removing the main file alone leaves a journal that
  // the next open would try to replay into a database that is no longer there.
  [filePath, `${filePath}-wal`, `${filePath}-shm`].forEach((entry) => {
    try {
      fs.unlinkSync(entry);
    } catch {
      // Already absent is the desired end state.
    }
  });
};

const openHandle = (filePath: string): DatabaseSync => {
  fs.mkdirSync(path.dirname(filePath), { recursive: true });
  const db = new DatabaseSync(filePath);
  try {
    db.exec('PRAGMA journal_mode = WAL');
    db.exec('PRAGMA foreign_keys = ON');
    const stored = Number(
      db.prepare('PRAGMA user_version').get()?.user_version
    );
    if (stored !== 0 && stored !== ASSET_METADATA_DB_VERSION) {
      throw new Error(
        `asset metadata cache was written at version ${stored}, expected ${ASSET_METADATA_DB_VERSION}`
      );
    }
    db.exec(SCHEMA);
    if (stored === 0) {
      db.exec(`PRAGMA user_version = ${ASSET_METADATA_DB_VERSION}`);
    }
  } catch (error) {
    db.close();
    throw error;
  }
  return db;
};

export class AssetMetadataDatabase {
  private _db: DatabaseSync | null = null;

  constructor(filePath: string) {
    try {
      this._db = openHandle(filePath);
      return;
    } catch (error) {
      logger.warn('Asset metadata cache: recreating the database', {
        reason: reasonOf(error),
      });
    }
    // One recreation, not a loop. Two failures in a row is a filesystem this
    // process cannot use, and retrying on every call would turn a degraded
    // cache into a stalled one.
    try {
      removeDatabaseFiles(filePath);
      this._db = openHandle(filePath);
    } catch (error) {
      logger.warn('Asset metadata cache: unavailable, answering as empty', {
        reason: reasonOf(error),
      });
    }
  }

  close(): void {
    if (!this._db) return;
    try {
      this._db.close();
    } catch (error) {
      logger.warn('Asset metadata cache: close failed', {
        reason: reasonOf(error),
      });
    }
    this._db = null;
  }

  readMetadata(subjects: Array<string>): Array<AssetMetadataRow> {
    return this._read(subjects, METADATA_SELECT, toMetadataRow);
  }

  readResolutions(subjects: Array<string>): Array<AssetResolutionRow> {
    return this._read(subjects, RESOLUTION_SELECT, toResolutionRow);
  }

  writeMetadata(
    rows: Array<AssetMetadataWrite>,
    updatedAt: number = Date.now()
  ): number {
    return this._write(rows, METADATA_UPSERT, (row) => [
      row.subject,
      row.policyId,
      row.assetName,
      asText(row.ticker),
      asText(row.name),
      asInteger(row.decimals),
      row.verified ? 1 : 0,
      asText(row.metadata),
      row.source,
      asInteger(row.sequenceNumber),
      asInteger(row.slot),
      updatedAt,
    ]);
  }

  readImage(subject: string): AssetImageRow | null {
    const db = this._db;
    if (!db || typeof subject !== 'string' || subject.length === 0) return null;
    try {
      const row = db.prepare(IMAGE_SELECT).get(subject);
      if (!row) return null;
      return {
        subject: String(row.subject),
        mediaType: String(row.media_type),
        bytes: row.bytes as Uint8Array,
        byteLength: Number(row.byte_length),
        fetchedAt: Number(row.fetched_at),
      };
    } catch (error) {
      logger.warn('Asset metadata cache: image read failed', {
        reason: reasonOf(error),
      });
      return null;
    }
  }

  /**
   * Returns false when the row was not stored. The foreign key means an image
   * can only exist for a subject the cache already knows, so a refusal is a
   * fact about the cache rather than a failure.
   *
   * The per-entry cap is enforced here rather than left to a caller, because
   * the eviction sweep floor is derived from it: below a row count of
   * total bound over per-entry cap, neither bound can be crossed, and that is
   * only true if no row can exceed the per-entry cap.
   */
  writeImage(
    row: AssetImageWrite,
    fetchedAt: number = Date.now(),
    maxBytes: number = ASSET_IMAGE_MAX_ENTRY_BYTES
  ): boolean {
    const db = this._db;
    if (!db) return false;
    if (!row.bytes || row.bytes.length === 0 || row.bytes.length > maxBytes) {
      return false;
    }
    try {
      db.prepare(IMAGE_UPSERT).run(
        row.subject,
        row.mediaType,
        row.bytes,
        row.bytes.length,
        fetchedAt
      );
      return true;
    } catch (error) {
      logger.warn('Asset metadata cache: image write refused', {
        reason: reasonOf(error),
      });
      return false;
    }
  }

  /**
   * Moves a row to the end of the eviction ordering. Rate-limited by the
   * caller's interval because SQLite rewrites a whole row on an update, blob
   * included, so an unconditional touch would write tens of kilobytes per
   * rendered row per paint.
   */
  touchImage(subject: string, fetchedAt: number): boolean {
    const db = this._db;
    if (!db) return false;
    try {
      return db.prepare(IMAGE_TOUCH).run(fetchedAt, subject).changes > 0;
    } catch (error) {
      logger.warn('Asset metadata cache: image touch failed', {
        reason: reasonOf(error),
      });
      return false;
    }
  }

  /**
   * Evicts the least recently fetched until both bounds hold. Returns how many
   * rows were removed.
   *
   * `count(*)` is answered without reading a row; `sum(byte_length)` is not,
   * because SQLite reads a whole row to reach any column of it and every row
   * here carries a blob. So the count comes first and the sum is only taken
   * above the floor, below which neither bound can be crossed.
   */
  enforceImageBounds(maxEntries: number, maxTotalBytes: number): number {
    const db = this._db;
    if (!db) return 0;
    try {
      const floor = Math.floor(maxTotalBytes / ASSET_IMAGE_MAX_ENTRY_BYTES);
      const entries = Number(db.prepare(IMAGE_COUNT).get()?.entries ?? 0);
      if (entries <= Math.min(maxEntries, floor)) return 0;

      const totals = db.prepare(IMAGE_TOTALS).get();
      let count = Number(totals?.entries ?? 0);
      let bytes = Number(totals?.bytes ?? 0);
      if (count <= maxEntries && bytes <= maxTotalBytes) return 0;

      const doomed: Array<string> = [];
      const rows = db.prepare(IMAGE_OLDEST_FIRST).all();
      for (let index = 0; index < rows.length; index += 1) {
        if (count <= maxEntries && bytes <= maxTotalBytes) break;
        doomed.push(String(rows[index].subject));
        count -= 1;
        bytes -= Number(rows[index].byte_length);
      }
      if (doomed.length === 0) return 0;

      const placeholders = doomed.map(() => '?').join(', ');
      db.prepare(
        `DELETE FROM asset_image WHERE subject IN (${placeholders})`
      ).run(...doomed);
      return doomed.length;
    } catch (error) {
      logger.warn('Asset metadata cache: image eviction failed', {
        reason: reasonOf(error),
      });
      return 0;
    }
  }

  writeResolutions(
    rows: Array<AssetResolutionWrite>,
    attemptedAt: number = Date.now()
  ): number {
    return this._write(rows, RESOLUTION_UPSERT, (row) => [
      row.subject,
      row.state,
      attemptedAt,
      row.retryAfter,
      row.failureCount,
    ]);
  }

  private _read<T>(
    subjects: Array<string>,
    selectPrefix: string,
    map: (row: Record<string, unknown>) => T
  ): Array<T> {
    const db = this._db;
    const results: Array<T> = [];
    if (!db || subjects.length === 0) return results;
    try {
      for (
        let index = 0;
        index < subjects.length;
        index += SUBJECT_CHUNK_SIZE
      ) {
        const chunk = subjects.slice(index, index + SUBJECT_CHUNK_SIZE);
        const placeholders = chunk.map(() => '?').join(', ');
        const statement = db.prepare(`${selectPrefix} (${placeholders})`);
        statement.all(...chunk).forEach((row) => results.push(map(row)));
      }
    } catch (error) {
      logger.warn('Asset metadata cache: read failed', {
        reason: reasonOf(error),
      });
    }
    return results;
  }

  private _write<T>(
    rows: Array<T>,
    sql: string,
    bind: (row: T) => Array<string | number | null>
  ): number {
    const db = this._db;
    if (!db || rows.length === 0) return 0;
    let written = 0;
    try {
      const statement = db.prepare(sql);
      db.exec('BEGIN');
      rows.forEach((row) => {
        try {
          statement.run(...bind(row));
          written += 1;
        } catch (error) {
          // A row the registry produced that violates a constraint is a fact
          // about that row. SQLite aborts the statement and leaves the
          // transaction usable, so the rest of the batch still commits.
          logger.warn('Asset metadata cache: row rejected', {
            reason: reasonOf(error),
          });
        }
      });
      db.exec('COMMIT');
    } catch (error) {
      logger.warn('Asset metadata cache: write failed', {
        reason: reasonOf(error),
      });
      try {
        db.exec('ROLLBACK');
      } catch {
        // No transaction was open.
      }
      return 0;
    }
    return written;
  }
}

export const openAssetMetadataDatabase = (
  filePath: string = assetMetadataDatabasePath()
): AssetMetadataDatabase => new AssetMetadataDatabase(filePath);
