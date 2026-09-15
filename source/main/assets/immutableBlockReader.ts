import fs from 'fs';
import path from 'path';
import { logger } from '../utils/logging';

/**
 * Reads one block out of the node's immutable database.
 *
 * **The format is undocumented and internal to ouroboros-consensus.** What is
 * below was read off a real preprod database on 2026-09-16 and every field was
 * confirmed against an independent source: the block hash and slot this reader
 * produces for a transaction match what Koios reports for the same transaction.
 * The layout was observed at **primary index version 1**, and a file carrying
 * any other version byte is refused rather than guessed at.
 *
 * A chunk is three files, `NNNNN.chunk`, `NNNNN.primary` and `NNNNN.secondary`.
 *
 * `NNNNN.primary` is one version byte followed by big-endian `uint32` offsets
 * into the secondary index, one per relative slot plus two. Relative slot 0 is
 * reserved for a Byron epoch boundary block, so the entry for absolute slot `s`
 * is at index `(s mod chunkSize) + 1`. Consecutive equal offsets mean no block
 * at that slot.
 *
 * `NNNNN.secondary` is a flat array of 56-byte entries in block order:
 * an 8-byte offset into the chunk file, a 2-byte header offset, a 2-byte header
 * size, a 4-byte CRC32, a 32-byte header hash, and the 8-byte slot.
 *
 * `NNNNN.chunk` is the raw blocks, concatenated with no framing, so a block runs
 * from its own offset to the next entry's offset or to the end of the file.
 *
 * **The chunk size is derived rather than hardcoded.** A complete chunk's
 * primary index has `chunkSize + 2` offsets, so chunk zero says what the size is
 * for the network in use. That avoids carrying a per-network constant that would
 * be wrong for a network nobody thought of.
 */

/** The only primary index version this reader has been read against. */
export const IMMUTABLE_PRIMARY_INDEX_VERSION = 1;

export const IMMUTABLE_SECONDARY_ENTRY_BYTES = 56;

const SECONDARY_HASH_OFFSET = 16;
const SECONDARY_HASH_BYTES = 32;
const SECONDARY_SLOT_OFFSET = 48;

/**
 * A chunk holds one Byron epoch of slots. Mainnet is 21,600 and preprod is the
 * same; the bound is here so a corrupt index cannot produce a chunk size that
 * makes every later arithmetic meaningless.
 */
const MIN_CHUNK_SIZE = 1;
const MAX_CHUNK_SIZE = 1_000_000;

export type ImmutableReadResult =
  | { status: 'found'; bytes: Uint8Array; headerHash: string }
  /** The pointer is newer than anything the immutable database holds. */
  | { status: 'beyond-immutable-tip'; tipSlot: number }
  /** The immutable database covers that slot and holds no such block. */
  | { status: 'absent' }
  /** The database could not be read, or is in a shape this reader does not know. */
  | { status: 'unreadable'; reason: string };

const chunkName = (chunk: number, extension: string): string =>
  `${String(chunk).padStart(5, '0')}.${extension}`;

const reasonOf = (error: unknown): string =>
  error instanceof Error ? error.message : 'unknown';

/**
 * The immutable directory of the chain the node is running against.
 *
 * The node's chain lives at `<stateDir>/chain` unless the user moved it, in
 * which case it is `<customPath>/chain` (`source/main/index.ts:360-369`). The
 * custom path is resolved by the caller, because this module is also built
 * against a fixture directory in its own spec.
 */
export const immutableDirectoryPath = (chainPath: string): string =>
  path.join(chainPath, 'immutable');

export class ImmutableBlockReader {
  private _directory: string;

  private _chunkSize: number | null = null;

  private _chunkSizeChecked = false;

  private _tipSlot: number | null = null;

  private _tipChecked = false;

  /**
   * One reader per resolution pass. The tip is read once and cached, because
   * finding it means listing a directory holding three files per chunk, which
   * on a synced mainnet is tens of thousands of entries. A tip that has gone
   * stale can only classify a pointer as newer than the immutable database,
   * which resolves it later rather than wrongly.
   */
  constructor(directory: string) {
    this._directory = directory;
  }

  /**
   * Derived from chunk zero's primary index, once. Null when the database is
   * absent or the index is not a shape this reader knows.
   */
  chunkSize(): number | null {
    if (this._chunkSizeChecked) return this._chunkSize;
    this._chunkSizeChecked = true;
    try {
      const primary = fs.readFileSync(
        path.join(this._directory, chunkName(0, 'primary'))
      );
      const version = primary[0];
      if (version !== IMMUTABLE_PRIMARY_INDEX_VERSION) {
        logger.warn('Immutable database: unrecognised primary index version', {
          version,
          expected: IMMUTABLE_PRIMARY_INDEX_VERSION,
        });
        return null;
      }
      if ((primary.length - 1) % 4 !== 0) {
        logger.warn(
          'Immutable database: primary index is not a whole number of offsets'
        );
        return null;
      }
      const size = (primary.length - 1) / 4 - 2;
      if (
        !Number.isInteger(size) ||
        size < MIN_CHUNK_SIZE ||
        size > MAX_CHUNK_SIZE
      ) {
        logger.warn('Immutable database: implausible chunk size', { size });
        return null;
      }
      this._chunkSize = size;
    } catch (error) {
      logger.debug('Immutable database: chunk size could not be read', {
        reason: reasonOf(error),
      });
    }
    return this._chunkSize;
  }

  /**
   * The highest slot the immutable database holds.
   *
   * This is what separates "the index is lying" from "the block is newer than
   * the immutable database". The last k blocks live in `volatile/`, which is a
   * different on-disk format and is deliberately not read, so a pointer past
   * this tip is unresolved for now rather than wrong.
   */
  tipSlot(): number | null {
    if (this._tipChecked) return this._tipSlot;
    this._tipChecked = true;
    this._tipSlot = this._readTipSlot();
    return this._tipSlot;
  }

  private _readTipSlot(): number | null {
    try {
      const chunk = this._highestChunk();
      if (chunk === null) return null;
      for (let index = chunk; index >= 0; index -= 1) {
        const secondary = this._secondary(index);
        if (secondary && secondary.length >= IMMUTABLE_SECONDARY_ENTRY_BYTES) {
          const last = secondary.length - IMMUTABLE_SECONDARY_ENTRY_BYTES;
          return Number(
            secondary.readBigUInt64BE(last + SECONDARY_SLOT_OFFSET)
          );
        }
      }
      return null;
    } catch (error) {
      logger.debug('Immutable database: tip could not be read', {
        reason: reasonOf(error),
      });
      return null;
    }
  }

  /**
   * The block at `slot` whose header hash is `headerHash`.
   *
   * Both are required and both are checked. The slot alone would let an index
   * name a block the user has that is not the one it means; the hash alone
   * would need a scan of the whole database to find.
   */
  readBlock(slot: number, headerHash: string): ImmutableReadResult {
    const size = this.chunkSize();
    if (size === null) {
      return { status: 'unreadable', reason: 'chunk-size-unknown' };
    }
    if (!Number.isInteger(slot) || slot < 0) {
      return { status: 'unreadable', reason: 'slot-not-a-slot' };
    }

    const tip = this.tipSlot();
    if (tip === null) {
      return { status: 'unreadable', reason: 'tip-unknown' };
    }
    if (slot > tip) {
      return { status: 'beyond-immutable-tip', tipSlot: tip };
    }

    const chunk = Math.floor(slot / size);
    // Relative slot 0 is the epoch boundary block's, so a real slot is one
    // further along, and the entry for it is bounded by the next offset.
    const relative = (slot % size) + 1;

    try {
      const primary = this._primary(chunk);
      const secondary = this._secondary(chunk);
      if (!primary || !secondary) return { status: 'absent' };
      if (primary[0] !== IMMUTABLE_PRIMARY_INDEX_VERSION) {
        return { status: 'unreadable', reason: 'primary-index-version' };
      }
      if (secondary.length % IMMUTABLE_SECONDARY_ENTRY_BYTES !== 0) {
        return { status: 'unreadable', reason: 'secondary-index-length' };
      }

      const offsetsEnd = 1 + (relative + 2) * 4;
      if (primary.length < offsetsEnd) {
        // The chunk is present but does not reach that slot, which happens for
        // the partially written chunk at the tip.
        return { status: 'absent' };
      }
      const from = primary.readUInt32BE(1 + relative * 4);
      const to = primary.readUInt32BE(1 + (relative + 1) * 4);
      if (from === to) return { status: 'absent' };
      if (
        from % IMMUTABLE_SECONDARY_ENTRY_BYTES !== 0 ||
        from + IMMUTABLE_SECONDARY_ENTRY_BYTES > secondary.length
      ) {
        return { status: 'unreadable', reason: 'secondary-offset' };
      }

      const entrySlot = Number(
        secondary.readBigUInt64BE(from + SECONDARY_SLOT_OFFSET)
      );
      if (entrySlot !== slot) {
        // The primary index pointed somewhere that is not this slot, which
        // means the chunk size or the layout is not what this reader expects.
        return { status: 'unreadable', reason: 'slot-mismatch' };
      }

      const storedHash = secondary
        .subarray(
          from + SECONDARY_HASH_OFFSET,
          from + SECONDARY_HASH_OFFSET + SECONDARY_HASH_BYTES
        )
        .toString('hex');
      if (storedHash !== headerHash.toLowerCase()) return { status: 'absent' };

      const blockOffset = Number(secondary.readBigUInt64BE(from));
      const nextEntry = from + IMMUTABLE_SECONDARY_ENTRY_BYTES;
      const nextOffset =
        nextEntry + IMMUTABLE_SECONDARY_ENTRY_BYTES <= secondary.length
          ? Number(secondary.readBigUInt64BE(nextEntry))
          : null;

      const blocks = this._chunk(chunk);
      if (!blocks) return { status: 'unreadable', reason: 'chunk-missing' };
      const end = nextOffset === null ? blocks.length : nextOffset;
      if (blockOffset < 0 || end > blocks.length || end <= blockOffset) {
        return { status: 'unreadable', reason: 'block-bounds' };
      }

      return {
        status: 'found',
        bytes: blocks.subarray(blockOffset, end),
        headerHash: storedHash,
      };
    } catch (error) {
      return { status: 'unreadable', reason: reasonOf(error) };
    }
  }

  private _highestChunk(): number | null {
    const entries = fs.readdirSync(this._directory);
    let highest: number | null = null;
    entries.forEach((entry) => {
      const match = /^(\d{5})\.secondary$/.exec(entry);
      if (!match) return;
      const chunk = Number(match[1]);
      if (highest === null || chunk > highest) highest = chunk;
    });
    return highest;
  }

  private _primary(chunk: number): Buffer | null {
    return this._read(chunkName(chunk, 'primary'));
  }

  private _secondary(chunk: number): Buffer | null {
    return this._read(chunkName(chunk, 'secondary'));
  }

  private _chunk(chunk: number): Buffer | null {
    return this._read(chunkName(chunk, 'chunk'));
  }

  private _read(name: string): Buffer | null {
    try {
      return fs.readFileSync(path.join(this._directory, name));
    } catch {
      return null;
    }
  }
}
