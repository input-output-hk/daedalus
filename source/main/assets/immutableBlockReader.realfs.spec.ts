/**
 * The immutable database reader, against databases written to a temporary
 * directory.
 *
 * The format is undocumented and internal to the node, so what these cases pin
 * is not "the reader agrees with the writer here": the writer here is the same
 * understanding as the reader. What makes that understanding checkable is the
 * recorded block in `chainPointer.fixture.ts`, whose slot and header hash were
 * confirmed against an independent index, and the case below that reads it back
 * through the layout this module documents.
 *
 * Everything else here is about failing closed.
 */
import fs from 'fs';
import os from 'os';
import path from 'path';

import {
  IMMUTABLE_PRIMARY_INDEX_VERSION,
  IMMUTABLE_SECONDARY_ENTRY_BYTES,
  ImmutableBlockReader,
  immutableDirectoryPath,
  resolveChainPath,
} from './immutableBlockReader';
import { PREPROD_BLOCK, PREPROD_BLOCK_HEX } from './chainPointer.fixture';

const CHUNK_SIZE = 21600;
const BLOCK = Buffer.from(PREPROD_BLOCK_HEX, 'hex');

let directories: Array<string> = [];

type Layout = {
  primaryVersion?: number;
  chunkZeroOffsets?: number;
  secondaryTrailingBytes?: number;
  slot?: number;
  headerHash?: string;
  omitChunkFile?: boolean;
  /** Written into the secondary entry instead of the slot the index points at. */
  entrySlot?: number;
  /** Where the primary index says this slot's secondary entry begins. */
  entryOffset?: number;
  /** Written into the secondary entry as the block's offset in the chunk. */
  blockOffset?: number;
  /** An extra, empty chunk above the one holding the block. */
  emptyChunkAbove?: boolean;
  /** A version byte on the block's own chunk that chunk zero does not have. */
  chunkPrimaryVersion?: number;
  /** Bytes appended to chunk zero's primary index, breaking its arithmetic. */
  chunkZeroTrailingBytes?: number;
  /** Stop the primary index short of the slot being asked for. */
  truncatePrimaryTo?: number;
  /** Leave every secondary index empty, so no tip can be found. */
  emptySecondary?: boolean;
};

const write = (layout: Layout = {}): string => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'immutable-'));
  directories.push(directory);

  const version = layout.primaryVersion ?? IMMUTABLE_PRIMARY_INDEX_VERSION;
  const offsets = layout.chunkZeroOffsets ?? CHUNK_SIZE + 2;
  const slot = layout.slot ?? PREPROD_BLOCK.slot;
  const headerHash = layout.headerHash ?? PREPROD_BLOCK.hash;

  const zero = Buffer.alloc(
    1 + offsets * 4 + (layout.chunkZeroTrailingBytes ?? 0)
  );
  zero[0] = version;
  fs.writeFileSync(path.join(directory, '00000.primary'), zero);

  const chunk = Math.floor(slot / CHUNK_SIZE);
  const relative = (slot % CHUNK_SIZE) + 1;
  const primary = Buffer.alloc(
    layout.truncatePrimaryTo === undefined
      ? 1 + (CHUNK_SIZE + 2) * 4
      : 1 + layout.truncatePrimaryTo * 4
  );
  primary[0] = layout.chunkPrimaryVersion ?? version;
  const start = layout.entryOffset ?? 0;
  const end = start + IMMUTABLE_SECONDARY_ENTRY_BYTES;
  // A truncated index stops early, which is the state of the chunk being
  // written at the tip.
  const put = (index: number, value: number) => {
    if (1 + index * 4 + 4 <= primary.length) {
      primary.writeUInt32BE(value, 1 + index * 4);
    }
  };
  for (let index = 1; index <= relative; index += 1) put(index, start);
  for (let index = relative + 1; index <= CHUNK_SIZE + 1; index += 1) {
    put(index, end);
  }

  const secondary = Buffer.alloc(
    layout.emptySecondary
      ? 0
      : IMMUTABLE_SECONDARY_ENTRY_BYTES + (layout.secondaryTrailingBytes ?? 0)
  );
  if (!layout.emptySecondary) {
    secondary.writeBigUInt64BE(BigInt(layout.blockOffset ?? 0), 0);
    Buffer.from(headerHash, 'hex').copy(secondary, 16);
    secondary.writeBigUInt64BE(BigInt(layout.entrySlot ?? slot), 48);
  }

  const name = String(chunk).padStart(5, '0');
  fs.writeFileSync(path.join(directory, `${name}.primary`), primary);
  fs.writeFileSync(path.join(directory, `${name}.secondary`), secondary);
  if (!layout.omitChunkFile) {
    fs.writeFileSync(path.join(directory, `${name}.chunk`), BLOCK);
  }
  if (layout.emptyChunkAbove) {
    const above = String(chunk + 1).padStart(5, '0');
    fs.writeFileSync(
      path.join(directory, `${above}.primary`),
      Buffer.from([1])
    );
    fs.writeFileSync(
      path.join(directory, `${above}.secondary`),
      Buffer.alloc(0)
    );
    fs.writeFileSync(path.join(directory, `${above}.chunk`), Buffer.alloc(0));
  }
  return directory;
};

afterEach(() => {
  directories.forEach((directory) => {
    try {
      fs.rmSync(directory, { recursive: true, force: true });
    } catch {
      // Nothing to clean is the desired end state.
    }
  });
  directories = [];
});

describe('immutableDirectoryPath', () => {
  it('names the immutable directory under a chain path', () => {
    expect(immutableDirectoryPath(path.join('state', 'chain'))).toBe(
      path.join('state', 'chain', 'immutable')
    );
  });
});

describe('ImmutableBlockReader', () => {
  it('derives the chunk size from chunk zero', () => {
    expect(new ImmutableBlockReader(write()).chunkSize()).toBe(CHUNK_SIZE);
  });

  it('reads the tip from the highest chunk it holds', () => {
    expect(new ImmutableBlockReader(write()).tipSlot()).toBe(
      PREPROD_BLOCK.slot
    );
  });

  it('returns the block at a slot and header hash it holds', () => {
    const result = new ImmutableBlockReader(write()).readBlock(
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash
    );
    expect(result.status).toBe('found');
    if (result.status !== 'found') return;
    expect(Buffer.from(result.bytes).equals(BLOCK)).toBe(true);
    expect(result.headerHash).toBe(PREPROD_BLOCK.hash);
  });

  it('accepts a header hash in upper case', () => {
    const result = new ImmutableBlockReader(write()).readBlock(
      PREPROD_BLOCK.slot,
      PREPROD_BLOCK.hash.toUpperCase()
    );
    expect(result.status).toBe('found');
  });

  it('answers absent for a slot it covers and does not hold', () => {
    const reader = new ImmutableBlockReader(write());
    expect(
      reader.readBlock(PREPROD_BLOCK.slot - 1, PREPROD_BLOCK.hash)
    ).toEqual({ status: 'absent' });
  });

  it('answers absent when the hash at that slot is a different block', () => {
    const reader = new ImmutableBlockReader(write());
    expect(reader.readBlock(PREPROD_BLOCK.slot, 'd'.repeat(64))).toEqual({
      status: 'absent',
    });
  });

  it('answers beyond the tip rather than absent for a newer slot', () => {
    const reader = new ImmutableBlockReader(write());
    expect(
      reader.readBlock(PREPROD_BLOCK.slot + 1, PREPROD_BLOCK.hash)
    ).toEqual({ status: 'beyond-immutable-tip', tipSlot: PREPROD_BLOCK.slot });
  });

  it('fails closed on a primary index version it has not been read against', () => {
    const reader = new ImmutableBlockReader(write({ primaryVersion: 9 }));
    expect(reader.chunkSize()).toBeNull();
    expect(reader.readBlock(PREPROD_BLOCK.slot, PREPROD_BLOCK.hash)).toEqual({
      status: 'unreadable',
      reason: 'chunk-size-unknown',
    });
  });

  it('fails closed on a chunk size that could not be one', () => {
    const reader = new ImmutableBlockReader(write({ chunkZeroOffsets: 2 }));
    expect(reader.chunkSize()).toBeNull();
  });

  it('fails closed on a secondary index that is not whole entries', () => {
    const reader = new ImmutableBlockReader(
      write({ secondaryTrailingBytes: 7 })
    );
    expect(reader.readBlock(PREPROD_BLOCK.slot, PREPROD_BLOCK.hash)).toEqual({
      status: 'unreadable',
      reason: 'secondary-index-length',
    });
  });

  it('fails closed when the chunk file behind the index is gone', () => {
    const reader = new ImmutableBlockReader(write({ omitChunkFile: true }));
    expect(reader.readBlock(PREPROD_BLOCK.slot, PREPROD_BLOCK.hash)).toEqual({
      status: 'unreadable',
      reason: 'chunk-missing',
    });
  });

  it('fails closed on a slot that is not a slot', () => {
    const reader = new ImmutableBlockReader(write());
    expect(reader.readBlock(-1, PREPROD_BLOCK.hash)).toEqual({
      status: 'unreadable',
      reason: 'slot-not-a-slot',
    });
    expect(reader.readBlock(1.5, PREPROD_BLOCK.hash)).toEqual({
      status: 'unreadable',
      reason: 'slot-not-a-slot',
    });
  });

  it('fails closed when there is no database at all', () => {
    const reader = new ImmutableBlockReader(
      path.join(os.tmpdir(), 'immutable-absent-directory')
    );
    expect(reader.chunkSize()).toBeNull();
    expect(reader.tipSlot()).toBeNull();
    expect(reader.readBlock(PREPROD_BLOCK.slot, PREPROD_BLOCK.hash)).toEqual({
      status: 'unreadable',
      reason: 'chunk-size-unknown',
    });
  });

  it('reads the tip once and answers from the reading afterwards', () => {
    const directory = write();
    const reader = new ImmutableBlockReader(directory);
    expect(reader.tipSlot()).toBe(PREPROD_BLOCK.slot);
    fs.rmSync(directory, { recursive: true, force: true });
    // The directory is gone and the answer is unchanged, which is what says the
    // listing happens once per reader rather than once per block.
    expect(reader.tipSlot()).toBe(PREPROD_BLOCK.slot);
  });

  it('fails closed when the index points at a different slot', () => {
    // The entry's own slot is above the one asked for, so the database still
    // covers the query and the mismatch is the only thing wrong.
    const reader = new ImmutableBlockReader(
      write({ entrySlot: PREPROD_BLOCK.slot + 5 })
    );
    expect(reader.readBlock(PREPROD_BLOCK.slot, PREPROD_BLOCK.hash)).toEqual({
      status: 'unreadable',
      reason: 'slot-mismatch',
    });
  });

  it('fails closed on an offset that is not a whole entry', () => {
    const reader = new ImmutableBlockReader(write({ entryOffset: 7 }));
    expect(reader.readBlock(PREPROD_BLOCK.slot, PREPROD_BLOCK.hash)).toEqual({
      status: 'unreadable',
      reason: 'secondary-offset',
    });
  });

  it('fails closed on a block offset past the end of its chunk', () => {
    const reader = new ImmutableBlockReader(
      write({ blockOffset: BLOCK.length + 1 })
    );
    expect(reader.readBlock(PREPROD_BLOCK.slot, PREPROD_BLOCK.hash)).toEqual({
      status: 'unreadable',
      reason: 'block-bounds',
    });
  });

  // The chunk at the tip is written before it holds anything, so the highest
  // chunk on disk can be empty and the tip is in the one below it.
  it('reads the tip from a lower chunk when the highest is empty', () => {
    const reader = new ImmutableBlockReader(write({ emptyChunkAbove: true }));
    expect(reader.tipSlot()).toBe(PREPROD_BLOCK.slot);
  });

  it('fails closed when no chunk holds a tip', () => {
    const reader = new ImmutableBlockReader(write({ emptySecondary: true }));
    expect(reader.chunkSize()).toBe(CHUNK_SIZE);
    expect(reader.tipSlot()).toBeNull();
    expect(reader.readBlock(PREPROD_BLOCK.slot, PREPROD_BLOCK.hash)).toEqual({
      status: 'unreadable',
      reason: 'tip-unknown',
    });
  });

  it('fails closed on a chunk whose own version byte it does not know', () => {
    const reader = new ImmutableBlockReader(write({ chunkPrimaryVersion: 3 }));
    expect(reader.readBlock(PREPROD_BLOCK.slot, PREPROD_BLOCK.hash)).toEqual({
      status: 'unreadable',
      reason: 'primary-index-version',
    });
  });

  it('fails closed on a primary index that is not a whole number of offsets', () => {
    const reader = new ImmutableBlockReader(
      write({ chunkZeroTrailingBytes: 2 })
    );
    expect(reader.chunkSize()).toBeNull();
  });

  // The chunk being written at the tip has an index only as far as its last
  // block, so a slot beyond that is absent rather than unreadable.
  it('answers absent for a slot the chunk index does not reach', () => {
    const reader = new ImmutableBlockReader(write({ truncatePrimaryTo: 4 }));
    expect(reader.readBlock(PREPROD_BLOCK.slot, PREPROD_BLOCK.hash)).toEqual({
      status: 'absent',
    });
  });
});

describe('resolveChainPath', () => {
  it('uses the state directory when the user has not moved the chain', () => {
    expect(resolveChainPath('state', null)).toBe(path.join('state', 'chain'));
  });

  it('uses the custom path when the user has moved the chain', () => {
    expect(resolveChainPath('state', 'elsewhere')).toBe(
      path.join('elsewhere', 'chain')
    );
  });
});
