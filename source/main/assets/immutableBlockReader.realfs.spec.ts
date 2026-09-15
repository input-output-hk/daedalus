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
};

const write = (layout: Layout = {}): string => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'immutable-'));
  directories.push(directory);

  const version = layout.primaryVersion ?? IMMUTABLE_PRIMARY_INDEX_VERSION;
  const offsets = layout.chunkZeroOffsets ?? CHUNK_SIZE + 2;
  const slot = layout.slot ?? PREPROD_BLOCK.slot;
  const headerHash = layout.headerHash ?? PREPROD_BLOCK.hash;

  const zero = Buffer.alloc(1 + offsets * 4);
  zero[0] = version;
  fs.writeFileSync(path.join(directory, '00000.primary'), zero);

  const chunk = Math.floor(slot / CHUNK_SIZE);
  const relative = (slot % CHUNK_SIZE) + 1;
  const primary = Buffer.alloc(1 + (CHUNK_SIZE + 2) * 4);
  primary[0] = version;
  for (let index = relative + 1; index <= CHUNK_SIZE + 1; index += 1) {
    primary.writeUInt32BE(IMMUTABLE_SECONDARY_ENTRY_BYTES, 1 + index * 4);
  }

  const secondary = Buffer.alloc(
    IMMUTABLE_SECONDARY_ENTRY_BYTES + (layout.secondaryTrailingBytes ?? 0)
  );
  secondary.writeBigUInt64BE(BigInt(0), 0);
  Buffer.from(headerHash, 'hex').copy(secondary, 16);
  secondary.writeBigUInt64BE(BigInt(slot), 48);

  const name = String(chunk).padStart(5, '0');
  fs.writeFileSync(path.join(directory, `${name}.primary`), primary);
  fs.writeFileSync(path.join(directory, `${name}.secondary`), secondary);
  if (!layout.omitChunkFile) {
    fs.writeFileSync(path.join(directory, `${name}.chunk`), BLOCK);
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
});
