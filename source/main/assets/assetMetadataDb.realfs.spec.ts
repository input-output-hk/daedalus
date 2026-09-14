/**
 * The asset metadata cache against real files: the schema constraints as the
 * engine enforces them, the version check, recovery from an unusable file, and
 * the four accessors.
 *
 * @jest-environment node
 */
import fs from 'fs';
import os from 'os';
import path from 'path';
import { DatabaseSync } from 'node:sqlite';
import {
  ASSET_METADATA_DB_VERSION,
  assetMetadataDatabasePath,
  assetMetadataDirectoryPath,
  openAssetMetadataDatabase,
} from './assetMetadataDb';
import type {
  AssetMetadataWrite,
  AssetResolutionState,
} from './assetMetadataDb';

// main/config boots launcher configuration and throws outside an Electron
// launcher, so the state directory is redirected to a temp dir instead.
jest.mock('../config', () => {
  const nodeOs = require('os');
  const nodePath = require('path');
  return {
    stateDirectoryPath: nodePath.join(nodeOs.tmpdir(), 'asset-metadata-spec'),
  };
});

jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

const POLICY_ID = 'c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b9';
const ASSET_NAME = '42544544';
const SUBJECT = `${POLICY_ID}${ASSET_NAME}`;

const METADATA_COLUMNS =
  'subject, policy_id, asset_name, ticker, name, decimals, verified, metadata, source, sequence_number, slot, updated_at';

type RawMetadata = {
  subject?: string;
  policyId?: string;
  assetName?: string;
  ticker?: string | null;
  name?: string | null;
  decimals?: number | string | null;
  verified?: number;
  metadata?: string | null;
  source?: string;
  sequenceNumber?: number | null;
  slot?: number | null;
  updatedAt?: number;
};

let directory: string;
let databaseFile: string;

const metadataWrite = (
  overrides: Partial<AssetMetadataWrite> = {}
): AssetMetadataWrite => ({
  subject: SUBJECT,
  policyId: POLICY_ID,
  assetName: ASSET_NAME,
  ticker: 'BTED',
  name: 'BitEd Token',
  decimals: 0,
  verified: true,
  metadata: null,
  source: 'registry',
  sequenceNumber: 0,
  slot: null,
  ...overrides,
});

// The typed accessors cannot express a row that violates the schema: `verified`
// is a boolean there and `decimals` a number. These probes go straight at the
// engine, which is the only thing that can prove a CHECK is enforced rather
// than merely written down.
const rawInsertMetadata = (raw: RawMetadata = {}): void => {
  const db = new DatabaseSync(databaseFile);
  try {
    db.exec('PRAGMA foreign_keys = ON');
    db.prepare(
      `INSERT INTO asset_metadata (${METADATA_COLUMNS}) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`
    ).run(
      raw.subject ?? SUBJECT,
      raw.policyId ?? POLICY_ID,
      raw.assetName ?? ASSET_NAME,
      raw.ticker ?? null,
      raw.name ?? null,
      raw.decimals === undefined ? 0 : raw.decimals,
      raw.verified === undefined ? 1 : raw.verified,
      raw.metadata ?? null,
      raw.source ?? 'registry',
      raw.sequenceNumber === undefined ? 0 : raw.sequenceNumber,
      raw.slot === undefined ? null : raw.slot,
      raw.updatedAt ?? 1_700_000_000_000
    );
  } finally {
    db.close();
  }
};

const rawExec = (sql: string): void => {
  const db = new DatabaseSync(databaseFile);
  try {
    db.exec(sql);
  } finally {
    db.close();
  }
};

const rawUserVersion = (): number => {
  const db = new DatabaseSync(databaseFile);
  try {
    return Number(db.prepare('PRAGMA user_version').get()?.user_version);
  } finally {
    db.close();
  }
};

const createSchema = (): void => {
  const db = openAssetMetadataDatabase(databaseFile);
  db.close();
};

beforeEach(() => {
  directory = fs.mkdtempSync(path.join(os.tmpdir(), 'asset-metadata-db-'));
  databaseFile = path.join(directory, 'cache', 'assets.sqlite');
});

afterEach(() => {
  fs.rmSync(directory, { recursive: true, force: true });
});

describe('assetMetadataDatabasePath', () => {
  it('places the database in its own directory under the state directory', () => {
    expect(assetMetadataDirectoryPath()).toBe(
      path.join(os.tmpdir(), 'asset-metadata-spec', 'asset-metadata-cache')
    );
    expect(assetMetadataDatabasePath()).toBe(
      path.join(
        os.tmpdir(),
        'asset-metadata-spec',
        'asset-metadata-cache',
        'assets.sqlite'
      )
    );
  });
});

describe('asset_metadata constraints', () => {
  beforeEach(createSchema);

  it('accepts a subject that is the policy id followed by the asset name', () => {
    expect(() => rawInsertMetadata()).not.toThrow();
  });

  it('rejects a subject that is not the policy id followed by the asset name', () => {
    expect(() => rawInsertMetadata({ subject: `${POLICY_ID}ffff` })).toThrow(
      /CHECK constraint failed/
    );
  });

  it('accepts an empty asset name, where the subject is the policy id alone', () => {
    expect(() =>
      rawInsertMetadata({ subject: POLICY_ID, assetName: '' })
    ).not.toThrow();
  });

  it('rejects a verified value outside 0 and 1', () => {
    expect(() => rawInsertMetadata({ verified: 2 })).toThrow(
      /CHECK constraint failed/
    );
  });

  it('accepts both verified values', () => {
    expect(() => rawInsertMetadata({ verified: 0 })).not.toThrow();
    expect(() =>
      rawInsertMetadata({ subject: POLICY_ID, assetName: '', verified: 1 })
    ).not.toThrow();
  });

  it('rejects a decimals value of 21', () => {
    expect(() => rawInsertMetadata({ decimals: 21 })).toThrow(
      /CHECK constraint failed/
    );
  });

  it('rejects a negative decimals value', () => {
    expect(() => rawInsertMetadata({ decimals: -1 })).toThrow(
      /CHECK constraint failed/
    );
  });

  it('accepts the decimals boundaries and a null', () => {
    expect(() => rawInsertMetadata({ decimals: 0 })).not.toThrow();
    expect(() =>
      rawInsertMetadata({ subject: POLICY_ID, assetName: '', decimals: 20 })
    ).not.toThrow();
    expect(() =>
      rawInsertMetadata({
        subject: `${POLICY_ID}00`,
        assetName: '00',
        decimals: null,
      })
    ).not.toThrow();
  });

  it('rejects a text value in the decimals column under STRICT', () => {
    expect(() => rawInsertMetadata({ decimals: 'three' })).toThrow(
      /cannot store TEXT value in INTEGER column/
    );
  });

  it('rejects a source outside the two channels', () => {
    expect(() => rawInsertMetadata({ source: 'koios' })).toThrow(
      /CHECK constraint failed/
    );
  });

  it('rejects a registry row that carries a slot', () => {
    expect(() => rawInsertMetadata({ source: 'registry', slot: 42 })).toThrow(
      /CHECK constraint failed/
    );
  });

  it('rejects a chain row that carries a sequence number', () => {
    expect(() =>
      rawInsertMetadata({ source: 'chain', sequenceNumber: 0, slot: 42 })
    ).toThrow(/CHECK constraint failed/);
  });

  it('accepts a chain row that carries a slot and no sequence number', () => {
    expect(() =>
      rawInsertMetadata({ source: 'chain', sequenceNumber: null, slot: 42 })
    ).not.toThrow();
  });
});

describe('asset_image constraints', () => {
  beforeEach(createSchema);

  it('rejects an image for a subject with no metadata row, so the foreign key pragma is live', () => {
    const db = new DatabaseSync(databaseFile);
    try {
      db.exec('PRAGMA foreign_keys = ON');
      expect(() =>
        db
          .prepare(
            'INSERT INTO asset_image (subject, media_type, bytes, byte_length, fetched_at) VALUES (?, ?, ?, ?, ?)'
          )
          .run(SUBJECT, 'image/png', new Uint8Array([1, 2, 3]), 3, 1)
      ).toThrow(/FOREIGN KEY constraint failed/);
    } finally {
      db.close();
    }
  });
});

describe('asset_resolution', () => {
  it('round-trips every declared state', () => {
    const db = openAssetMetadataDatabase(databaseFile);
    const states: Array<AssetResolutionState> = [
      'pending',
      'resolved',
      'unregistered',
      'failed',
    ];
    const written = db.writeResolutions(
      states.map((state, index) => ({
        subject: `${index}`.padStart(64, '0'),
        state,
        retryAfter: index * 1000,
        failureCount: index,
      })),
      1_700_000_000_000
    );
    expect(written).toBe(4);
    const read = db.readResolutions(
      states.map((_state, index) => `${index}`.padStart(64, '0'))
    );
    expect(read.map((row) => row.state).sort()).toEqual([...states].sort());
    expect(read.every((row) => row.attemptedAt === 1_700_000_000_000)).toBe(
      true
    );
    db.close();
  });

  it('rejects a state the schema does not declare', () => {
    createSchema();
    expect(() =>
      rawExec(
        "INSERT INTO asset_resolution (subject, state, attempted_at, retry_after, failure_count) VALUES ('x', 'stalled', 1, 1, 0)"
      )
    ).toThrow(/CHECK constraint failed/);
  });
});

describe('opening the database', () => {
  it('creates the directory, the file and the version stamp', () => {
    const db = openAssetMetadataDatabase(databaseFile);
    db.close();
    expect(fs.existsSync(databaseFile)).toBe(true);
    expect(rawUserVersion()).toBe(ASSET_METADATA_DB_VERSION);
  });

  it('preserves rows across a reopen', () => {
    const first = openAssetMetadataDatabase(databaseFile);
    expect(first.writeMetadata([metadataWrite()])).toBe(1);
    first.close();

    const second = openAssetMetadataDatabase(databaseFile);
    expect(second.readMetadata([SUBJECT])).toHaveLength(1);
    second.close();
  });

  it('recreates a database whose version is ahead of this code', () => {
    const first = openAssetMetadataDatabase(databaseFile);
    first.writeMetadata([metadataWrite()]);
    first.close();
    rawExec(`PRAGMA user_version = ${ASSET_METADATA_DB_VERSION + 1}`);

    const second = openAssetMetadataDatabase(databaseFile);
    expect(second.readMetadata([SUBJECT])).toEqual([]);
    second.close();
    expect(rawUserVersion()).toBe(ASSET_METADATA_DB_VERSION);
  });

  it('recreates a database stamped with any other version', () => {
    const first = openAssetMetadataDatabase(databaseFile);
    first.writeMetadata([metadataWrite()]);
    first.close();
    rawExec('PRAGMA user_version = 97');

    const second = openAssetMetadataDatabase(databaseFile);
    expect(second.readMetadata([SUBJECT])).toEqual([]);
    second.close();
    expect(rawUserVersion()).toBe(ASSET_METADATA_DB_VERSION);
  });

  it('recreates a file that is not a database', () => {
    const first = openAssetMetadataDatabase(databaseFile);
    first.writeMetadata([metadataWrite()]);
    first.close();
    fs.writeFileSync(databaseFile, 'this is not a database');

    const second = openAssetMetadataDatabase(databaseFile);
    expect(second.readMetadata([SUBJECT])).toEqual([]);
    expect(second.writeMetadata([metadataWrite()])).toBe(1);
    second.close();
  });

  it('removes the write-ahead log siblings when it recreates', () => {
    const first = openAssetMetadataDatabase(databaseFile);
    first.writeMetadata([metadataWrite()]);
    first.close();
    fs.writeFileSync(`${databaseFile}-wal`, 'stale journal');
    fs.writeFileSync(databaseFile, 'this is not a database');

    const second = openAssetMetadataDatabase(databaseFile);
    expect(second.readMetadata([SUBJECT])).toEqual([]);
    second.close();
  });

  it('gives a working empty cache after the directory is deleted underneath it', () => {
    const first = openAssetMetadataDatabase(databaseFile);
    expect(first.writeMetadata([metadataWrite()])).toBe(1);
    fs.rmSync(path.dirname(databaseFile), { recursive: true, force: true });
    first.close();

    const second = openAssetMetadataDatabase(databaseFile);
    expect(second.readMetadata([SUBJECT])).toEqual([]);
    expect(second.writeMetadata([metadataWrite()])).toBe(1);
    expect(second.readMetadata([SUBJECT])).toHaveLength(1);
    second.close();
  });

  it('answers as an empty cache when the path cannot be created', () => {
    fs.mkdirSync(path.dirname(path.dirname(databaseFile)), { recursive: true });
    fs.writeFileSync(
      path.dirname(databaseFile),
      'a file where a directory goes'
    );

    const db = openAssetMetadataDatabase(databaseFile);
    expect(db.readMetadata([SUBJECT])).toEqual([]);
    expect(db.readResolutions([SUBJECT])).toEqual([]);
    expect(db.writeMetadata([metadataWrite()])).toBe(0);
    expect(db.writeResolutions([])).toBe(0);
    expect(() => db.close()).not.toThrow();
  });
});

describe('reading and writing', () => {
  it('returns only the subjects asked for', () => {
    const db = openAssetMetadataDatabase(databaseFile);
    const other = `${'0'.repeat(56)}beef`;
    db.writeMetadata([
      metadataWrite(),
      metadataWrite({
        subject: other,
        policyId: '0'.repeat(56),
        assetName: 'beef',
        ticker: 'OTHER',
      }),
    ]);
    const read = db.readMetadata([SUBJECT]);
    expect(read).toHaveLength(1);
    expect(read[0].ticker).toBe('BTED');
    db.close();
  });

  it('round-trips every column, including the booleans and the nulls', () => {
    const db = openAssetMetadataDatabase(databaseFile);
    db.writeMetadata(
      [
        metadataWrite({
          ticker: null,
          name: null,
          decimals: null,
          verified: false,
          metadata: '{"url":"https://bit-ed.org/"}',
        }),
      ],
      1_700_000_000_000
    );
    expect(db.readMetadata([SUBJECT])[0]).toEqual({
      subject: SUBJECT,
      policyId: POLICY_ID,
      assetName: ASSET_NAME,
      ticker: null,
      name: null,
      decimals: null,
      verified: false,
      metadata: '{"url":"https://bit-ed.org/"}',
      source: 'registry',
      sequenceNumber: 0,
      slot: null,
      updatedAt: 1_700_000_000_000,
    });
    db.close();
  });

  it('replaces a subject rather than duplicating it', () => {
    const db = openAssetMetadataDatabase(databaseFile);
    db.writeMetadata([metadataWrite({ ticker: 'FIRST' })]);
    db.writeMetadata([metadataWrite({ ticker: 'SECOND' })]);
    const read = db.readMetadata([SUBJECT]);
    expect(read).toHaveLength(1);
    expect(read[0].ticker).toBe('SECOND');
    db.close();
  });

  it('writes the rest of a batch when one row violates a constraint', () => {
    const db = openAssetMetadataDatabase(databaseFile);
    const before = metadataWrite({
      subject: `${'1'.repeat(56)}01`,
      policyId: '1'.repeat(56),
      assetName: '01',
    });
    const offending = metadataWrite({
      subject: `${'2'.repeat(56)}02`,
      policyId: '2'.repeat(56),
      assetName: '02',
      decimals: 21,
    });
    const after = metadataWrite({
      subject: `${'3'.repeat(56)}03`,
      policyId: '3'.repeat(56),
      assetName: '03',
    });

    expect(db.writeMetadata([before, offending, after])).toBe(2);
    expect(
      db
        .readMetadata([before.subject, offending.subject, after.subject])
        .map((row) => row.subject)
        .sort()
    ).toEqual([before.subject, after.subject].sort());
    db.close();
  });

  it('answers an empty list and an unknown subject without throwing', () => {
    const db = openAssetMetadataDatabase(databaseFile);
    expect(db.readMetadata([])).toEqual([]);
    expect(db.readResolutions([])).toEqual([]);
    expect(db.readMetadata([SUBJECT])).toEqual([]);
    expect(db.writeMetadata([])).toBe(0);
    db.close();
  });

  it('joins the results of every chunk when the subject list is long', () => {
    const db = openAssetMetadataDatabase(databaseFile);
    const rows = Array.from({ length: 600 }, (_value, index) => {
      const policyId = index.toString(16).padStart(56, '0');
      return metadataWrite({
        subject: `${policyId}${ASSET_NAME}`,
        policyId,
      });
    });
    expect(db.writeMetadata(rows)).toBe(600);
    expect(db.readMetadata(rows.map((row) => row.subject))).toHaveLength(600);
    db.close();
  });

  it('names the subjects that have an image without reading one', () => {
    const db = openAssetMetadataDatabase(databaseFile);
    const other = `${'0'.repeat(56)}beef`;
    db.writeMetadata([
      metadataWrite(),
      metadataWrite({
        subject: other,
        policyId: '0'.repeat(56),
        assetName: 'beef',
      }),
    ]);
    db.writeImage(
      {
        subject: SUBJECT,
        mediaType: 'image/png',
        bytes: new Uint8Array([1, 2, 3]),
      },
      1_700_000_000_000
    );
    expect(db.readImageSubjects([SUBJECT, other])).toEqual([SUBJECT]);
    db.close();
  });

  it('answers with no subjects when nothing has an image', () => {
    const db = openAssetMetadataDatabase(databaseFile);
    db.writeMetadata([metadataWrite()]);
    expect(db.readImageSubjects([SUBJECT])).toEqual([]);
    expect(db.readImageSubjects([])).toEqual([]);
    db.close();
  });

  it('stamps updated_at from the clock unless it is given one', () => {
    const db = openAssetMetadataDatabase(databaseFile);
    const before = Date.now();
    db.writeMetadata([metadataWrite()]);
    const stamped = db.readMetadata([SUBJECT])[0].updatedAt;
    expect(stamped).toBeGreaterThanOrEqual(before);
    expect(stamped).toBeLessThanOrEqual(Date.now());

    db.writeMetadata([metadataWrite()], 1_600_000_000_000);
    expect(db.readMetadata([SUBJECT])[0].updatedAt).toBe(1_600_000_000_000);
    db.close();
  });
});
