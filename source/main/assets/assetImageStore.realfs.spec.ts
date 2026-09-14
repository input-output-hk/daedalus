/**
 * The image store against a real database file and a stubbed transport: the
 * single-subject logo query, media-type detection, the per-entry cap, and the
 * byte round trip.
 *
 * @jest-environment node
 */
import fs from 'fs';
import os from 'os';
import path from 'path';
import { openAssetMetadataDatabase } from './assetMetadataDb';
import type { AssetMetadataDatabase } from './assetMetadataDb';
import {
  ASSET_IMAGE_MAX_BYTES,
  ASSET_IMAGE_MAX_ENTRIES,
  ASSET_IMAGE_MAX_TOTAL_BYTES,
  ASSET_IMAGE_TOUCH_INTERVAL_MS,
  AssetImageStore,
  detectImageMediaType,
} from './assetImageStore';
import type {
  RegistryTransport,
  RegistryTransportResult,
} from './assetRegistryClient';

jest.mock('../config', () => ({
  launcherConfig: { metadataUrl: 'https://tokens.example' },
  MOCK_TOKEN_METADATA_SERVER_URL: 'http://127.0.0.1',
  MOCK_TOKEN_METADATA_SERVER_PORT: 41531,
  stateDirectoryPath: '/nonexistent',
}));

jest.mock('../environment', () => ({ environment: { isSelfnode: false } }));

jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

const SUBJECT =
  'c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b942544544';
const OTHER_SUBJECT = `${'a'.repeat(56)}beef`;
const NOW = 1_700_000_000_000;

// A PNG signature followed by bytes no text encoding round-trips, so a column
// that were not a BLOB would corrupt them.
const PNG = Buffer.from('89504e470d0a1a0a00ff7f8001', 'hex');
const JPEG = Buffer.from('ffd8ffe000104a464946', 'hex');
const GIF = Buffer.from('474946383961010001', 'hex');
const WEBP = Buffer.from('52494646000000005745425000', 'hex');
const SVG = Buffer.from('<svg xmlns="http://www.w3.org/2000/svg"></svg>');

let directory: string;
let database: AssetMetadataDatabase;

const stub = (
  answer: (subject: string) => RegistryTransportResult
): RegistryTransport & { calls: Array<string> } => {
  const state = {
    calls: [] as Array<string>,
    async post(_url: string, body: string): Promise<RegistryTransportResult> {
      state.calls.push(body);
      await Promise.resolve();
      return answer(JSON.parse(body).subjects[0]);
    },
  };
  return state;
};

const answering = (payload: Buffer | null, subject = SUBJECT) =>
  stub(() => ({
    ok: true,
    status: 200,
    body: JSON.stringify({
      subjects: [
        {
          subject,
          ...(payload
            ? {
                logo: {
                  value: payload.toString('base64'),
                  sequenceNumber: 0,
                  signatures: [],
                },
              }
            : {}),
        },
      ],
    }),
  }));

const seedMetadata = (subject = SUBJECT) => {
  database.writeMetadata(
    [
      {
        subject,
        policyId: subject.slice(0, 56),
        assetName: subject.slice(56),
        ticker: 'BTED',
        name: null,
        decimals: null,
        verified: false,
        metadata: null,
        source: 'registry',
        sequenceNumber: 0,
        slot: null,
      },
    ],
    NOW
  );
};

const storeWith = (transport: RegistryTransport) =>
  new AssetImageStore({
    database,
    transport,
    endpoint: 'https://tokens.example',
    now: () => NOW,
  });

beforeEach(() => {
  directory = fs.mkdtempSync(path.join(os.tmpdir(), 'asset-image-'));
  database = openAssetMetadataDatabase(
    path.join(directory, 'cache', 'assets.sqlite')
  );
  seedMetadata();
});

afterEach(() => {
  database.close();
  fs.rmSync(directory, { recursive: true, force: true });
});

describe('detectImageMediaType', () => {
  it.each([
    ['image/png', PNG],
    ['image/jpeg', JPEG],
    ['image/gif', GIF],
    ['image/webp', WEBP],
  ])('detects %s', (expected, bytes) => {
    expect(detectImageMediaType(new Uint8Array(bytes))).toBe(expected);
  });

  it('refuses markup, text and anything too short to carry a signature', () => {
    expect(detectImageMediaType(new Uint8Array(SVG))).toBeNull();
    expect(
      detectImageMediaType(new Uint8Array(Buffer.from('not an image')))
    ).toBeNull();
    expect(detectImageMediaType(new Uint8Array(0))).toBeNull();
    expect(
      detectImageMediaType(new Uint8Array(Buffer.from([0x89, 0x50])))
    ).toBeNull();
  });
});

describe('fetching a logo', () => {
  it('asks for one subject and only the logo property', async () => {
    const transport = answering(PNG);
    await storeWith(transport).fetch(SUBJECT);
    const body = JSON.parse(transport.calls[0]);
    expect(body.subjects).toEqual([SUBJECT]);
    expect(body.properties).toEqual(['logo']);
  });

  it('stores the decoded bytes with their detected media type', async () => {
    const store = storeWith(answering(PNG));
    const row = await store.fetch(SUBJECT);
    expect(row).toMatchObject({
      subject: SUBJECT,
      mediaType: 'image/png',
      byteLength: PNG.length,
      fetchedAt: NOW,
    });
    expect(Buffer.from(row.bytes).equals(PNG)).toBe(true);
  });

  it('round-trips bytes that are not valid text', async () => {
    const store = storeWith(answering(PNG));
    await store.fetch(SUBJECT);
    const stored = store.read(SUBJECT);
    expect(Buffer.from(stored.bytes).toString('hex')).toBe(PNG.toString('hex'));
  });

  it('answers from the database without a second request', async () => {
    const transport = answering(PNG);
    const store = storeWith(transport);
    await store.fetch(SUBJECT);
    await store.fetch(SUBJECT);
    expect(transport.calls).toHaveLength(1);
  });

  it('discards an entry over the per-entry cap', async () => {
    const oversized = Buffer.concat([
      Buffer.from('89504e470d0a1a0a', 'hex'),
      Buffer.alloc(ASSET_IMAGE_MAX_BYTES, 7),
    ]);
    const store = storeWith(answering(oversized));
    expect(await store.fetch(SUBJECT)).toBeNull();
    expect(store.read(SUBJECT)).toBeNull();
  });

  it('discards a payload that is not a raster image', async () => {
    const store = storeWith(answering(SVG));
    expect(await store.fetch(SUBJECT)).toBeNull();
    expect(store.read(SUBJECT)).toBeNull();
  });

  it('remembers a subject the registry answers without a logo', async () => {
    const transport = answering(null);
    const store = storeWith(transport);
    expect(await store.fetch(SUBJECT)).toBeNull();
    expect(await store.fetch(SUBJECT)).toBeNull();
    expect(transport.calls).toHaveLength(1);
  });

  it('refuses an entry for a different subject', async () => {
    const store = storeWith(answering(PNG, OTHER_SUBJECT));
    expect(await store.fetch(SUBJECT)).toBeNull();
    expect(store.read(SUBJECT)).toBeNull();
  });

  it.each([404, 500])('stores nothing on a %i', async (status) => {
    const transport = stub(() => ({ ok: true, status, body: '' }));
    const store = storeWith(transport);
    expect(await store.fetch(SUBJECT)).toBeNull();
    expect(transport.calls).toHaveLength(1);
  });

  it('stores nothing when the transport fails, and does not remember it', async () => {
    const transport = stub(() => ({ ok: false, reason: 'network' }));
    const store = storeWith(transport);
    expect(await store.fetch(SUBJECT)).toBeNull();
    await store.fetch(SUBJECT);
    expect(transport.calls).toHaveLength(2);
  });

  it('stores nothing when the response is not JSON', async () => {
    const store = storeWith(
      stub(() => ({ ok: true, status: 200, body: '{not json' }))
    );
    expect(await store.fetch(SUBJECT)).toBeNull();
  });

  it('shares one request between concurrent callers', async () => {
    const transport = answering(PNG);
    const store = storeWith(transport);
    const [first, second] = await Promise.all([
      store.fetch(SUBJECT),
      store.fetch(SUBJECT),
    ]);
    expect(transport.calls).toHaveLength(1);
    expect(first).not.toBeNull();
    expect(second).not.toBeNull();
  });

  it('answers nothing for an empty subject', async () => {
    const transport = answering(PNG);
    expect(await storeWith(transport).fetch('')).toBeNull();
    expect(transport.calls).toHaveLength(0);
  });
});

describe('the foreign key', () => {
  it('refuses an image for a subject the cache does not know', async () => {
    const transport = answering(PNG, OTHER_SUBJECT);
    const store = storeWith(transport);
    expect(await store.fetch(OTHER_SUBJECT)).toBeNull();
    expect(store.read(OTHER_SUBJECT)).toBeNull();
  });

  it('reports a refused write rather than throwing', () => {
    expect(
      database.writeImage(
        {
          subject: OTHER_SUBJECT,
          mediaType: 'image/png',
          bytes: new Uint8Array(PNG),
        },
        NOW
      )
    ).toBe(false);
    expect(
      database.writeImage(
        {
          subject: SUBJECT,
          mediaType: 'image/png',
          bytes: new Uint8Array(PNG),
        },
        NOW
      )
    ).toBe(true);
  });

  it('reads nothing for an absent subject', () => {
    expect(database.readImage(OTHER_SUBJECT)).toBeNull();
    expect(database.readImage('')).toBeNull();
  });
});

describe('eviction', () => {
  // Bounds are passed in at test size, so a case does not have to write 64 MiB
  // of blobs. The shipped constants are asserted separately.
  const IMAGE_BYTES = 100;

  const imageOf = (index: number) =>
    Buffer.concat([
      Buffer.from('89504e470d0a1a0a', 'hex'),
      Buffer.alloc(IMAGE_BYTES - 8, index),
    ]);

  const subjectOf = (index: number) =>
    `${index.toString(16).padStart(56, '0')}42544544`;

  const seed = (count: number) => {
    for (let index = 0; index < count; index += 1) {
      const subject = subjectOf(index);
      seedMetadata(subject);
      database.writeImage(
        {
          subject,
          mediaType: 'image/png',
          bytes: new Uint8Array(imageOf(index)),
        },
        NOW + index
      );
    }
  };

  const survivors = (count: number) =>
    Array.from({ length: count }, (_value, index) => subjectOf(index)).filter(
      (subject) => database.readImage(subject) !== null
    );

  it('evicts the oldest first when the entry bound is crossed', () => {
    seed(5);
    expect(database.enforceImageBounds(3, 1_000_000)).toBe(2);
    expect(survivors(5)).toEqual([subjectOf(2), subjectOf(3), subjectOf(4)]);
  });

  it('evicts nothing when the table is exactly at the entry bound', () => {
    seed(3);
    expect(database.enforceImageBounds(3, 1_000_000)).toBe(0);
    expect(survivors(3)).toHaveLength(3);
  });

  it('evicts until the byte bound is satisfied and no further', () => {
    seed(5);
    expect(database.enforceImageBounds(1_000, IMAGE_BYTES * 3)).toBe(2);
    expect(survivors(5)).toEqual([subjectOf(2), subjectOf(3), subjectOf(4)]);
  });

  it('evicts one when the byte bound admits four of five', () => {
    seed(5);
    expect(database.enforceImageBounds(1_000, IMAGE_BYTES * 4)).toBe(1);
    expect(survivors(5)).toHaveLength(4);
  });

  it('ends inside both bounds when both are crossed', () => {
    seed(6);
    expect(database.enforceImageBounds(4, IMAGE_BYTES * 2)).toBe(4);
    expect(survivors(6)).toEqual([subjectOf(4), subjectOf(5)]);
  });

  it('leaves the metadata rows untouched', () => {
    seed(5);
    database.enforceImageBounds(1, 1_000_000);
    expect(survivors(5)).toEqual([subjectOf(4)]);
    Array.from({ length: 5 }, (_value, index) => subjectOf(index)).forEach(
      (subject) => {
        expect(database.readMetadata([subject])).toHaveLength(1);
      }
    );
  });

  it('evicts nothing below the sweep floor, where neither bound can be crossed', () => {
    seed(5);
    // The floor is an optimisation and must not change the outcome, so it is
    // asserted by outcome rather than by instrumenting the query path: below
    // it, the shipped bounds cannot be crossed and nothing is evicted.
    expect(
      database.enforceImageBounds(
        ASSET_IMAGE_MAX_ENTRIES,
        ASSET_IMAGE_MAX_TOTAL_BYTES
      )
    ).toBe(0);
    expect(survivors(5)).toHaveLength(5);
  });

  it('derives the sweep floor from the two shipped constants', () => {
    expect(
      Math.floor(ASSET_IMAGE_MAX_TOTAL_BYTES / ASSET_IMAGE_MAX_BYTES)
    ).toBe(256);
    expect(ASSET_IMAGE_MAX_ENTRIES).toBe(2000);
    expect(ASSET_IMAGE_MAX_TOTAL_BYTES).toBe(64 * 1024 * 1024);
  });

  it('refuses a row over the per-entry cap, which is what the floor rests on', () => {
    seedMetadata(subjectOf(0));
    const oversized = Buffer.concat([
      Buffer.from('89504e470d0a1a0a', 'hex'),
      Buffer.alloc(ASSET_IMAGE_MAX_BYTES - 7),
    ]);
    expect(
      database.writeImage(
        {
          subject: subjectOf(0),
          mediaType: 'image/png',
          bytes: new Uint8Array(oversized),
        },
        NOW
      )
    ).toBe(false);
    expect(
      database.writeImage(
        {
          subject: subjectOf(0),
          mediaType: 'image/png',
          bytes: new Uint8Array(oversized.subarray(0, ASSET_IMAGE_MAX_BYTES)),
        },
        NOW
      )
    ).toBe(true);
  });

  it('enforces the bounds after a fetch stores an image', async () => {
    seed(3);
    const store = new AssetImageStore({
      database,
      transport: answering(PNG),
      endpoint: 'https://tokens.example',
      now: () => NOW + 100,
      maxEntries: 3,
      maxTotalBytes: 1_000_000,
    });
    await store.fetch(SUBJECT);
    expect(database.readImage(SUBJECT)).not.toBeNull();
    expect(survivors(3)).toEqual([subjectOf(1), subjectOf(2)]);
  });
});

describe('the eviction ordering', () => {
  const PAST = NOW - 10 * ASSET_IMAGE_TOUCH_INTERVAL_MS;

  it('advances fetched_at on a read of a stale row', () => {
    database.writeImage(
      { subject: SUBJECT, mediaType: 'image/png', bytes: new Uint8Array(PNG) },
      PAST
    );
    const store = storeWith(answering(PNG));
    store.read(SUBJECT);
    expect(database.readImage(SUBJECT).fetchedAt).toBe(NOW);
  });

  it('leaves fetched_at alone on a read inside the interval', () => {
    const recent = NOW - 60_000;
    database.writeImage(
      { subject: SUBJECT, mediaType: 'image/png', bytes: new Uint8Array(PNG) },
      recent
    );
    const store = storeWith(answering(PNG));
    store.read(SUBJECT);
    expect(database.readImage(SUBJECT).fetchedAt).toBe(recent);
  });

  it('keeps the recently read row rather than the recently written one', () => {
    const older = `${'1'.repeat(56)}01`;
    const newer = `${'2'.repeat(56)}02`;
    seedMetadata(older);
    seedMetadata(newer);
    database.writeImage(
      { subject: older, mediaType: 'image/png', bytes: new Uint8Array(PNG) },
      PAST
    );
    database.writeImage(
      { subject: newer, mediaType: 'image/png', bytes: new Uint8Array(PNG) },
      PAST + 1000
    );

    // Reading the older one moves it to the end of the ordering.
    storeWith(answering(PNG)).read(older);
    expect(database.enforceImageBounds(1, 1_000_000)).toBe(1);
    expect(database.readImage(older)).not.toBeNull();
    expect(database.readImage(newer)).toBeNull();
  });
});
