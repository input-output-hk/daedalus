/**
 * The resolver against a real database file and a stubbed transport: answering
 * from disk, deciding what is due, verifying per property, writing, refreshing
 * and emitting.
 *
 * @jest-environment node
 */
import fs from 'fs';
import os from 'os';
import path from 'path';
import { openAssetMetadataDatabase } from './assetMetadataDb';
import type { AssetMetadataDatabase } from './assetMetadataDb';
import {
  ASSET_METADATA_REFRESH_MS,
  AssetMetadataResolver,
  chainPointerToRow,
  openAssetMetadataResolver,
} from './assetMetadataResolver';
import type {
  RegistryTransport,
  RegistryTransportResult,
} from './assetRegistryClient';
import type { HttpTransport, HttpTransportResult } from './httpTransport';
import { KoiosRequestBudget } from './koiosClient';
import {
  IMMUTABLE_PRIMARY_INDEX_VERSION,
  IMMUTABLE_SECONDARY_ENTRY_BYTES,
} from './immutableBlockReader';
import { arraySpans, mapSpans, readHead } from './cborSpan';
import { PREPROD_BLOCK, PREPROD_BLOCK_HEX } from './chainPointer.fixture';

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

const BTED = {
  subject: 'c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b942544544',
  policy:
    '820182018282051a0303eb448200581c39a1df51147b6de6689a4727846962fb6540c3a3c7859a1a79b9420f',
  publicKey: '5817526d712f71e33a31ac3429fb7ce70b3e17e727044d9a2a51493e7894ba48',
  tickerSignature:
    '68a722e7aa51d7d36baae9cbaadbf7c632f41596e2ed4030a4bacd58643a81d6ed8cb6f79b585e63c58e9df6b2f88f5a9d66248ad0eb624c86709e0abe40cd0a',
  decimalsSignature:
    '622bf53a1ba2891cf75c6b44394cad40597213a854de0d2dbcc0d1cd31767776afdcb069b744bf54dec4e3f3486d1ba5872dae07ead7288900acc5b684b0b10b',
  nameSignature:
    '9a5e7bb00e1b4e2c9d4d40d894ba6f019400ca2f4138f65d5d8bd7ef9c5a9143f837428c29c40af9757da139e37d8eced1973e9d029700c9d46073ab22476e09',
};

const OTHER_SUBJECT = `${'a'.repeat(56)}beef`;
const NOW = 1_700_000_000_000;

type Overrides = {
  decimalsValue?: unknown;
  decimalsSignature?: string;
  tickerSignature?: string;
  policy?: string | null;
  sequenceNumber?: number;
  omitDecimals?: boolean;
};

const bted = (overrides: Overrides = {}) => {
  const sequenceNumber = overrides.sequenceNumber ?? 0;
  const entry: Record<string, unknown> = {
    subject: BTED.subject,
    policy: overrides.policy === undefined ? BTED.policy : overrides.policy,
    name: {
      value: 'BitEd Token',
      sequenceNumber,
      signatures: [
        { signature: BTED.nameSignature, publicKey: BTED.publicKey },
      ],
    },
    ticker: {
      value: 'BTED',
      sequenceNumber,
      signatures: [
        {
          signature: overrides.tickerSignature ?? BTED.tickerSignature,
          publicKey: BTED.publicKey,
        },
      ],
    },
    url: {
      value: 'https://bit-ed.org/',
      sequenceNumber,
      signatures: [{ signature: 'ff'.repeat(64), publicKey: BTED.publicKey }],
    },
  };
  if (!overrides.omitDecimals) {
    entry.decimals = {
      value:
        overrides.decimalsValue === undefined ? 0 : overrides.decimalsValue,
      sequenceNumber,
      signatures: [
        {
          signature: overrides.decimalsSignature ?? BTED.decimalsSignature,
          publicKey: BTED.publicKey,
        },
      ],
    };
  }
  return entry;
};

const transportFor = (
  answer: (subjects: Array<string>) => Array<Record<string, unknown>>
): RegistryTransport & { calls: number } => {
  const stub = {
    calls: 0,
    async post(_url: string, body: string): Promise<RegistryTransportResult> {
      stub.calls += 1;
      const { subjects } = JSON.parse(body);
      return {
        ok: true,
        status: 200,
        body: JSON.stringify({ subjects: answer(subjects) }),
      };
    },
  };
  return stub;
};

const failingTransport = (): RegistryTransport & { calls: number } => {
  const stub = {
    calls: 0,
    async post(): Promise<RegistryTransportResult> {
      stub.calls += 1;
      return { ok: false, reason: 'network' };
    },
  };
  return stub;
};

let directory: string;
let database: AssetMetadataDatabase;

const resolverWith = (
  transport: RegistryTransport,
  extra: Record<string, unknown> = {}
) =>
  new AssetMetadataResolver({
    database,
    transport,
    endpoint: 'https://tokens.example',
    now: () => NOW,
    retryBackoffMs: 0,
    ...extra,
  });

beforeEach(() => {
  directory = fs.mkdtempSync(path.join(os.tmpdir(), 'asset-resolver-'));
  database = openAssetMetadataDatabase(
    path.join(directory, 'cache', 'assets.sqlite')
  );
});

afterEach(() => {
  database.close();
  fs.rmSync(directory, { recursive: true, force: true });
});

const storedRow = (subject = BTED.subject) =>
  database.readMetadata([subject])[0];

describe('reading', () => {
  it('answers a cached subject without touching the transport', async () => {
    const transport = transportFor(() => [bted()]);
    const resolver = resolverWith(transport);
    await resolver.resolve([BTED.subject]);
    expect(transport.calls).toBe(1);

    const rows = resolver.readCached([BTED.subject]);
    expect(rows).toHaveLength(1);
    expect(rows[0].ticker).toBe('BTED');
    expect(transport.calls).toBe(1);
  });

  it('answers absent for an uncached subject and schedules a fetch', async () => {
    const transport = transportFor(() => [bted()]);
    const resolver = resolverWith(transport);
    expect(resolver.request([BTED.subject])).toEqual([]);
    await resolver.pending();
    expect(transport.calls).toBe(1);
    expect(storedRow().ticker).toBe('BTED');
  });

  it('makes no call for an empty subject list', async () => {
    const transport = transportFor(() => []);
    const resolver = resolverWith(transport);
    expect(resolver.readCached([])).toEqual([]);
    expect(resolver.request([])).toEqual([]);
    await resolver.pending();
    expect(transport.calls).toBe(0);
  });

  it('answers from disk with the transport unavailable', async () => {
    const good = transportFor(() => [bted()]);
    await resolverWith(good).resolve([BTED.subject]);

    const broken = failingTransport();
    const resolver = resolverWith(broken);
    expect(resolver.readCached([BTED.subject])).toHaveLength(1);
    expect(broken.calls).toBe(0);
  });
});

describe('the due rule', () => {
  it('does not fetch a subject whose row is fresh', async () => {
    const transport = transportFor(() => [bted()]);
    await resolverWith(transport).resolve([BTED.subject]);

    const second = transportFor(() => [bted()]);
    const resolver = resolverWith(second);
    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(second.calls).toBe(0);
  });

  it('fetches either side of the refresh boundary and not before it', async () => {
    const writeAt = (age: number) => {
      database.writeMetadata(
        [
          {
            subject: BTED.subject,
            policyId: BTED.subject.slice(0, 56),
            assetName: BTED.subject.slice(56),
            ticker: 'OLD',
            name: null,
            decimals: null,
            verified: false,
            metadata: null,
            source: 'registry',
            sequenceNumber: 0,
            slot: null,
          },
        ],
        NOW - age
      );
    };

    writeAt(ASSET_METADATA_REFRESH_MS - 1);
    const fresh = transportFor(() => [bted()]);
    const a = resolverWith(fresh);
    a.request([BTED.subject]);
    await a.pending();
    expect(fresh.calls).toBe(0);

    writeAt(ASSET_METADATA_REFRESH_MS + 1);
    const stale = transportFor(() => [bted()]);
    const b = resolverWith(stale);
    b.request([BTED.subject]);
    await b.pending();
    expect(stale.calls).toBe(1);
  });

  it('does not fetch a subject inside its retry-after window', async () => {
    database.writeResolutions(
      [
        {
          subject: BTED.subject,
          state: 'unregistered',
          retryAfter: NOW + 60_000,
          failureCount: 1,
        },
      ],
      NOW
    );
    const transport = transportFor(() => [bted()]);
    const resolver = resolverWith(transport);
    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(transport.calls).toBe(0);
  });

  it('fetches a subject whose retry-after window has passed', async () => {
    database.writeResolutions(
      [
        {
          subject: BTED.subject,
          state: 'failed',
          retryAfter: NOW - 1,
          failureCount: 1,
        },
      ],
      NOW
    );
    const transport = transportFor(() => [bted()]);
    const resolver = resolverWith(transport);
    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(transport.calls).toBe(1);
  });

  it('asks once when two requests for the same subject overlap', async () => {
    const transport = transportFor(() => [bted()]);
    const resolver = resolverWith(transport);
    resolver.request([BTED.subject]);
    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(transport.calls).toBe(1);
  });
});

describe('verification and the verified column', () => {
  it('writes a verified decimals value', async () => {
    const resolver = resolverWith(transportFor(() => [bted()]));
    await resolver.resolve([BTED.subject]);
    expect(storedRow()).toMatchObject({
      ticker: 'BTED',
      name: 'BitEd Token',
      decimals: 0,
      verified: true,
      source: 'registry',
      slot: null,
    });
  });

  it('keeps an unverified decimals value and marks the row unverified', async () => {
    const resolver = resolverWith(
      transportFor(() => [bted({ decimalsSignature: 'ff'.repeat(64) })])
    );
    await resolver.resolve([BTED.subject]);
    expect(storedRow()).toMatchObject({ decimals: 0, verified: false });
  });

  it('stays verified when a ticker signature fails but decimals does not', async () => {
    const resolver = resolverWith(
      transportFor(() => [bted({ tickerSignature: 'ff'.repeat(64) })])
    );
    await resolver.resolve([BTED.subject]);
    expect(storedRow()).toMatchObject({ ticker: 'BTED', verified: true });
  });

  it('is unverified when there is no decimals property to verify', async () => {
    const resolver = resolverWith(
      transportFor(() => [bted({ omitDecimals: true })])
    );
    await resolver.resolve([BTED.subject]);
    expect(storedRow()).toMatchObject({ decimals: null, verified: false });
  });

  it('is unverified when the entry carries no policy', async () => {
    const resolver = resolverWith(transportFor(() => [bted({ policy: null })]));
    await resolver.resolve([BTED.subject]);
    expect(storedRow()).toMatchObject({ verified: false, ticker: 'BTED' });
  });

  it('drops a decimals value the schema cannot hold and keeps the row', async () => {
    const resolver = resolverWith(
      transportFor(() => [bted({ decimalsValue: 21 })])
    );
    await resolver.resolve([BTED.subject]);
    expect(storedRow()).toMatchObject({
      decimals: null,
      ticker: 'BTED',
      verified: false,
    });
  });

  it('records url and description in the metadata column', async () => {
    const resolver = resolverWith(transportFor(() => [bted()]));
    await resolver.resolve([BTED.subject]);
    expect(JSON.parse(storedRow().metadata)).toEqual({
      url: 'https://bit-ed.org/',
    });
  });
});

describe('writing, emitting and failure', () => {
  it('emits the rows it wrote, once', async () => {
    const onResolved = jest.fn();
    const resolver = resolverWith(
      transportFor(() => [bted()]),
      { onResolved }
    );
    await resolver.resolve([BTED.subject]);
    expect(onResolved).toHaveBeenCalledTimes(1);
    expect(onResolved.mock.calls[0][0]).toHaveLength(1);
    expect(onResolved.mock.calls[0][0][0].subject).toBe(BTED.subject);
  });

  it('does not emit when nothing resolved', async () => {
    const onResolved = jest.fn();
    const resolver = resolverWith(
      transportFor(() => []),
      { onResolved }
    );
    await resolver.resolve([OTHER_SUBJECT]);
    expect(onResolved).not.toHaveBeenCalled();
  });

  it('records an omitted subject as unregistered', async () => {
    const resolver = resolverWith(transportFor(() => []));
    await resolver.resolve([OTHER_SUBJECT]);
    expect(database.readResolutions([OTHER_SUBJECT])[0]).toMatchObject({
      state: 'unregistered',
      attemptedAt: NOW,
    });
  });

  it('records a failed batch and writes no metadata', async () => {
    const transport = failingTransport();
    const resolver = resolverWith(transport);
    await resolver.resolve([BTED.subject]);
    expect(database.readMetadata([BTED.subject])).toEqual([]);
    expect(database.readResolutions([BTED.subject])[0]).toMatchObject({
      state: 'failed',
    });
    expect(transport.calls).toBe(2);
  });

  it('does not propagate a throwing consumer', async () => {
    const resolver = resolverWith(
      transportFor(() => [bted()]),
      {
        onResolved: () => {
          throw new Error('consumer exploded');
        },
      }
    );
    await expect(resolver.resolve([BTED.subject])).resolves.toBeDefined();
    expect(storedRow().ticker).toBe('BTED');
  });
});

describe('refreshing', () => {
  const makeStale = () => {
    const row = storedRow();
    database.writeMetadata(
      [
        {
          subject: row.subject,
          policyId: row.policyId,
          assetName: row.assetName,
          ticker: row.ticker,
          name: row.name,
          decimals: row.decimals,
          verified: row.verified,
          metadata: row.metadata,
          source: row.source,
          sequenceNumber: row.sequenceNumber,
          slot: row.slot,
        },
      ],
      NOW - ASSET_METADATA_REFRESH_MS - 1
    );
  };

  const seed = async () => {
    await resolverWith(transportFor(() => [bted()])).resolve([BTED.subject]);
    makeStale();
  };

  it('rewrites when a sequence number has risen', async () => {
    await seed();
    const onResolved = jest.fn();
    const resolver = resolverWith(
      transportFor(() => [
        {
          ...bted({ sequenceNumber: 1 }),
          ticker: {
            value: 'NEWTICK',
            sequenceNumber: 1,
            signatures: [
              { signature: 'ff'.repeat(64), publicKey: BTED.publicKey },
            ],
          },
        },
      ]),
      { onResolved }
    );
    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(storedRow()).toMatchObject({ ticker: 'NEWTICK', sequenceNumber: 1 });
    expect(onResolved).toHaveBeenCalledTimes(1);
  });

  it('keeps its values and restamps when nothing has risen, without emitting', async () => {
    await seed();
    const onResolved = jest.fn();
    const resolver = resolverWith(
      transportFor(() => [
        {
          ...bted(),
          ticker: {
            value: 'CHANGED-WITHOUT-BUMP',
            sequenceNumber: 0,
            signatures: [
              { signature: 'ff'.repeat(64), publicKey: BTED.publicKey },
            ],
          },
        },
      ]),
      { onResolved }
    );
    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(storedRow()).toMatchObject({ ticker: 'BTED', updatedAt: NOW });
    expect(onResolved).not.toHaveBeenCalled();
  });

  it('does not accept a lower sequence number', async () => {
    const resolver = resolverWith(
      transportFor(() => [bted({ sequenceNumber: 5 })])
    );
    await resolver.resolve([BTED.subject]);
    makeStale();

    const second = resolverWith(
      transportFor(() => [
        {
          ...bted({ sequenceNumber: 1 }),
          ticker: {
            value: 'DOWNGRADE',
            sequenceNumber: 1,
            signatures: [
              { signature: 'ff'.repeat(64), publicKey: BTED.publicKey },
            ],
          },
        },
      ])
    );
    second.request([BTED.subject]);
    await second.pending();
    expect(storedRow()).toMatchObject({ ticker: 'BTED', sequenceNumber: 5 });
  });

  it('rewrites a stored row whose sequence number is null', async () => {
    database.writeMetadata(
      [
        {
          subject: BTED.subject,
          policyId: BTED.subject.slice(0, 56),
          assetName: BTED.subject.slice(56),
          ticker: 'OLD',
          name: null,
          decimals: null,
          verified: false,
          metadata: null,
          source: 'registry',
          sequenceNumber: null,
          slot: null,
        },
      ],
      NOW - ASSET_METADATA_REFRESH_MS - 1
    );
    const resolver = resolverWith(transportFor(() => [bted()]));
    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(storedRow()).toMatchObject({ ticker: 'BTED', sequenceNumber: 0 });
  });

  it('lets a registry response replace a chain row regardless of sequence', async () => {
    database.writeMetadata(
      [
        {
          subject: BTED.subject,
          policyId: BTED.subject.slice(0, 56),
          assetName: BTED.subject.slice(56),
          ticker: null,
          name: 'From the chain',
          decimals: null,
          verified: false,
          metadata: null,
          source: 'chain',
          sequenceNumber: null,
          slot: 99,
        },
      ],
      NOW - ASSET_METADATA_REFRESH_MS - 1
    );
    const resolver = resolverWith(transportFor(() => [bted()]));
    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(storedRow()).toMatchObject({
      source: 'registry',
      ticker: 'BTED',
      slot: null,
    });
  });
});

describe('a forced refresh', () => {
  const seedFresh = async () => {
    await resolverWith(transportFor(() => [bted()])).resolve([BTED.subject]);
  };

  const backOff = () => {
    database.writeResolutions(
      [
        {
          subject: BTED.subject,
          state: 'failed',
          failureCount: 3,
          retryAfter: NOW + 60_000,
        },
      ],
      NOW
    );
  };

  it('fetches a subject inside its retry backoff, where an ordinary read does not', async () => {
    await seedFresh();
    backOff();

    // The complement first, so the case is about the flag rather than about a
    // resolver that ignores the window for everything.
    const ordinary = transportFor(() => [bted()]);
    const first = resolverWith(ordinary);
    first.request([BTED.subject]);
    await first.pending();
    expect(ordinary.calls).toBe(0);

    const forced = transportFor(() => [bted()]);
    const second = resolverWith(forced);
    second.request([BTED.subject], { force: true });
    await second.pending();
    expect(forced.calls).toBe(1);
  });

  it('fetches a row that is well inside its refresh window', async () => {
    await seedFresh();

    const transport = transportFor(() => [bted()]);
    const resolver = resolverWith(transport);
    resolver.request([BTED.subject], { force: true });
    await resolver.pending();
    expect(transport.calls).toBe(1);
  });

  it('keeps the row values and restamps it when nothing has risen', async () => {
    await seedFresh();
    database.writeMetadata(
      [
        {
          subject: BTED.subject,
          policyId: BTED.subject.slice(0, 56),
          assetName: BTED.subject.slice(56),
          ticker: 'BTED',
          name: 'BitEd Token',
          decimals: 0,
          verified: true,
          metadata: storedRow().metadata,
          source: 'registry',
          sequenceNumber: 0,
          slot: null,
        },
      ],
      NOW - 5_000
    );
    expect(storedRow().updatedAt).toBe(NOW - 5_000);

    const resolver = resolverWith(transportFor(() => [bted()]));
    resolver.request([BTED.subject], { force: true });
    await resolver.pending();

    // The values and the stamp are asserted separately, so a refresh that
    // rewrote the row with identical content is still distinguishable from one
    // that only restamped it.
    expect(storedRow()).toMatchObject({
      ticker: 'BTED',
      decimals: 0,
      verified: true,
      sequenceNumber: 0,
    });
    expect(storedRow().updatedAt).toBe(NOW);
  });

  it('rewrites the row and runs verification again when the sequence number rises', async () => {
    await seedFresh();
    expect(storedRow()).toMatchObject({ verified: true, decimals: 0 });

    // The stored verdict is true. The new content carries a higher sequence
    // number, which the old signature does not cover, so the verdict has to move
    // to false. A refresh that carried the stored verdict across would leave it
    // true and pass every other case here.
    const resolver = resolverWith(
      transportFor(() => [bted({ sequenceNumber: 1 })])
    );
    resolver.request([BTED.subject], { force: true });
    await resolver.pending();

    expect(storedRow()).toMatchObject({ sequenceNumber: 1, verified: false });
  });

  it('leaves the cached row in place when the transport is unavailable', async () => {
    await seedFresh();
    const broken = failingTransport();
    const resolver = resolverWith(broken);

    expect(resolver.request([BTED.subject], { force: true })).toHaveLength(1);
    await resolver.pending();

    expect(broken.calls).toBeGreaterThan(0);
    expect(storedRow()).toMatchObject({ ticker: 'BTED', verified: true });
  });
});

describe('timers', () => {
  afterEach(() => {
    jest.useRealTimers();
  });

  it('schedules nothing after a resolve has finished', async () => {
    const transport = transportFor(() => [bted()]);
    const resolver = resolverWith(transport);
    await resolver.resolve([BTED.subject]);
    expect(transport.calls).toBe(1);

    jest.useFakeTimers();
    jest.advanceTimersByTime(24 * 60 * 60 * 1000);
    await Promise.resolve();
    expect(transport.calls).toBe(1);
  });
});

/**
 * Offline is a state rather than a failure, and the two places that say so are
 * a transport that throws rather than answering, and the background work the
 * request queues behind its answer. Neither is reached by a transport that
 * returns a failure result, which is what every other case here uses.
 */
describe('a transport that throws', () => {
  const throwingTransport = (): RegistryTransport => ({
    post: () => {
      throw new Error('there is no network');
    },
  });

  it('answers from the cache and records nothing', async () => {
    const resolver = resolverWith(throwingTransport());
    const rows = await resolver.resolve([BTED.subject]);
    expect(rows).toEqual([]);
    expect(storedRow()).toBeUndefined();
  });

  it('does not reject the request that queued the work', async () => {
    const resolver = resolverWith(throwingTransport());
    expect(resolver.request([BTED.subject])).toEqual([]);
    await expect(resolver.pending()).resolves.toBeUndefined();
  });

  it('releases the subject, so a later request tries again', async () => {
    let attempts = 0;
    const transport: RegistryTransport = {
      post: async (_url: string, body: string) => {
        attempts += 1;
        if (attempts === 1) throw new Error('there is no network');
        const { subjects } = JSON.parse(body);
        return {
          ok: true,
          status: 200,
          body: JSON.stringify({ subjects: subjects.map(() => bted()) }),
        };
      },
    };
    const resolver = resolverWith(transport);
    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(storedRow()).toBeUndefined();

    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(storedRow().ticker).toBe('BTED');
    expect(attempts).toBe(2);
  });
});

describe('a database that throws under the resolver', () => {
  it('does not reject the background work, and releases the subject', async () => {
    // `request` reads the cache itself and then queues the rest behind its
    // answer, so this fails the second read, inside the queued work. Every
    // accessor on the real wrapper swallows its own failure, so an injected
    // one is the only way to reach the queue's own guard: work scheduled and
    // not awaited must not become an unhandled rejection.
    let reads = 0;
    const failsOnTheSecondRead = {
      readMetadata: (subjects: Array<string>) => {
        reads += 1;
        // Read one is `request`'s own; read two is the queued work's.
        if (reads === 2) throw new Error('the handle went away');
        return database.readMetadata(subjects);
      },
      readResolutions: () => [],
      writeMetadata: () => 0,
      writeResolutions: () => 0,
      close: () => {},
    };
    const resolver = resolverWith(failingTransport(), {
      database: failsOnTheSecondRead,
    });

    expect(resolver.request([BTED.subject])).toEqual([]);
    await expect(resolver.pending()).resolves.toBeUndefined();
    expect(reads).toBe(2);

    // Released rather than left claimed, so the subject can be asked for again.
    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(reads).toBe(4);
  });
});

// ---------------------------------------------------------------------------
// The chain channel.
//
// The pointer source is stubbed and the immutable database is written to a
// temporary directory from the recorded preprod block, so the whole path runs:
// two requests, four checks, one row.
// ---------------------------------------------------------------------------

const CHUNK_SIZE = 21600;
const CHAIN_BLOCK = Buffer.from(PREPROD_BLOCK_HEX, 'hex');
const CHAIN_SUBJECT = `${PREPROD_BLOCK.policyId}${PREPROD_BLOCK.assetName}`;

const chainTransaction = () => {
  const outer = arraySpans(CHAIN_BLOCK, 0);
  const inner = arraySpans(CHAIN_BLOCK, outer[1].start);
  const bodies = arraySpans(CHAIN_BLOCK, inner[1].start);
  const witnesses = arraySpans(CHAIN_BLOCK, inner[2].start);
  const auxiliary = mapSpans(CHAIN_BLOCK, inner[3].start).find(
    (entry) =>
      Number(readHead(CHAIN_BLOCK, entry.key.start).argument) ===
      PREPROD_BLOCK.transactionIndex
  );
  const index = PREPROD_BLOCK.transactionIndex;
  return Buffer.concat([
    Buffer.from([0x84]),
    CHAIN_BLOCK.subarray(bodies[index].start, bodies[index].end),
    CHAIN_BLOCK.subarray(witnesses[index].start, witnesses[index].end),
    Buffer.from([0xf5]),
    CHAIN_BLOCK.subarray(auxiliary.value.start, auxiliary.value.end),
  ]).toString('hex');
};

const writeImmutable = (root: string, slot = PREPROD_BLOCK.slot): string => {
  const immutable = path.join(root, 'immutable');
  fs.mkdirSync(immutable, { recursive: true });
  const zero = Buffer.alloc(1 + (CHUNK_SIZE + 2) * 4);
  zero[0] = IMMUTABLE_PRIMARY_INDEX_VERSION;
  fs.writeFileSync(path.join(immutable, '00000.primary'), zero);

  const chunk = Math.floor(slot / CHUNK_SIZE);
  const relative = (slot % CHUNK_SIZE) + 1;
  const primary = Buffer.alloc(1 + (CHUNK_SIZE + 2) * 4);
  primary[0] = IMMUTABLE_PRIMARY_INDEX_VERSION;
  for (let index = relative + 1; index <= CHUNK_SIZE + 1; index += 1) {
    primary.writeUInt32BE(IMMUTABLE_SECONDARY_ENTRY_BYTES, 1 + index * 4);
  }
  const secondary = Buffer.alloc(IMMUTABLE_SECONDARY_ENTRY_BYTES);
  Buffer.from(PREPROD_BLOCK.hash, 'hex').copy(secondary, 16);
  secondary.writeBigUInt64BE(BigInt(slot), 48);

  const name = String(chunk).padStart(5, '0');
  fs.writeFileSync(path.join(immutable, `${name}.primary`), primary);
  fs.writeFileSync(path.join(immutable, `${name}.secondary`), secondary);
  fs.writeFileSync(path.join(immutable, `${name}.chunk`), CHAIN_BLOCK);
  return immutable;
};

const pointerTransport = (
  options: { slot?: number; txHash?: string } = {}
): HttpTransport & { calls: number } => {
  const stub = {
    calls: 0,
    async post(url: string): Promise<HttpTransportResult> {
      stub.calls += 1;
      if (url.includes('asset_info')) {
        return {
          ok: true,
          status: 200,
          body: JSON.stringify([
            {
              policy_id: PREPROD_BLOCK.policyId,
              asset_name: PREPROD_BLOCK.assetName,
              fingerprint: 'asset1chain',
              minting_tx_hash: options.txHash ?? PREPROD_BLOCK.transactionHash,
              mint_cnt: 1,
              cip68_metadata: null,
            },
          ]),
        };
      }
      return {
        ok: true,
        status: 200,
        body: JSON.stringify([
          {
            tx_hash: options.txHash ?? PREPROD_BLOCK.transactionHash,
            block_hash: PREPROD_BLOCK.hash,
            absolute_slot: options.slot ?? PREPROD_BLOCK.slot,
            block_height: 5078119,
            cbor: chainTransaction(),
          },
        ]),
      };
    },
  };
  return stub;
};

const chainResolver = (
  registry: RegistryTransport,
  extra: Record<string, unknown> = {}
) =>
  resolverWith(registry, {
    immutableDirectory: writeImmutable(directory),
    pointerSourceUrl: 'https://preprod.koios.rest/api/v1',
    pointerBudget: new KoiosRequestBudget(),
    ...extra,
  });

describe('the chain channel', () => {
  it('writes a chain row for a subject the registry does not answer', async () => {
    const pointer = pointerTransport();
    const resolver = chainResolver(
      transportFor(() => []),
      {
        pointerTransport: pointer,
      }
    );
    await resolver.resolve([CHAIN_SUBJECT]);

    const row = database.readMetadata([CHAIN_SUBJECT])[0];
    expect(row.source).toBe('chain');
    expect(row.slot).toBe(PREPROD_BLOCK.slot);
    expect(row.sequenceNumber).toBeNull();
    expect(row.decimals).toBeNull();
    expect(row.verified).toBe(false);
    expect(row.ticker).toBeNull();
    expect(row.name).toBe('Northwind Demo');
    // Two requests for the whole batch, which is what the pointer client
    // promises and what the resolver must not turn into two per asset.
    expect(pointer.calls).toBe(2);
  });

  it('does not consult the index for a subject the registry answered', async () => {
    const pointer = pointerTransport();
    const resolver = chainResolver(
      transportFor(() => [bted()]),
      {
        pointerTransport: pointer,
      }
    );
    await resolver.resolve([BTED.subject]);

    expect(pointer.calls).toBe(0);
    expect(storedRow().source).toBe('registry');
  });

  it('does not overwrite a registry row with a chain row', async () => {
    const pointer = pointerTransport();
    // A registry answer for the same subject the pointer source knows.
    const registry = transportFor(() => [
      {
        subject: CHAIN_SUBJECT,
        policy: null,
        name: { value: 'Registry Name', sequenceNumber: 0, signatures: [] },
      },
    ]);
    const resolver = chainResolver(registry, { pointerTransport: pointer });
    await resolver.resolve([CHAIN_SUBJECT]);

    const row = database.readMetadata([CHAIN_SUBJECT])[0];
    expect(row.source).toBe('registry');
    expect(row.name).toBe('Registry Name');
    expect(pointer.calls).toBe(0);
  });

  it('writes no row and records a pending retry inside the volatile window', async () => {
    const pointer = pointerTransport({ slot: PREPROD_BLOCK.slot + 1 });
    const resolver = chainResolver(
      transportFor(() => []),
      {
        pointerTransport: pointer,
      }
    );
    await resolver.resolve([CHAIN_SUBJECT]);

    expect(database.readMetadata([CHAIN_SUBJECT])).toEqual([]);
    const resolution = database.readResolutions([CHAIN_SUBJECT])[0];
    expect(resolution.state).toBe('pending');
    expect(resolution.retryAfter).toBe(NOW + 60 * 60 * 1000);
    // Twelve hours is the order of the window the immutable database cannot
    // see into, and the retry has to land inside it.
    expect(resolution.retryAfter - NOW).toBeLessThan(12 * 60 * 60 * 1000);
  });

  it('writes no row when the pointer names a transaction the chain does not hold', async () => {
    const pointer = pointerTransport({ txHash: 'a'.repeat(64) });
    const resolver = chainResolver(
      transportFor(() => []),
      {
        pointerTransport: pointer,
      }
    );
    await resolver.resolve([CHAIN_SUBJECT]);

    expect(database.readMetadata([CHAIN_SUBJECT])).toEqual([]);
    expect(database.readResolutions([CHAIN_SUBJECT])[0].state).toBe('failed');
  });

  it('issues nothing when no pointer source is selected', async () => {
    const pointer = pointerTransport();
    const resolver = chainResolver(
      transportFor(() => []),
      {
        pointerTransport: pointer,
        pointerSourceUrl: null,
      }
    );
    await resolver.resolve([CHAIN_SUBJECT]);

    expect(pointer.calls).toBe(0);
    expect(database.readMetadata([CHAIN_SUBJECT])).toEqual([]);
  });

  it('issues nothing when there is no immutable database to confirm against', async () => {
    const pointer = pointerTransport();
    const resolver = resolverWith(
      transportFor(() => []),
      {
        immutableDirectory: null,
        pointerSourceUrl: 'https://preprod.koios.rest/api/v1',
        pointerTransport: pointer,
        pointerBudget: new KoiosRequestBudget(),
      }
    );
    await resolver.resolve([CHAIN_SUBJECT]);

    expect(pointer.calls).toBe(0);
  });

  it('takes the pointer source the renderer last named', async () => {
    const pointer = pointerTransport();
    const resolver = chainResolver(
      transportFor(() => []),
      {
        pointerTransport: pointer,
        pointerSourceUrl: null,
      }
    );
    resolver.setPointerSourceUrl('https://preprod.koios.rest/api/v1');
    await resolver.resolve([CHAIN_SUBJECT]);
    expect(pointer.calls).toBe(2);
  });
});

describe('chainPointerToRow', () => {
  const pointer = (cip68: Record<string, unknown> | null = null) => ({
    subject: CHAIN_SUBJECT,
    policyId: PREPROD_BLOCK.policyId,
    assetName: PREPROD_BLOCK.assetName,
    fingerprint: 'asset1chain',
    mintingTxHash: PREPROD_BLOCK.transactionHash,
    mintCount: 1,
    cip68Metadata: cip68,
  });

  const closureOf = (row: { metadata: string | null }) =>
    JSON.parse(row.metadata).closed;

  it('freezes a CIP-25 row under a policy that can no longer mint', () => {
    const row = chainPointerToRow(
      pointer(),
      PREPROD_BLOCK.slot,
      { name: 'Northwind Demo' },
      true
    );
    expect(closureOf(row)).toBe(true);
  });

  it('leaves a CIP-25 row under an open policy unfrozen', () => {
    const row = chainPointerToRow(
      pointer(),
      PREPROD_BLOCK.slot,
      { name: 'Northwind Demo' },
      false
    );
    expect(closureOf(row)).toBe(false);
  });

  // A CIP-68 datum lives at a spendable output and changes when that output is
  // spent, which needs no minting, so closure says nothing about it.
  it('never freezes a CIP-68 row, even under a closed policy', () => {
    const row = chainPointerToRow(
      pointer({ name: 'Live Record' }),
      PREPROD_BLOCK.slot,
      { name: 'Mint Record' },
      true
    );
    expect(closureOf(row)).toBe(false);
    expect(row.name).toBe('Live Record');
  });

  it('reads a CIP-25 name written as an array of parts', () => {
    const row = chainPointerToRow(
      pointer(),
      PREPROD_BLOCK.slot,
      { name: ['Northwind ', 'Demo'] },
      false
    );
    expect(row.name).toBe('Northwind Demo');
  });

  it('carries no decimals and claims no verification', () => {
    const row = chainPointerToRow(pointer(), PREPROD_BLOCK.slot, null, false);
    expect(row.decimals).toBeNull();
    expect(row.verified).toBe(false);
    expect(row.sequenceNumber).toBeNull();
    expect(row.ticker).toBeNull();
    expect(row.source).toBe('chain');
  });
});

describe('freshness per channel', () => {
  const WEEK = ASSET_METADATA_REFRESH_MS;

  const writeChainRow = (options: { closed: boolean; record?: unknown }) => {
    database.writeMetadata(
      [
        {
          subject: CHAIN_SUBJECT,
          policyId: PREPROD_BLOCK.policyId,
          assetName: PREPROD_BLOCK.assetName,
          ticker: null,
          name: 'Northwind Demo',
          decimals: null,
          verified: false,
          metadata: JSON.stringify({
            record: options.record ?? { name: 'Northwind Demo' },
            closed: options.closed,
          }),
          source: 'chain',
          sequenceNumber: null,
          slot: PREPROD_BLOCK.slot,
        },
      ],
      // Written a fortnight ago, so the seven-day window has elapsed and the
      // only thing that can hold a re-read back is the freezing rule.
      NOW - 2 * WEEK
    );
  };

  const asked = async (extra: Record<string, unknown> = {}) => {
    const pointer = pointerTransport();
    const resolver = chainResolver(
      transportFor(() => []),
      {
        pointerTransport: pointer,
        ...extra,
      }
    );
    resolver.request([CHAIN_SUBJECT]);
    await resolver.pending();
    return pointer.calls;
  };

  it('never re-reads a CIP-25 row under a policy that can no longer mint', async () => {
    writeChainRow({ closed: true });
    expect(await asked()).toBe(0);
  });

  it('re-reads a CIP-25 row under an open policy once the window has elapsed', async () => {
    writeChainRow({ closed: false });
    expect(await asked()).toBe(2);
  });

  it('re-reads a frozen row when the read is forced', async () => {
    writeChainRow({ closed: true });
    const pointer = pointerTransport();
    const resolver = chainResolver(
      transportFor(() => []),
      {
        pointerTransport: pointer,
      }
    );
    // A manual refresh from the settings dialog does not consult the window or
    // the freezing rule at all.
    resolver.request([CHAIN_SUBJECT], { force: true });
    await resolver.pending();
    expect(pointer.calls).toBe(2);
  });

  it('does not freeze a row inside the window either way', async () => {
    writeChainRow({ closed: false });
    database.writeMetadata(
      [
        {
          subject: CHAIN_SUBJECT,
          policyId: PREPROD_BLOCK.policyId,
          assetName: PREPROD_BLOCK.assetName,
          ticker: null,
          name: 'Northwind Demo',
          decimals: null,
          verified: false,
          metadata: JSON.stringify({ record: {}, closed: false }),
          source: 'chain',
          sequenceNumber: null,
          slot: PREPROD_BLOCK.slot,
        },
      ],
      NOW
    );
    expect(await asked()).toBe(0);
  });

  it('takes the window for a registry row whatever its policy does', async () => {
    const registry = transportFor(() => [bted()]);
    const resolver = resolverWith(registry);
    await resolver.resolve([BTED.subject]);
    expect(registry.calls).toBe(1);

    // The same row, a fortnight old. A registry record can be updated by its
    // issuer long after the minting window shuts, so closure never freezes one.
    const stored = storedRow();
    database.writeMetadata(
      [
        {
          subject: stored.subject,
          policyId: stored.policyId,
          assetName: stored.assetName,
          ticker: stored.ticker,
          name: stored.name,
          decimals: stored.decimals,
          verified: stored.verified,
          metadata: stored.metadata,
          source: stored.source,
          sequenceNumber: stored.sequenceNumber,
          slot: stored.slot,
        },
      ],
      NOW - 2 * WEEK
    );
    resolver.request([BTED.subject]);
    await resolver.pending();
    expect(registry.calls).toBe(2);
  });
});

describe('openAssetMetadataResolver', () => {
  it('builds a resolver on the options it is given and closes its database', async () => {
    const resolver = openAssetMetadataResolver({
      database,
      transport: transportFor(() => [bted()]),
      endpoint: 'https://tokens.example',
      now: () => NOW,
    });
    await resolver.resolve([BTED.subject]);
    expect(storedRow().ticker).toBe('BTED');

    resolver.close();
    // Closed through the resolver rather than through the handle the suite
    // holds, so the teardown's own close is the second one.
    expect(database.readMetadata([BTED.subject])).toEqual([]);
  });
});
