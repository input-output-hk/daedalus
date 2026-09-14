/**
 * The main-side handlers against a real database file and a stubbed transport:
 * what a request answers with, what it never waits for, and what reaches the
 * renderer without being asked for twice.
 *
 * @jest-environment node
 */
import fs from 'fs';
import os from 'os';
import path from 'path';
import { openAssetMetadataDatabase } from '../assets/assetMetadataDb';
import type { AssetMetadataDatabase } from '../assets/assetMetadataDb';
import type {
  RegistryTransport,
  RegistryTransportResult,
} from '../assets/assetRegistryClient';

const mockChannels: Array<{ onRequest: jest.Mock; send: jest.Mock }> = [];

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => {
    const channel = {
      onRequest: jest.fn(),
      send: jest.fn().mockResolvedValue(undefined),
    };
    mockChannels.push(channel);
    return channel;
  }),
}));

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

const POLICY = 'c'.repeat(56);
const SUBJECT = `${POLICY}424f4f4b`;
const OTHER = `${'a'.repeat(56)}beef`;

const PNG = Buffer.concat([
  Buffer.from('89504e470d0a1a0a', 'hex'),
  Buffer.from('00ff7f8001', 'hex'),
]);

type Handlers = {
  readMetadata: (request: any) => Promise<any>;
  readImage: (request: any) => Promise<any>;
  push: (rows: Array<any>) => void;
};

const loadModule = () => {
  let module: any;
  jest.isolateModules(() => {
    // eslint-disable-next-line global-require
    module = require('./assetMetadataChannel');
  });
  return module;
};

const stubTransport = (
  post: (url: string, body: string) => Promise<RegistryTransportResult>
): RegistryTransport => ({
  post: (url: string, body: string) => post(url, body),
});

const registryAnswer = (subject: string): RegistryTransportResult => ({
  ok: true,
  status: 200,
  body: JSON.stringify({
    subjects: [
      {
        subject,
        policy: null,
        ticker: { value: 'BOOK', sequenceNumber: 0, signatures: [] },
        name: { value: 'Bookmark', sequenceNumber: 0, signatures: [] },
      },
    ],
  }),
});

describe('assetMetadataChannel', () => {
  let directory: string;
  let database: AssetMetadataDatabase;

  beforeEach(() => {
    jest.clearAllMocks();
    mockChannels.length = 0;
    directory = fs.mkdtempSync(path.join(os.tmpdir(), 'asset-ipc-'));
    database = openAssetMetadataDatabase(path.join(directory, 'assets.sqlite'));
  });

  afterEach(() => {
    database.close();
    fs.rmSync(directory, {
      recursive: true,
      force: true,
    });
  });

  const handlersWith = (transport: RegistryTransport): Handlers => {
    const { AssetMetadataChannelHandlers } = loadModule();
    return new AssetMetadataChannelHandlers({
      database,
      transport,
      endpoint: 'https://tokens.example',
    });
  };

  const writeRow = (subject: string, overrides: Record<string, any> = {}) => {
    database.writeMetadata(
      [
        {
          subject,
          policyId: subject.slice(0, 56),
          assetName: subject.slice(56),
          ticker: 'BOOK',
          name: 'Bookmark',
          decimals: 6,
          verified: true,
          metadata: JSON.stringify({ url: 'https://example.org' }),
          source: 'registry',
          sequenceNumber: 0,
          slot: null,
          ...overrides,
        },
      ],
      1_700_000_000_000
    );
  };

  describe('readMetadata', () => {
    it('answers a cold cache with no entries and every subject pending', async () => {
      const handlers = handlersWith(
        stubTransport(async () => ({ ok: false, reason: 'network' }))
      );
      const response = await handlers.readMetadata({
        requestId: 'r-1',
        subjects: [SUBJECT, OTHER],
      });
      expect(response.entries).toEqual([]);
      expect(response.unresolved).toEqual([
        { subject: SUBJECT, state: 'pending' },
        { subject: OTHER, state: 'pending' },
      ]);
    });

    it('echoes the request id it was given', async () => {
      const handlers = handlersWith(
        stubTransport(async () => ({ ok: false, reason: 'network' }))
      );
      const first = await handlers.readMetadata({
        requestId: 'r-first',
        subjects: [SUBJECT],
      });
      const second = await handlers.readMetadata({
        requestId: 'r-second',
        subjects: [SUBJECT],
      });
      expect(first.requestId).toBe('r-first');
      expect(second.requestId).toBe('r-second');
    });

    it('answers from the cache without waiting for the transport', async () => {
      writeRow(SUBJECT);
      let calls = 0;
      const handlers = handlersWith(
        stubTransport(() => {
          calls += 1;
          // Never settles. A handler that awaited the network would never
          // answer, so this case fails by hanging rather than by asserting.
          return new Promise<RegistryTransportResult>(() => {});
        })
      );
      const response = await handlers.readMetadata({
        requestId: 'r-2',
        subjects: [SUBJECT, OTHER],
      });
      expect(response.entries).toHaveLength(1);
      expect(response.entries[0].subject).toBe(SUBJECT);
      expect(response.unresolved).toEqual([
        { subject: OTHER, state: 'pending' },
      ]);
      expect(calls).toBe(1);
    });

    it('distinguishes never asked from unregistered from failed', async () => {
      const failed = `${'b'.repeat(56)}01`;
      const unregistered = `${'d'.repeat(56)}02`;
      database.writeResolutions(
        [
          {
            subject: failed,
            state: 'failed',
            retryAfter: 4_000_000_000_000,
            failureCount: 2,
          },
          {
            subject: unregistered,
            state: 'unregistered',
            retryAfter: 4_000_000_000_000,
            failureCount: 0,
          },
        ],
        1_700_000_000_000
      );
      const handlers = handlersWith(
        stubTransport(async () => ({ ok: false, reason: 'network' }))
      );
      const response = await handlers.readMetadata({
        requestId: 'r-3',
        subjects: [failed, unregistered, OTHER],
      });
      expect(response.unresolved).toEqual([
        { subject: failed, state: 'failed' },
        { subject: unregistered, state: 'unregistered' },
        { subject: OTHER, state: 'pending' },
      ]);
    });

    it('never names a subject it has a row for under unresolved', async () => {
      writeRow(SUBJECT);
      database.writeResolutions(
        [
          {
            subject: SUBJECT,
            state: 'resolved',
            retryAfter: 4_000_000_000_000,
            failureCount: 0,
          },
        ],
        1_700_000_000_000
      );
      const handlers = handlersWith(
        stubTransport(async () => ({ ok: false, reason: 'network' }))
      );
      const response = await handlers.readMetadata({
        requestId: 'r-4',
        subjects: [SUBJECT],
      });
      expect(response.unresolved).toEqual([]);
      expect(response.entries).toHaveLength(1);
    });

    it('carries the stored verdict, the source and the parsed metadata', async () => {
      writeRow(SUBJECT);
      const handlers = handlersWith(
        stubTransport(async () => ({ ok: false, reason: 'network' }))
      );
      const response = await handlers.readMetadata({
        requestId: 'r-5',
        subjects: [SUBJECT],
      });
      expect(response.entries[0]).toEqual({
        subject: SUBJECT,
        policyId: POLICY,
        assetName: '424f4f4b',
        ticker: 'BOOK',
        name: 'Bookmark',
        decimals: 6,
        verified: true,
        source: 'registry',
        hasImage: false,
        metadata: { url: 'https://example.org' },
      });
    });

    it('maps a metadata column that is not an object to null', async () => {
      writeRow(SUBJECT, { metadata: 'not json at all' });
      const handlers = handlersWith(
        stubTransport(async () => ({ ok: false, reason: 'network' }))
      );
      const response = await handlers.readMetadata({
        requestId: 'r-6',
        subjects: [SUBJECT],
      });
      expect(response.entries[0].metadata).toBeNull();
    });

    it('reports hasImage per subject', async () => {
      writeRow(SUBJECT);
      writeRow(OTHER);
      database.writeImage(
        {
          subject: SUBJECT,
          mediaType: 'image/png',
          bytes: new Uint8Array(PNG),
        },
        1_700_000_000_000
      );
      const handlers = handlersWith(
        stubTransport(async () => ({ ok: false, reason: 'network' }))
      );
      const response = await handlers.readMetadata({
        requestId: 'r-7',
        subjects: [SUBJECT, OTHER],
      });
      const bySubject = new Map(
        response.entries.map((entry: any) => [entry.subject, entry.hasImage])
      );
      expect(bySubject.get(SUBJECT)).toBe(true);
      expect(bySubject.get(OTHER)).toBe(false);
    });

    it('asks once for a subject named several times', async () => {
      writeRow(SUBJECT);
      const handlers = handlersWith(
        stubTransport(async () => ({ ok: false, reason: 'network' }))
      );
      const response = await handlers.readMetadata({
        requestId: 'r-8',
        subjects: [SUBJECT, SUBJECT, SUBJECT],
      });
      expect(response.entries).toHaveLength(1);
      expect(response.unresolved).toEqual([]);
    });

    it('answers rather than rejecting when the database is gone', async () => {
      const handlers = handlersWith(
        stubTransport(async () => ({ ok: false, reason: 'network' }))
      );
      database.close();
      const response = await handlers.readMetadata({
        requestId: 'r-9',
        subjects: [SUBJECT],
      });
      expect(response.requestId).toBe('r-9');
      expect(response.entries).toEqual([]);
    });
  });

  describe('a refresh request', () => {
    it('asks the resolver to ignore the window, and an ordinary read does not', async () => {
      const handlers = handlersWith(
        stubTransport(async () => ({ ok: false, reason: 'network' }))
      );
      const resolver = (handlers as any)._resolver;
      const asked: Array<Record<string, unknown>> = [];
      resolver.request = (subjects: Array<string>, options = {}) => {
        asked.push({ subjects, options });
        return [];
      };

      await handlers.readMetadata({ requestId: 'r-1', subjects: [SUBJECT] });
      await handlers.readMetadata({
        requestId: 'r-2',
        subjects: [SUBJECT],
        refresh: true,
      });

      expect(asked).toEqual([
        { subjects: [SUBJECT], options: { force: false } },
        { subjects: [SUBJECT], options: { force: true } },
      ]);
    });

    it('answers a refresh from the cache, without waiting for the fetch', async () => {
      writeRow(SUBJECT);
      const handlers = handlersWith(
        stubTransport(() => new Promise(() => {}) as any)
      );

      const response = await handlers.readMetadata({
        requestId: 'r-3',
        subjects: [SUBJECT],
        refresh: true,
      });

      expect(response.requestId).toBe('r-3');
      expect(response.entries.map((entry) => entry.subject)).toEqual([SUBJECT]);
    });
  });

  describe('push', () => {
    it('sends the rows a resolve produced without a second request', async () => {
      const handlers = handlersWith(
        stubTransport(async () => registryAnswer(SUBJECT))
      );
      (handlers as any)._window = {
        isDestroyed: () => false,
        webContents: {},
      };
      await (handlers as any)._resolver.resolve([SUBJECT]);
      const updateChannel = mockChannels[1];
      expect(updateChannel.send).toHaveBeenCalledTimes(1);
      const [message] = updateChannel.send.mock.calls[0];
      expect(message.entries).toHaveLength(1);
      expect(message.entries[0].ticker).toBe('BOOK');
      expect(message.entries[0].hasImage).toBe(false);
    });

    it('sends nothing when the window has gone', async () => {
      const handlers = handlersWith(
        stubTransport(async () => registryAnswer(SUBJECT))
      );
      (handlers as any)._window = {
        isDestroyed: () => true,
        webContents: {},
      };
      await (handlers as any)._resolver.resolve([SUBJECT]);
      expect(mockChannels[1].send).not.toHaveBeenCalled();
    });
  });

  describe('readImage', () => {
    it('answers present with the stored bytes and media type', async () => {
      writeRow(SUBJECT);
      database.writeImage(
        {
          subject: SUBJECT,
          mediaType: 'image/png',
          bytes: new Uint8Array(PNG),
        },
        1_700_000_000_000
      );
      const handlers = handlersWith(
        stubTransport(async () => {
          throw new Error('no request expected');
        })
      );
      const response = await handlers.readImage({
        requestId: 'i-1',
        subject: SUBJECT,
      });
      expect(response.requestId).toBe('i-1');
      expect(response.status).toBe('present');
      expect(response.mediaType).toBe('image/png');
      expect(Buffer.from(response.bytes).equals(PNG)).toBe(true);
    });

    it('answers absent for a subject the registry has no logo for', async () => {
      writeRow(SUBJECT);
      const handlers = handlersWith(
        stubTransport(async () => ({
          ok: true,
          status: 200,
          body: JSON.stringify({ subjects: [{ subject: SUBJECT }] }),
        }))
      );
      const response = await handlers.readImage({
        requestId: 'i-2',
        subject: SUBJECT,
      });
      expect(response).toEqual({ requestId: 'i-2', status: 'absent' });
    });

    it('answers absent rather than rejecting when the transport fails', async () => {
      writeRow(SUBJECT);
      const handlers = handlersWith(
        stubTransport(async () => ({ ok: false, reason: 'network' }))
      );
      const response = await handlers.readImage({
        requestId: 'i-3',
        subject: SUBJECT,
      });
      expect(response).toEqual({ requestId: 'i-3', status: 'absent' });
    });
  });

  describe('handleAssetMetadataRequests', () => {
    it('registers one handler per request channel however often it is called', () => {
      const module = loadModule();
      const window = { isDestroyed: () => false, webContents: {} };
      const first = module.handleAssetMetadataRequests(window, { database });
      const second = module.handleAssetMetadataRequests(window, { database });
      expect(first).not.toBeNull();
      expect(second).toBeNull();
      expect(mockChannels[0].onRequest).toHaveBeenCalledTimes(1);
      expect(mockChannels[2].onRequest).toHaveBeenCalledTimes(1);
    });
  });
});
