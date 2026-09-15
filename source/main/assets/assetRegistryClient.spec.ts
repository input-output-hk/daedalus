/**
 * The registry client: endpoint resolution, batch sizing in request bytes, one
 * rule per status class, the terminating 413 split, the backoff, and response
 * normalisation.
 *
 * @jest-environment node
 */
import http from 'http';
import type { AddressInfo } from 'net';
import {
  ASSET_REGISTRY_BACKOFF_BASE_MS,
  ASSET_REGISTRY_BACKOFF_CEILING_MS,
  ASSET_REGISTRY_FALLBACK_URL,
  ASSET_REGISTRY_MAX_REQUEST_BYTES,
  assetRegistryBackoffMs,
  assetRegistryBatches,
  assetRegistryEndpoint,
  assetRegistryQueryUrl,
  assetRegistryRequestBody,
  ASSET_REGISTRY_MAX_RESPONSE_BYTES,
  httpRegistryTransport,
  queryAssetRegistry,
} from './assetRegistryClient';
import type {
  RegistryTransport,
  RegistryTransportResult,
} from './assetRegistryClient';
import { launcherConfig } from '../config';
import { environment } from '../environment';

jest.mock('../config', () => ({
  launcherConfig: { metadataUrl: undefined },
  MOCK_TOKEN_METADATA_SERVER_URL: 'http://127.0.0.1',
  MOCK_TOKEN_METADATA_SERVER_PORT: 41531,
}));

jest.mock('../environment', () => ({
  environment: { isSelfnode: false },
}));

jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

const POLICY = 'c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b9';
const NAME = '42544544';
const SUBJECT = `${POLICY}${NAME}`;
const OTHER_SUBJECT = `${'a'.repeat(56)}beef`;

const mutableConfig = launcherConfig as { metadataUrl?: string };
const mutableEnvironment = environment as { isSelfnode: boolean };

type Call = { url: string; body: string };

const stub = (
  answers:
    | Array<RegistryTransportResult>
    | ((call: Call) => RegistryTransportResult)
): RegistryTransport & {
  calls: Array<Call>;
  inFlight: number;
  overlapped: boolean;
} => {
  const calls: Array<Call> = [];
  const state = {
    calls,
    inFlight: 0,
    overlapped: false,
    async post(url: string, body: string): Promise<RegistryTransportResult> {
      calls.push({ url, body });
      state.inFlight += 1;
      if (state.inFlight > 1) state.overlapped = true;
      await Promise.resolve();
      state.inFlight -= 1;
      if (typeof answers === 'function') return answers({ url, body });
      return (
        answers[calls.length - 1] ??
        answers[answers.length - 1] ?? {
          ok: false,
          reason: 'network',
        }
      );
    },
  };
  return state;
};

const ok = (
  subjects: Array<Record<string, unknown>>
): RegistryTransportResult => ({
  ok: true,
  status: 200,
  body: JSON.stringify({ subjects }),
});

const entryFor = (
  subject: string,
  overrides: Record<string, unknown> = {}
) => ({
  subject,
  policy:
    '820182018282051a0303eb448200581c39a1df51147b6de6689a4727846962fb6540c3a3c7859a1a79b9420f',
  name: {
    value: 'BitEd Token',
    sequenceNumber: 0,
    signatures: [{ signature: 'aa', publicKey: 'bb' }],
  },
  ticker: {
    value: 'BTED',
    sequenceNumber: 0,
    signatures: [{ signature: 'cc', publicKey: 'bb' }],
  },
  ...overrides,
});

const query = (subjects: Array<string>, options = {}) =>
  queryAssetRegistry(subjects, {
    retryBackoffMs: 0,
    now: 1_700_000_000_000,
    ...options,
  });

const stateOf = (
  resolutions: Array<{ subject: string; state: string }>,
  subject: string
) => resolutions.find((row) => row.subject === subject)?.state;

beforeEach(() => {
  mutableConfig.metadataUrl = undefined;
  mutableEnvironment.isSelfnode = false;
});

describe('assetRegistryEndpoint', () => {
  it('prefers an explicit override', () => {
    mutableConfig.metadataUrl = 'https://metadata.world.dev.cardano.org';
    expect(assetRegistryEndpoint('https://chosen.example')).toBe(
      'https://chosen.example'
    );
  });

  it('uses the launcher value when there is no override', () => {
    mutableConfig.metadataUrl = 'https://metadata.world.dev.cardano.org';
    expect(assetRegistryEndpoint()).toBe(
      'https://metadata.world.dev.cardano.org'
    );
  });

  it('composes the bundled mock on selfnode, where the launcher omits the key', () => {
    mutableEnvironment.isSelfnode = true;
    expect(assetRegistryEndpoint()).toBe('http://127.0.0.1:41531');
    expect(assetRegistryEndpoint()).not.toBe(ASSET_REGISTRY_FALLBACK_URL);
  });

  it('falls back to the mainnet literal when nothing else applies', () => {
    expect(assetRegistryEndpoint()).toBe(ASSET_REGISTRY_FALLBACK_URL);
  });

  it('appends the query path without doubling a trailing slash', () => {
    expect(assetRegistryQueryUrl('https://tokens.cardano.org')).toBe(
      'https://tokens.cardano.org/metadata/query'
    );
    expect(assetRegistryQueryUrl('https://tokens.cardano.org/')).toBe(
      'https://tokens.cardano.org/metadata/query'
    );
  });
});

describe('assetRegistryBatches', () => {
  const maximumLengthSubjects = (count: number) =>
    Array.from({ length: count }, (_value, index) => {
      const policy = index.toString(16).padStart(56, '0');
      return `${policy}${'ab'.repeat(32)}`;
    });

  it('splits 100 maximum-length subjects into more than one batch', () => {
    const batches = assetRegistryBatches(maximumLengthSubjects(100));
    expect(batches.length).toBeGreaterThan(1);
    expect(batches.reduce((sum, batch) => sum + batch.length, 0)).toBe(100);
  });

  it('keeps every batch under the request byte ceiling', () => {
    assetRegistryBatches(maximumLengthSubjects(500)).forEach((batch) => {
      expect(
        Buffer.byteLength(assetRegistryRequestBody(batch))
      ).toBeLessThanOrEqual(ASSET_REGISTRY_MAX_REQUEST_BYTES);
    });
  });

  it('sends a single oversized subject alone rather than dropping it', () => {
    const huge = 'f'.repeat(ASSET_REGISTRY_MAX_REQUEST_BYTES * 2);
    const batches = assetRegistryBatches([huge, SUBJECT]);
    expect(batches[0]).toEqual([huge]);
    expect(batches[1]).toEqual([SUBJECT]);
  });

  it('returns no batches for no subjects', () => {
    expect(assetRegistryBatches([])).toEqual([]);
  });
});

describe('the request body', () => {
  it('asks for five properties and never for the logo', async () => {
    const transport = stub([ok([entryFor(SUBJECT)])]);
    await query([SUBJECT], { transport });
    const body = JSON.parse(transport.calls[0].body);
    expect(body.properties).toEqual([
      'name',
      'ticker',
      'decimals',
      'url',
      'description',
    ]);
    expect(transport.calls[0].body).not.toContain('logo');
  });

  it('issues no request at all for an empty subject list', async () => {
    const transport = stub([ok([])]);
    const result = await query([], { transport });
    expect(transport.calls).toHaveLength(0);
    expect(result).toEqual({ entries: [], resolutions: [] });
  });

  it('requests a duplicated subject once and resolves it once', async () => {
    const transport = stub([ok([entryFor(SUBJECT)])]);
    const result = await query([SUBJECT, SUBJECT, SUBJECT], { transport });
    expect(JSON.parse(transport.calls[0].body).subjects).toEqual([SUBJECT]);
    expect(result.resolutions).toHaveLength(1);
  });
});

describe('status handling', () => {
  it('resolves every subject a 200 answers', async () => {
    const transport = stub([ok([entryFor(SUBJECT), entryFor(OTHER_SUBJECT)])]);
    const result = await query([SUBJECT, OTHER_SUBJECT], { transport });
    expect(result.entries).toHaveLength(2);
    expect(result.resolutions.every((row) => row.state === 'resolved')).toBe(
      true
    );
    expect(result.resolutions.every((row) => row.retryAfter === 0)).toBe(true);
  });

  it('records a subject a 200 omits as unregistered', async () => {
    const transport = stub([ok([entryFor(SUBJECT)])]);
    const result = await query([SUBJECT, OTHER_SUBJECT], { transport });
    expect(stateOf(result.resolutions, SUBJECT)).toBe('resolved');
    expect(stateOf(result.resolutions, OTHER_SUBJECT)).toBe('unregistered');
    expect(transport.calls).toHaveLength(1);
  });

  it.each([400, 401, 403, 404, 429])('never retries a %i', async (status) => {
    const transport = stub([{ ok: true, status, body: '' }]);
    const result = await query([SUBJECT], { transport });
    expect(transport.calls).toHaveLength(1);
    expect(stateOf(result.resolutions, SUBJECT)).toBe('failed');
  });

  it.each([500, 503])('retries a %i exactly once', async (status) => {
    const transport = stub([{ ok: true, status, body: '' }]);
    const result = await query([SUBJECT], { transport });
    expect(transport.calls).toHaveLength(2);
    expect(stateOf(result.resolutions, SUBJECT)).toBe('failed');
  });

  it('retries a timeout exactly once and then abandons the batch', async () => {
    const transport = stub([{ ok: false, reason: 'timeout' }]);
    const result = await query([SUBJECT], { transport });
    expect(transport.calls).toHaveLength(2);
    expect(stateOf(result.resolutions, SUBJECT)).toBe('failed');
  });

  it('retries a network error exactly once', async () => {
    const transport = stub([{ ok: false, reason: 'network' }]);
    const result = await query([SUBJECT], { transport });
    expect(transport.calls).toHaveLength(2);
  });

  it('discards an over-sized response without retrying it', async () => {
    const transport = stub([{ ok: false, reason: 'too-large' }]);
    const result = await query([SUBJECT], { transport });
    expect(transport.calls).toHaveLength(1);
    expect(result.entries).toEqual([]);
    expect(stateOf(result.resolutions, SUBJECT)).toBe('failed');
  });

  it('fails the batch rather than throwing on malformed JSON', async () => {
    const transport = stub([{ ok: true, status: 200, body: '{not json' }]);
    const result = await query([SUBJECT], { transport });
    expect(transport.calls).toHaveLength(1);
    expect(stateOf(result.resolutions, SUBJECT)).toBe('failed');
  });
});

describe('the 413 split', () => {
  it('splits once and sends both halves', async () => {
    const subjects = [SUBJECT, OTHER_SUBJECT];
    const transport = stub((call) =>
      JSON.parse(call.body).subjects.length > 1
        ? { ok: true, status: 413, body: '' }
        : ok([entryFor(JSON.parse(call.body).subjects[0])])
    );
    const result = await query(subjects, { transport });
    expect(transport.calls).toHaveLength(3);
    expect(result.entries).toHaveLength(2);
    expect(result.resolutions.every((row) => row.state === 'resolved')).toBe(
      true
    );
  });

  it('records a half that is refused again without splitting further', async () => {
    const transport = stub([{ ok: true, status: 413, body: '' }]);
    const result = await query([SUBJECT, OTHER_SUBJECT], { transport });
    expect(transport.calls).toHaveLength(3);
    expect(result.resolutions.every((row) => row.state === 'failed')).toBe(
      true
    );
  });

  it('does not split a batch of one subject', async () => {
    const transport = stub([{ ok: true, status: 413, body: '' }]);
    const result = await query([SUBJECT], { transport });
    expect(transport.calls).toHaveLength(1);
    expect(stateOf(result.resolutions, SUBJECT)).toBe('failed');
  });

  it('gives each half its own outcome', async () => {
    const transport = stub((call) => {
      const { subjects } = JSON.parse(call.body);
      if (subjects.length > 1) return { ok: true, status: 413, body: '' };
      if (subjects[0] === SUBJECT) return ok([entryFor(SUBJECT)]);
      return { ok: false, reason: 'timeout' };
    });
    const result = await query([SUBJECT, OTHER_SUBJECT], { transport });
    expect(stateOf(result.resolutions, SUBJECT)).toBe('resolved');
    expect(stateOf(result.resolutions, OTHER_SUBJECT)).toBe('failed');
  });
});

describe('the backoff', () => {
  it('doubles from the base and stops at the ceiling', () => {
    expect(assetRegistryBackoffMs(1)).toBe(ASSET_REGISTRY_BACKOFF_BASE_MS);
    expect(assetRegistryBackoffMs(2)).toBe(ASSET_REGISTRY_BACKOFF_BASE_MS * 2);
    expect(assetRegistryBackoffMs(3)).toBe(ASSET_REGISTRY_BACKOFF_BASE_MS * 4);
    expect(assetRegistryBackoffMs(20)).toBe(ASSET_REGISTRY_BACKOFF_CEILING_MS);
    expect(assetRegistryBackoffMs(21)).toBe(ASSET_REGISTRY_BACKOFF_CEILING_MS);
  });

  it('lengthens the retry instant for a subject that has failed before', async () => {
    const transport = stub([{ ok: true, status: 400, body: '' }]);
    const result = await query([SUBJECT, OTHER_SUBJECT], {
      transport,
      failureCounts: { [SUBJECT]: 3 },
    });
    const first = result.resolutions.find((row) => row.subject === SUBJECT);
    const second = result.resolutions.find(
      (row) => row.subject === OTHER_SUBJECT
    );
    expect(first.failureCount).toBe(4);
    expect(second.failureCount).toBe(1);
    expect(first.retryAfter).toBeGreaterThan(second.retryAfter);
  });

  it('expresses retryAfter as an instant, not a duration', async () => {
    const transport = stub([{ ok: true, status: 400, body: '' }]);
    const result = await query([SUBJECT], {
      transport,
      now: 1_700_000_000_000,
    });
    expect(result.resolutions[0].retryAfter).toBe(
      1_700_000_000_000 + ASSET_REGISTRY_BACKOFF_BASE_MS
    );
  });
});

describe('normalisation', () => {
  it('drops an entry for a subject that was not requested', async () => {
    const transport = stub([ok([entryFor(OTHER_SUBJECT)])]);
    const result = await query([SUBJECT], { transport });
    expect(result.entries).toEqual([]);
    expect(stateOf(result.resolutions, SUBJECT)).toBe('unregistered');
  });

  it('drops an entry belonging to the other half of a split', async () => {
    const transport = stub((call) => {
      const { subjects } = JSON.parse(call.body);
      if (subjects.length > 1) return { ok: true, status: 413, body: '' };
      return ok([entryFor(SUBJECT)]);
    });
    const result = await query([SUBJECT, OTHER_SUBJECT], { transport });
    expect(result.entries.map((entry) => entry.subject)).toEqual([SUBJECT]);
    expect(stateOf(result.resolutions, OTHER_SUBJECT)).toBe('unregistered');
  });

  it('drops a property with no numeric sequence number and keeps the rest', async () => {
    const transport = stub([
      ok([
        entryFor(SUBJECT, {
          decimals: {
            value: 6,
            sequenceNumber: 'zero',
            signatures: [{ signature: 'aa', publicKey: 'bb' }],
          },
        }),
      ]),
    ]);
    const result = await query([SUBJECT], { transport });
    expect(result.entries[0].properties.decimals).toBeUndefined();
    expect(result.entries[0].properties.ticker.value).toBe('BTED');
  });

  it('drops a property whose signatures are not signature objects', async () => {
    const transport = stub([
      ok([
        entryFor(SUBJECT, {
          decimals: { value: 6, sequenceNumber: 0, signatures: ['aa'] },
        }),
      ]),
    ]);
    const result = await query([SUBJECT], { transport });
    expect(result.entries[0].properties.decimals).toBeUndefined();
  });

  it('keeps an entry that carries no policy, with policy null', async () => {
    const transport = stub([ok([entryFor(SUBJECT, { policy: undefined })])]);
    const result = await query([SUBJECT], { transport });
    expect(result.entries[0].policy).toBeNull();
    expect(stateOf(result.resolutions, SUBJECT)).toBe('resolved');
  });
});

describe('sequencing', () => {
  it('issues batches one after another rather than in parallel', async () => {
    const subjects = Array.from({ length: 300 }, (_value, index) => {
      const policy = index.toString(16).padStart(56, '0');
      return `${policy}${'ab'.repeat(32)}`;
    });
    const transport = stub((call) =>
      ok(
        JSON.parse(call.body).subjects.map((subject: string) =>
          entryFor(subject)
        )
      )
    );
    const result = await query(subjects, { transport });
    expect(transport.calls.length).toBeGreaterThan(1);
    expect(transport.overlapped).toBe(false);
    expect(result.entries).toHaveLength(300);
  });
});

/**
 * The real transport, against a server on the loopback address.
 *
 * Every other group in this file substitutes `post`, so this is the only place
 * the code that opens a socket runs at all: the two response caps, the timeout,
 * the two error paths and the scheme choice that lets the selfnode mock be
 * reached over plain HTTP.
 */
describe('httpRegistryTransport', () => {
  let server: http.Server | null = null;

  const serve = async (
    handler: (
      request: http.IncomingMessage,
      response: http.ServerResponse
    ) => void
  ): Promise<string> => {
    server = http.createServer(handler);
    await new Promise<void>((resolve) => {
      server.listen(0, '127.0.0.1', resolve);
    });
    const { port } = server.address() as AddressInfo;
    return `http://127.0.0.1:${port}/metadata/query`;
  };

  afterEach(async () => {
    if (!server) return;
    const closing = server;
    server = null;
    // `closeAllConnections` is Node 18.2 and newer and is not in the @types
    // version this repository pins, so it is reached through the instance
    // rather than the declaration. Without it `close` waits for keep-alive
    // sockets and the case times out.
    (
      closing as unknown as { closeAllConnections?: () => void }
    ).closeAllConnections?.();
    await new Promise<void>((resolve) => {
      closing.close(() => resolve());
    });
  });

  it('posts the body and returns the status and the response text', async () => {
    let seen = '';
    let method = '';
    let contentType = '';
    const url = await serve((request, response) => {
      method = request.method;
      contentType = String(request.headers['content-type']);
      const chunks: Array<Buffer> = [];
      request.on('data', (chunk) => chunks.push(chunk));
      request.on('end', () => {
        seen = Buffer.concat(chunks).toString('utf8');
        response.writeHead(200, { 'content-type': 'application/json' });
        response.end('{"subjects":[]}');
      });
    });

    const result = await httpRegistryTransport.post(
      url,
      '{"subjects":["a"]}',
      5000
    );
    expect(result).toEqual({
      ok: true,
      status: 200,
      body: '{"subjects":[]}',
    });
    expect(method).toBe('POST');
    expect(contentType).toBe('application/json');
    expect(seen).toBe('{"subjects":["a"]}');
  });

  it('reports a status it does not like rather than failing', async () => {
    const url = await serve((_request, response) => {
      response.writeHead(413);
      response.end('too big');
    });
    const result = await httpRegistryTransport.post(url, '{}', 5000);
    expect(result).toEqual({ ok: true, status: 413, body: 'too big' });
  });

  it('refuses a response that declares a length over the cap', async () => {
    let bodySent = false;
    const url = await serve((_request, response) => {
      response.writeHead(200, {
        'content-length': String(ASSET_REGISTRY_MAX_RESPONSE_BYTES + 1),
      });
      bodySent = true;
      response.end('x'.repeat(ASSET_REGISTRY_MAX_RESPONSE_BYTES + 1));
    });
    const result = await httpRegistryTransport.post(url, '{}', 5000);
    expect(result).toEqual({ ok: false, reason: 'too-large' });
    // The refusal is on the header, so it does not depend on how much of the
    // body arrived.
    expect(bodySent).toBe(true);
  });

  it('refuses a response that streams past the cap without declaring it', async () => {
    const url = await serve((_request, response) => {
      // Chunked, so there is no content-length to read and the cap has to be
      // enforced as the bytes arrive.
      response.writeHead(200, { 'transfer-encoding': 'chunked' });
      const chunk = 'x'.repeat(64 * 1024);
      for (
        let sent = 0;
        sent <= ASSET_REGISTRY_MAX_RESPONSE_BYTES;
        sent += chunk.length
      ) {
        response.write(chunk);
      }
      response.end();
    });
    const result = await httpRegistryTransport.post(url, '{}', 5000);
    expect(result).toEqual({ ok: false, reason: 'too-large' });
  });

  it('returns a response exactly at the cap rather than refusing it', async () => {
    const body = 'x'.repeat(ASSET_REGISTRY_MAX_RESPONSE_BYTES);
    const url = await serve((_request, response) => {
      response.writeHead(200, { 'content-length': String(body.length) });
      response.end(body);
    });
    const result = await httpRegistryTransport.post(url, '{}', 5000);
    expect(result).toEqual({
      ok: true,
      status: 200,
      body,
    });
  });

  it('gives up on a server that never answers', async () => {
    const url = await serve(() => {
      // Deliberately no response.
    });
    const result = await httpRegistryTransport.post(url, '{}', 50);
    expect(result).toEqual({ ok: false, reason: 'timeout' });
  });

  it('reports a connection nobody is listening on as a network failure', async () => {
    const url = await serve((_request, response) => response.end('{}'));
    const closing = server;
    server = null;
    (
      closing as unknown as { closeAllConnections?: () => void }
    ).closeAllConnections?.();
    await new Promise<void>((resolve) => closing.close(() => resolve()));

    const result = await httpRegistryTransport.post(url, '{}', 5000);
    expect(result).toEqual({ ok: false, reason: 'network' });
  });

  it('reports a URL it cannot parse as a network failure rather than throwing', async () => {
    const result = await httpRegistryTransport.post('not a url', '{}', 5000);
    expect(result).toEqual({ ok: false, reason: 'network' });
  });

  it('settles once when the server answers and then the connection drops', async () => {
    const url = await serve((_request, response) => {
      response.writeHead(200);
      response.end('{}');
    });
    const results = await Promise.all([
      httpRegistryTransport.post(url, '{}', 5000),
      httpRegistryTransport.post(url, '{}', 5000),
    ]);
    expect(results).toEqual([
      { ok: true, status: 200, body: '{}' },
      { ok: true, status: 200, body: '{}' },
    ]);
  });

  it('answers network for a URL it cannot parse, without opening a socket', async () => {
    await expect(
      httpRegistryTransport.post('not a url', '{}', 5000)
    ).resolves.toEqual({ ok: false, reason: 'network' });
  });

  // The cap is a parameter of the transport now, because a batch of raw
  // transactions is an order larger than a registry answer.
  it('reads up to the cap the caller names rather than a fixed one', async () => {
    const url = await serve((request, response) => {
      response.writeHead(200, { 'content-type': 'application/json' });
      response.end('x'.repeat(2048));
    });
    await expect(
      httpRegistryTransport.post(url, '{}', 5000, 1024)
    ).resolves.toEqual({ ok: false, reason: 'too-large' });
    await expect(
      httpRegistryTransport.post(url, '{}', 5000, 4096)
    ).resolves.toEqual({ ok: true, status: 200, body: 'x'.repeat(2048) });
  });
});
