/**
 * Verification pipeline ordering: cache read, fetch, Blake2b-256 digest check
 * against the on-chain hash, and only then parse and cache write. The fetch
 * transport is fully mocked and every body is a committed fixture.
 *
 * @jest-environment node
 */
import fs from 'fs';
import os from 'os';
import path from 'path';
import { blake2bHex } from 'blakejs';
import {
  anchorDigest,
  resolveVerifiedAnchor,
} from '../../../source/main/governance/AnchorVerificationService';
import {
  readVerifiedAnchorBytes,
  writeVerifiedAnchorBytes,
} from '../../../source/main/governance/anchorCache';
import { fetchAnchorBytes } from '../../../source/main/governance/AnchorFetchService';
import { AnchorFetchErrorType } from '../../../source/common/types/governance.types';
import { logger } from '../../../source/main/utils/logging';

// main/config boots launcher configuration and throws outside an Electron
// launcher, so the state directory is redirected to a temp dir instead.
jest.mock('../../../source/main/config', () => {
  const nodeOs = require('os');
  const nodePath = require('path');
  return {
    stateDirectoryPath: nodePath.join(
      nodeOs.tmpdir(),
      'anchor-verification-spec'
    ),
  };
});

jest.mock('../../../source/main/governance/AnchorFetchService', () => ({
  fetchAnchorBytes: jest.fn(),
}));

const mockFetchAnchorBytes = fetchAnchorBytes as jest.Mock;

const FIXTURE_DIR = path.join(__dirname, '../../mocks/governance');
const SAMPLE_BYTES = fs.readFileSync(
  path.join(FIXTURE_DIR, 'anchor-cip119-sample.json')
);
const SAMPLE_HASH = fs
  .readFileSync(path.join(FIXTURE_DIR, 'anchor-cip119-sample.hash'), 'utf8')
  .trim();
const MALFORMED_BYTES = fs.readFileSync(
  path.join(FIXTURE_DIR, 'anchor-malformed.txt')
);
const ONCHAIN_URL =
  'https://raw.githubusercontent.com/cardano-foundation/cardano-academy/refs/heads/main/Cardano%20Academy.jsonld';
const OTHER_HASH =
  '9e8cb2b0f4c2ddbd9dea316b44680d8a989743868aeb40c1e6959982452f38e1';
const okResult = (bytes: Buffer) => ({
  ok: true,
  bytes,
  host: 'raw.githubusercontent.com',
  contentType: 'application/json',
  byteLength: bytes.length,
});

// Mirrors the mocked config factory above.
const cacheDirectory = path.join(
  os.tmpdir(),
  'anchor-verification-spec',
  'DRep-anchor-cache'
);

describe('AnchorVerificationService', () => {
  beforeEach(() => {
    jest.restoreAllMocks();
    mockFetchAnchorBytes.mockReset();
    fs.rmSync(cacheDirectory, { recursive: true, force: true });
  });

  it('verifies fetched bytes against the on-chain hash and returns the parsed name', async () => {
    mockFetchAnchorBytes.mockResolvedValue(okResult(SAMPLE_BYTES));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: SAMPLE_HASH,
    });
    expect(result).toMatchObject({
      status: 'verified',
      content: {
        givenName: 'Daedalus Test DRep',
        objectives:
          'Synthetic fixture objectives for offline anchor verification tests.',
        motivations:
          'Synthetic fixture motivations for offline anchor verification tests.',
        qualifications:
          'Synthetic fixture qualifications for offline anchor verification tests.',
        references: [],
        paymentAddress: null,
        doNotList: false,
      },
      host: 'raw.githubusercontent.com',
    });
    expect(typeof (result as { fetchedAt?: unknown }).fetchedAt).toBe('number');
  });

  it('agrees with an independent blake2b implementation and the committed digest', () => {
    const digest = anchorDigest(SAMPLE_BYTES);
    expect(digest).toBe(blake2bHex(SAMPLE_BYTES, null, 32));
    expect(digest).toBe(SAMPLE_HASH);
  });

  it('never parses bytes whose digest does not match the on-chain hash', async () => {
    mockFetchAnchorBytes.mockResolvedValue(okResult(SAMPLE_BYTES));
    const parseSpy = jest.spyOn(JSON, 'parse');
    const writeSpy = jest.spyOn(fs, 'writeFileSync');
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: OTHER_HASH,
    });
    expect(result).toEqual({
      status: 'unavailable',
      reason: AnchorFetchErrorType.HashMismatch,
    });
    expect(parseSpy).not.toHaveBeenCalled();
    expect(writeSpy).not.toHaveBeenCalled();
  });

  it('leaves the cache empty after a hash mismatch', async () => {
    mockFetchAnchorBytes.mockResolvedValue(okResult(SAMPLE_BYTES));
    await resolveVerifiedAnchor({ url: ONCHAIN_URL, hash: OTHER_HASH });
    expect(readVerifiedAnchorBytes(OTHER_HASH)).toBeNull();
    expect(fs.existsSync(path.join(cacheDirectory, `${OTHER_HASH}.json`))).toBe(
      false
    );
  });

  it('resolves as unavailable with no content when verified bytes fail to parse', async () => {
    mockFetchAnchorBytes.mockResolvedValue(okResult(MALFORMED_BYTES));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(MALFORMED_BYTES),
    });
    expect(result).toEqual({
      status: 'unavailable',
      reason: AnchorFetchErrorType.ParseFailed,
    });
    expect('content' in result).toBe(false);
  });

  it('verifies a body with no givenName and leaves every field at its empty value', async () => {
    const bytes = Buffer.from(JSON.stringify({ body: {} }));
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect(result).toMatchObject({
      status: 'verified',
      content: {
        givenName: null,
        objectives: null,
        motivations: null,
        qualifications: null,
        references: [],
        paymentAddress: null,
        doNotList: false,
      },
    });
  });

  it('keeps a doNotList opt-out from a body that carries no givenName', async () => {
    const bytes = Buffer.from(JSON.stringify({ body: { doNotList: true } }));
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect(result).toMatchObject({
      status: 'verified',
      content: { doNotList: true },
    });
  });

  it('still fails to parse a body that is missing, null or not an object', async () => {
    for (const body of [undefined, null, [], 'text']) {
      const bytes = Buffer.from(JSON.stringify({ body }));
      mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
      const result = await resolveVerifiedAnchor({
        url: ONCHAIN_URL,
        hash: anchorDigest(bytes),
      });
      expect(result).toEqual({
        status: 'unavailable',
        reason: AnchorFetchErrorType.ParseFailed,
      });
    }
  });

  it('splits references into link, identity and default buckets', async () => {
    const bytes = Buffer.from(
      JSON.stringify({
        body: {
          references: [
            { '@type': 'Link', label: 'Blog', uri: 'https://example.org/blog' },
            {
              '@type': 'Identity',
              label: 'Profile',
              uri: 'https://example.org/id',
            },
            { '@type': 'CIP119:Identity', uri: 'https://example.org/id2' },
            { '@type': 'Something', uri: 'https://example.org/other' },
            { uri: 'https://example.org/untyped' },
          ],
        },
      })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect((result as any).content.references).toEqual([
      { type: 'link', label: 'Blog', uri: 'https://example.org/blog' },
      { type: 'identity', label: 'Profile', uri: 'https://example.org/id' },
      { type: 'identity', label: null, uri: 'https://example.org/id2' },
      { type: 'other', label: null, uri: 'https://example.org/other' },
      { type: 'other', label: null, uri: 'https://example.org/untyped' },
    ]);
  });

  it('drops reference entries with no uri and keeps the rest, however many', async () => {
    // An entry without a uri is not a reference. A long list of real ones is
    // just a long list, and the transport already bounds the document.
    const many = Array.from({ length: 25 }, (_unused, index) => ({
      '@type': 'Link',
      uri: `https://example.org/${index}`,
    }));
    const bytes = Buffer.from(
      JSON.stringify({ body: { references: [{ label: 'no uri' }, ...many] } })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect((result as any).content.references).toHaveLength(25);
  });

  it('carries fields no standard defines, under the keys their author chose', async () => {
    const bytes = Buffer.from(
      JSON.stringify({
        body: {
          givenName: 'Named DRep',
          bio: 'A biography',
          nationality: 'Japan',
          verifiedBy: 'Cardano Foundation',
        },
      })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });

    expect((result as any).content.additionalFields).toEqual([
      { key: 'bio', value: { kind: 'text', text: 'A biography' } },
      { key: 'nationality', value: { kind: 'text', text: 'Japan' } },
      // Carried, not endorsed. The renderer puts it in a quieter block under
      // the author's own key rather than beside a heading we wrote.
      {
        key: 'verifiedBy',
        value: { kind: 'text', text: 'Cardano Foundation' },
      },
    ]);
  });

  it('withholds images in either form, and the dRepName duplicate', async () => {
    const bytes = Buffer.from(
      JSON.stringify({
        body: {
          givenName: 'Named DRep',
          dRepName: 'Named DRep',
          image: {
            '@type': 'ImageObject',
            contentUrl: 'data:image/jpeg;base64,AAAA',
          },
          logo: 'https://example.org/logo.png',
        },
      })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });

    // A linked image would disclose the reader's address to a host the DRep
    // chose, at page load and with no click; an inlined one costs weight. And
    // dRepName repeats givenName in every sampled case, so surfacing it would
    // print the same name twice under two headings.
    expect((result as any).content.additionalFields).toEqual([]);
  });

  it('refuses a data URI under any key, not only the ones named image', async () => {
    const bytes = Buffer.from(
      JSON.stringify({
        body: {
          avatar: 'data:image/jpeg;base64,/9j/4RM/RXhpZgAASUkqAAgAAAAM',
          banner: 'DATA:image/png;base64,iVBORw0KGgo=',
          padded: '   data:text/html,<script>1</script>',
          nationality: 'Japan',
        },
      })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });

    // Banning by key name only bans the keys someone thought of. The payload
    // is the same whatever it is called, carries nothing a reader can read,
    // and the largest in the mainnet sample is 268,625 bytes of base64.
    expect((result as any).content.additionalFields).toEqual([
      { key: 'nationality', value: { kind: 'text', text: 'Japan' } },
    ]);
  });

  it('keeps the structure a DRep gave a field of their own', async () => {
    // A multi-sig DRep listing its members. Flattening this loses which name
    // went with which title; dropping it loses the members.
    const bytes = Buffer.from(
      JSON.stringify({
        body: {
          members: [
            { name: 'A. Signer', title: 'Treasurer', company: 'Example Ltd' },
            { name: 'B. Signer', title: 'Secretary' },
          ],
          threshold: 2,
          active: true,
        },
      })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });

    expect((result as any).content.additionalFields).toEqual([
      {
        key: 'members',
        value: {
          kind: 'list',
          items: [
            {
              kind: 'group',
              fields: [
                { key: 'name', value: { kind: 'text', text: 'A. Signer' } },
                { key: 'title', value: { kind: 'text', text: 'Treasurer' } },
                {
                  key: 'company',
                  value: { kind: 'text', text: 'Example Ltd' },
                },
              ],
            },
            {
              kind: 'group',
              fields: [
                { key: 'name', value: { kind: 'text', text: 'B. Signer' } },
                { key: 'title', value: { kind: 'text', text: 'Secretary' } },
              ],
            },
          ],
        },
      },
      // Numbers and booleans are values a reader can read.
      { key: 'threshold', value: { kind: 'text', text: '2' } },
      { key: 'active', value: { kind: 'text', text: 'true' } },
    ]);
  });

  it('stops following structure past the nesting bound', async () => {
    // The renderer walks this tree recursively, so a document nesting far
    // enough would exhaust the stack rather than produce a page. The bound is
    // structural: it limits how deep the reader follows, not how much a DRep
    // may say.
    let nested: Record<string, unknown> = { leaf: 'too deep to reach' };
    for (let i = 0; i < 12; i += 1) nested = { deeper: nested };
    const bytes = Buffer.from(
      JSON.stringify({ body: { runaway: nested, shallow: 'reachable' } })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });

    const fields = (result as any).content.additionalFields;
    // The shallow field survives; the runaway one is cut off rather than
    // taking the page with it.
    expect(fields.map((f: any) => f.key)).toContain('shallow');
    const depthOf = (value: any): number =>
      value.kind === 'group'
        ? 1 + Math.max(...value.fields.map((f: any) => depthOf(f.value)))
        : value.kind === 'list'
          ? 1 + Math.max(...value.items.map(depthOf))
          : 0;
    const runaway = fields.find((f: any) => f.key === 'runaway');
    if (runaway) expect(depthOf(runaway.value)).toBeLessThanOrEqual(8);
  });

  it('keeps JSON-LD scaffolding and empty values out of the content', async () => {
    const bytes = Buffer.from(
      JSON.stringify({
        body: {
          blank: '   ',
          empty: [],
          nothing: null,
          '@context': 'https://example.org/context',
          real: 'kept',
        },
      })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });

    // Structure is content and is kept; scaffolding, blanks and empties are
    // not, and a field with nothing behind it is worse than no field.
    expect((result as any).content.additionalFields).toEqual([
      { key: 'real', value: { kind: 'text', text: 'kept' } },
    ]);
  });

  it('keeps the longest prose mainnet actually carries', async () => {
    // A 3,374-character motivations is the longest observed on mainnet. The
    // bound exists so one hostile anchor cannot produce an unbounded view, not
    // to shorten a DRep who wrote at length.
    const prose = 'm'.repeat(3374);
    const bytes = Buffer.from(JSON.stringify({ body: { motivations: prose } }));
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect((result as any).content.motivations).toBe(prose);
  });

  it('keeps prose of any length the document can carry', async () => {
    // Volume is bounded once, at the transport. Clamping here would destroy the
    // text before the cache or the renderer saw it, leaving no way to offer the
    // rest later, and would cut a DRep off mid-sentence with nothing to say so.
    const prose = 'o'.repeat(50000);
    const bytes = Buffer.from(JSON.stringify({ body: { objectives: prose } }));
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect((result as any).content.objectives).toBe(prose);
  });

  it('clamps givenName at eighty characters', async () => {
    const bytes = Buffer.from(
      JSON.stringify({ body: { givenName: 'n'.repeat(500) } })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect((result as any).content.givenName).toHaveLength(80);
  });

  it('drops an over-length payment address instead of truncating it', async () => {
    const bytes = Buffer.from(
      JSON.stringify({ body: { paymentAddress: `addr1${'q'.repeat(200)}` } })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect((result as any).content.paymentAddress).toBeNull();
  });

  it('reads the JSON-LD @value wrapper form for strings and booleans', async () => {
    const bytes = Buffer.from(
      JSON.stringify({
        body: {
          givenName: { '@value': 'Wrapped Name' },
          doNotList: { '@value': true },
        },
      })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect(result).toMatchObject({
      content: { givenName: 'Wrapped Name', doNotList: true },
    });
  });

  it('serves a cache hit without issuing a fetch', async () => {
    writeVerifiedAnchorBytes(SAMPLE_HASH, SAMPLE_BYTES);
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: SAMPLE_HASH,
    });
    expect(mockFetchAnchorBytes).not.toHaveBeenCalled();
    expect(result).toMatchObject({
      status: 'verified',
      content: { givenName: 'Daedalus Test DRep' },
    });
  });

  it('never serves bytes cached under a previous on-chain hash', async () => {
    writeVerifiedAnchorBytes(SAMPLE_HASH, SAMPLE_BYTES);
    mockFetchAnchorBytes.mockResolvedValue({
      ok: false,
      reason: AnchorFetchErrorType.HttpStatus,
    });
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: OTHER_HASH,
    });
    expect(mockFetchAnchorBytes).toHaveBeenCalledTimes(1);
    expect(result).toEqual({
      status: 'unavailable',
      reason: AnchorFetchErrorType.HttpStatus,
    });
    expect(
      fs.existsSync(path.join(cacheDirectory, `${SAMPLE_HASH}.json`))
    ).toBe(true);
  });

  it('refetches when a cached file no longer matches its own filename digest', async () => {
    fs.mkdirSync(cacheDirectory, { recursive: true });
    const entryPath = path.join(cacheDirectory, `${SAMPLE_HASH}.json`);
    fs.writeFileSync(entryPath, Buffer.from('tampered'));
    mockFetchAnchorBytes.mockResolvedValue(okResult(SAMPLE_BYTES));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: SAMPLE_HASH,
    });
    expect(mockFetchAnchorBytes).toHaveBeenCalledTimes(1);
    expect(fs.readFileSync(entryPath).toString('utf8')).not.toContain(
      'tampered'
    );
    expect(result).toMatchObject({ status: 'verified' });
  });

  it('passes transport failure reasons through unchanged', async () => {
    const reasons = [
      AnchorFetchErrorType.TooLarge,
      AnchorFetchErrorType.Timeout,
      AnchorFetchErrorType.BlockedAddress,
    ];
    for (const reason of reasons) {
      mockFetchAnchorBytes.mockResolvedValue({ ok: false, reason });
      const result = await resolveVerifiedAnchor({
        url: ONCHAIN_URL,
        hash: SAMPLE_HASH,
      });
      expect(result).toEqual({ status: 'unavailable', reason });
    }
  });

  it('rejects an invalid on-chain hash before touching disk or network', async () => {
    const readSpy = jest.spyOn(fs, 'readFileSync');
    const writeSpy = jest.spyOn(fs, 'writeFileSync');
    const invalidHashes = [
      '../../etc/passwd',
      'ZZZZ',
      '',
      `${SAMPLE_HASH.toUpperCase()}x`,
    ];
    for (const hash of invalidHashes) {
      const result = await resolveVerifiedAnchor({ url: ONCHAIN_URL, hash });
      expect(result).toEqual({
        status: 'unavailable',
        reason: AnchorFetchErrorType.InvalidRequest,
      });
    }
    expect(mockFetchAnchorBytes).not.toHaveBeenCalled();
    expect(readSpy).not.toHaveBeenCalled();
    expect(writeSpy).not.toHaveBeenCalled();
  });

  it('collapses concurrent resolutions for one anchor into a single fetch', async () => {
    mockFetchAnchorBytes.mockImplementation(() =>
      Promise.resolve(okResult(SAMPLE_BYTES))
    );
    const [first, second] = await Promise.all([
      resolveVerifiedAnchor({ url: ONCHAIN_URL, hash: SAMPLE_HASH }),
      resolveVerifiedAnchor({ url: ONCHAIN_URL, hash: SAMPLE_HASH }),
    ]);
    expect(mockFetchAnchorBytes).toHaveBeenCalledTimes(1);
    expect(first).toMatchObject({ status: 'verified' });
    expect(second).toMatchObject({ status: 'verified' });
  });

  it('logs no anchor url, host, hash or identity on any resolution path', async () => {
    const spies = [
      jest.spyOn(logger, 'debug').mockImplementation(() => {}),
      jest.spyOn(logger, 'info').mockImplementation(() => {}),
      jest.spyOn(logger, 'warn').mockImplementation(() => {}),
      jest.spyOn(logger, 'error').mockImplementation(() => {}),
    ];

    mockFetchAnchorBytes.mockResolvedValue(okResult(SAMPLE_BYTES));
    await resolveVerifiedAnchor({ url: ONCHAIN_URL, hash: SAMPLE_HASH });
    await resolveVerifiedAnchor({ url: ONCHAIN_URL, hash: OTHER_HASH });
    mockFetchAnchorBytes.mockResolvedValue(okResult(MALFORMED_BYTES));
    await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(MALFORMED_BYTES),
    });
    await resolveVerifiedAnchor({ url: ONCHAIN_URL, hash: 'ZZZZ' });

    jest.spyOn(fs, 'writeFileSync').mockImplementation(() => {
      const err: NodeJS.ErrnoException = new Error('write refused');
      err.code = 'EACCES';
      throw err;
    });
    writeVerifiedAnchorBytes(OTHER_HASH, SAMPLE_BYTES);

    const serialized = JSON.stringify(spies.map((spy) => spy.mock.calls));
    expect(serialized).not.toContain(ONCHAIN_URL);
    expect(serialized).not.toContain('raw.githubusercontent.com');
    expect(serialized).not.toContain(SAMPLE_HASH);
    expect(serialized).not.toContain(OTHER_HASH);
    expect(serialized).not.toContain('drep1');
  });
});
