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
      content: { givenName: 'Daedalus Test DRep' },
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

  it('treats a body without a givenName as a parse failure', async () => {
    const bytes = Buffer.from(JSON.stringify({ body: {} }));
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect(result).toEqual({
      status: 'unavailable',
      reason: AnchorFetchErrorType.ParseFailed,
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
