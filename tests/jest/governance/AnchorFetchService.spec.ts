/**
 * Transport-security floor for the anchor fetch service: scheme allow-list,
 * SSRF address guard, DNS-rebinding pinning, TLS, redirects, timeouts, the
 * response-size cap and the content-type allow-list.
 *
 * Uses deterministic jest.mock over https.request and dns.promises.lookup — no
 * real socket is opened.
 *
 * @jest-environment node
 */
import { EventEmitter } from 'events';
import https from 'https';
import dns from 'dns';
import fs from 'fs';
import {
  fetchAnchorBytes,
  isBlockedAnchorAddress,
  httpsAnchorTransport,
  ANCHOR_MAX_BYTES,
  ANCHOR_TIMEOUT_MS,
} from '../../../source/main/governance/AnchorFetchService';
import { AnchorFetchErrorType } from '../../../source/common/types/governance.types';

jest.mock('https', () => {
  const actual = jest.requireActual('https');
  return {
    ...actual,
    request: jest.fn(),
  };
});

jest.mock('dns', () => {
  const actual = jest.requireActual('dns');
  return {
    ...actual,
    promises: { ...actual.promises, lookup: jest.fn() },
  };
});

const mockRequest = (https.request as unknown) as jest.Mock;
const mockLookup = (dns.promises.lookup as unknown) as jest.Mock;
const ANCHOR_URL = 'https://anchor.example.org/profile.jsonld';

class FakeResponse extends EventEmitter {
  statusCode = 200;
  headers: Record<string, string> = { 'content-type': 'application/json' };
  destroy = jest.fn();
}

class FakeRequest extends EventEmitter {
  end = jest.fn();
  destroy = jest.fn();
}

function primeTransport(response: FakeResponse | null, body?: string) {
  const request = new FakeRequest();
  mockRequest.mockImplementation((_options: any, callback: any) => {
    if (response) {
      process.nextTick(() => {
        callback(response);
        process.nextTick(() => {
          if (body !== undefined) {
            response.emit('data', Buffer.from(body));
            response.emit('end');
          }
        });
      });
    }
    return request;
  });
  return request;
}

function primeChunkedTransport(
  response: FakeResponse,
  chunks: Buffer[],
  end = true
) {
  const request = new FakeRequest();
  mockRequest.mockImplementation((_options: any, callback: any) => {
    process.nextTick(() => {
      callback(response);
      process.nextTick(() => {
        chunks.forEach((chunk) => response.emit('data', chunk));
        if (end) response.emit('end');
      });
    });
    return request;
  });
  return request;
}

function primeRequestEvent(event: string, ...args: any[]) {
  const request = new FakeRequest();
  mockRequest.mockImplementation(() => {
    process.nextTick(() => request.emit(event, ...args));
    return request;
  });
  return request;
}

beforeEach(() => {
  mockLookup.mockResolvedValue([{ address: '93.184.216.34', family: 4 }]);
});

describe('Anchor fetch service — transport selection', () => {
  it('exposes the https transport under the https scheme', () => {
    expect(httpsAnchorTransport.scheme).toBe('https:');
  });

  it('rejects an http url without opening a socket', async () => {
    const result = await fetchAnchorBytes(
      'http://anchor.example.org/profile.jsonld'
    );
    expect(result).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.UnsupportedScheme,
    });
    expect(mockRequest).not.toHaveBeenCalled();
  });

  it('rejects an ipfs url because no transport is registered for it', async () => {
    const result = await fetchAnchorBytes('ipfs://bafybeigdyrztprofile');
    expect(result).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.UnsupportedScheme,
    });
    expect(mockRequest).not.toHaveBeenCalled();
  });

  it('rejects a malformed url', async () => {
    const result = await fetchAnchorBytes('not a url');
    expect(result).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.InvalidRequest,
    });
  });
});

describe('Anchor fetch service — address guard', () => {
  it('blocks RFC 1918 ranges', () => {
    expect(isBlockedAnchorAddress('10.0.0.1')).toBe(true);
    expect(isBlockedAnchorAddress('172.16.5.5')).toBe(true);
    expect(isBlockedAnchorAddress('172.31.255.255')).toBe(true);
    expect(isBlockedAnchorAddress('192.168.1.1')).toBe(true);
  });

  it('blocks loopback', () => {
    expect(isBlockedAnchorAddress('127.0.0.1')).toBe(true);
    expect(isBlockedAnchorAddress('127.10.0.1')).toBe(true);
  });

  it('blocks link-local including the cloud metadata address', () => {
    expect(isBlockedAnchorAddress('169.254.0.1')).toBe(true);
    expect(isBlockedAnchorAddress('169.254.169.254')).toBe(true);
  });

  it('blocks the 0.0.0.0/8 range', () => {
    expect(isBlockedAnchorAddress('0.0.0.0')).toBe(true);
    expect(isBlockedAnchorAddress('0.1.2.3')).toBe(true);
  });

  it('blocks shared, protocol-assignment, benchmarking, multicast and reserved v4 ranges', () => {
    expect(isBlockedAnchorAddress('100.64.0.1')).toBe(true);
    expect(isBlockedAnchorAddress('192.0.0.1')).toBe(true);
    expect(isBlockedAnchorAddress('198.18.0.1')).toBe(true);
    expect(isBlockedAnchorAddress('224.0.0.1')).toBe(true);
    expect(isBlockedAnchorAddress('255.255.255.255')).toBe(true);
  });

  it('blocks the IPv6 unspecified and loopback addresses', () => {
    expect(isBlockedAnchorAddress('::')).toBe(true);
    expect(isBlockedAnchorAddress('::1')).toBe(true);
  });

  it('blocks IPv6 unique local and link-local addresses', () => {
    expect(isBlockedAnchorAddress('fc00::1')).toBe(true);
    expect(isBlockedAnchorAddress('fd00::1')).toBe(true);
    expect(isBlockedAnchorAddress('fe80::1')).toBe(true);
  });

  it('blocks IPv6 documentation, 6to4, Teredo, NAT64 and multicast ranges', () => {
    expect(isBlockedAnchorAddress('2001:db8::1')).toBe(true);
    expect(isBlockedAnchorAddress('2002:7f00:1::')).toBe(true);
    expect(isBlockedAnchorAddress('2001:0:53aa::1')).toBe(true);
    expect(isBlockedAnchorAddress('64:ff9b::7f00:1')).toBe(true);
    expect(isBlockedAnchorAddress('ff02::1')).toBe(true);
  });

  it('blocks an IPv4-mapped IPv6 address wrapping a private address and allows one wrapping a public address', () => {
    expect(isBlockedAnchorAddress('::ffff:127.0.0.1')).toBe(true);
    expect(isBlockedAnchorAddress('::ffff:93.184.216.34')).toBe(false);
  });

  it('allows public addresses and blocks anything that is not an IP', () => {
    expect(isBlockedAnchorAddress('93.184.216.34')).toBe(false);
    expect(isBlockedAnchorAddress('8.8.8.8')).toBe(false);
    expect(isBlockedAnchorAddress('172.32.0.1')).toBe(false);
    expect(isBlockedAnchorAddress('2606:4700:4700::1111')).toBe(false);
    expect(isBlockedAnchorAddress('not-an-ip')).toBe(true);
    expect(isBlockedAnchorAddress('')).toBe(true);
  });
});

describe('Anchor fetch service — DNS resolution and rebinding', () => {
  it('rejects a host that resolves to a private address before opening a socket', async () => {
    mockLookup.mockResolvedValue([{ address: '10.0.0.5', family: 4 }]);
    const result = await fetchAnchorBytes(ANCHOR_URL);
    expect(result).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.BlockedAddress,
    });
    expect(mockRequest).not.toHaveBeenCalled();
  });

  it('rejects when any resolved address is private', async () => {
    mockLookup.mockResolvedValue([
      { address: '93.184.216.34', family: 4 },
      { address: '10.0.0.5', family: 4 },
    ]);
    const result = await fetchAnchorBytes(ANCHOR_URL);
    expect(result).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.BlockedAddress,
    });
    expect(mockRequest).not.toHaveBeenCalled();
  });

  it('maps a resolver failure', async () => {
    mockLookup.mockRejectedValue({ code: 'ENOTFOUND' });
    const result = await fetchAnchorBytes(ANCHOR_URL);
    expect(result).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.DnsFailed,
    });
  });

  it('maps an empty resolution', async () => {
    mockLookup.mockResolvedValue([]);
    const result = await fetchAnchorBytes(ANCHOR_URL);
    expect(result).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.DnsFailed,
    });
  });

  it('pins the validated address through a custom lookup while the hostname and servername stay the original host', async () => {
    primeTransport(new FakeResponse(), '{}');
    await fetchAnchorBytes(ANCHOR_URL);

    const options = mockRequest.mock.calls[0][0];
    expect(options.hostname).toBe('anchor.example.org');
    expect(options.servername).toBe('anchor.example.org');
    expect(options.path).toBe('/profile.jsonld');

    const callback = jest.fn();
    options.lookup('anchor.example.org', {}, callback);
    expect(callback).toHaveBeenCalledWith(null, '93.184.216.34', 4);
  });
});

describe('Anchor fetch service — TLS', () => {
  it('never sets rejectUnauthorized on the outgoing request', async () => {
    primeTransport(new FakeResponse(), '{}');
    await fetchAnchorBytes(ANCHOR_URL);
    expect(mockRequest.mock.calls[0][0].rejectUnauthorized).toBeUndefined();
  });

  it('maps certificate errors', async () => {
    primeRequestEvent(
      'error',
      Object.assign(new Error('certificate rejected'), {
        code: 'CERT_HAS_EXPIRED',
      })
    );
    expect(await fetchAnchorBytes(ANCHOR_URL)).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.TlsFailed,
    });

    primeRequestEvent(
      'error',
      Object.assign(new Error('certificate rejected'), {
        code: 'ERR_TLS_CERT_ALTNAME_INVALID',
      })
    );
    expect(await fetchAnchorBytes(ANCHOR_URL)).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.TlsFailed,
    });
  });
});

describe('Anchor fetch service — redirects and status codes', () => {
  it('rejects a 302 and never follows the location header', async () => {
    const response = new FakeResponse();
    response.statusCode = 302;
    response.headers = { location: 'https://evil.example.net/profile.jsonld' };
    primeTransport(response);

    const result = await fetchAnchorBytes(ANCHOR_URL);
    expect(result).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.Redirected,
    });
    expect(mockRequest).toHaveBeenCalledTimes(1);
    expect(response.destroy).toHaveBeenCalled();
  });

  it('rejects 301, 307 and 308 the same way', async () => {
    const statusCodes = [301, 307, 308];
    // eslint-disable-next-line no-restricted-syntax
    for (const statusCode of statusCodes) {
      const response = new FakeResponse();
      response.statusCode = statusCode;
      primeTransport(response);
      // eslint-disable-next-line no-await-in-loop
      expect(await fetchAnchorBytes(ANCHOR_URL)).toEqual({
        ok: false,
        reason: AnchorFetchErrorType.Redirected,
      });
    }
  });

  it('rejects a 404 with the same result shape as a redirect', async () => {
    const notFound = new FakeResponse();
    notFound.statusCode = 404;
    primeTransport(notFound);
    const notFoundResult = await fetchAnchorBytes(ANCHOR_URL);

    const redirect = new FakeResponse();
    redirect.statusCode = 302;
    primeTransport(redirect);
    const redirectResult = await fetchAnchorBytes(ANCHOR_URL);

    expect(Object.keys(notFoundResult).sort()).toEqual(
      Object.keys(redirectResult).sort()
    );
    expect(notFoundResult.ok).toBe(false);
    expect(redirectResult.ok).toBe(false);
    expect(notFoundResult).not.toHaveProperty('bytes');
    expect(notFoundResult).not.toHaveProperty('host');
    expect(redirectResult).not.toHaveProperty('bytes');
    expect(redirectResult).not.toHaveProperty('host');
  });

  it('rejects a 500', async () => {
    const response = new FakeResponse();
    response.statusCode = 500;
    primeTransport(response);
    expect(await fetchAnchorBytes(ANCHOR_URL)).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.HttpStatus,
    });
  });
});

describe('Anchor fetch service — timeouts', () => {
  it('sets a socket timeout no larger than the ten second budget', async () => {
    primeTransport(new FakeResponse(), '{}');
    await fetchAnchorBytes(ANCHOR_URL);

    const { timeout } = mockRequest.mock.calls[0][0];
    expect(timeout).toBeGreaterThan(0);
    expect(timeout).toBeLessThanOrEqual(ANCHOR_TIMEOUT_MS);
    expect(ANCHOR_TIMEOUT_MS).toBe(10000);
  });

  it('maps a socket timeout event and destroys the request', async () => {
    const request = primeRequestEvent('timeout');
    expect(await fetchAnchorBytes(ANCHOR_URL)).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.Timeout,
    });
    expect(request.destroy).toHaveBeenCalled();
  });

  it('aborts a response that never ends once the total budget elapses', async () => {
    jest.useFakeTimers();
    try {
      const request = primeTransport(new FakeResponse());
      const pending = fetchAnchorBytes(ANCHOR_URL);
      for (let tick = 0; tick < 10; tick += 1) {
        // eslint-disable-next-line no-await-in-loop
        await Promise.resolve();
      }
      jest.advanceTimersByTime(ANCHOR_TIMEOUT_MS);

      expect(await pending).toEqual({
        ok: false,
        reason: AnchorFetchErrorType.Timeout,
      });
      expect(request.destroy).toHaveBeenCalled();
    } finally {
      jest.useRealTimers();
    }
  });
});

describe('Anchor fetch service — response size cap', () => {
  it('rejects a declared content-length above the cap before reading a body', async () => {
    const response = new FakeResponse();
    response.headers = {
      'content-type': 'application/json',
      'content-length': String(ANCHOR_MAX_BYTES + 1),
    };
    primeTransport(response);

    expect(await fetchAnchorBytes(ANCHOR_URL)).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.TooLarge,
    });
    expect(response.listenerCount('data')).toBe(0);
  });

  it('aborts mid-stream when the body overflows the cap', async () => {
    const response = new FakeResponse();
    const request = primeChunkedTransport(response, [
      Buffer.alloc(ANCHOR_MAX_BYTES),
      Buffer.alloc(1),
    ]);

    expect(await fetchAnchorBytes(ANCHOR_URL)).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.TooLarge,
    });
    expect(response.destroy).toHaveBeenCalled();
    expect(request.destroy).toHaveBeenCalled();
  });

  it('accepts a body exactly at the cap', async () => {
    const response = new FakeResponse();
    primeChunkedTransport(response, [Buffer.alloc(ANCHOR_MAX_BYTES)]);

    const result = await fetchAnchorBytes(ANCHOR_URL);
    if (!result.ok) throw new Error('expected the capped body to be accepted');
    expect(result.byteLength).toBe(ANCHOR_MAX_BYTES);
    expect(ANCHOR_MAX_BYTES).toBe(1024 * 1024);
  });
});

describe('Anchor fetch service — content type', () => {
  it('accepts application/json', async () => {
    primeTransport(new FakeResponse(), '{"givenName":"Sample"}');
    const result = await fetchAnchorBytes(ANCHOR_URL);
    if (!result.ok) throw new Error('expected application/json to be accepted');
    expect(result.contentType).toBe('application/json');
  });

  it('accepts application/ld+json with parameters', async () => {
    const response = new FakeResponse();
    response.headers = { 'content-type': 'application/ld+json; charset=utf-8' };
    primeTransport(response, '{"givenName":"Sample"}');

    const result = await fetchAnchorBytes(ANCHOR_URL);
    if (!result.ok) throw new Error('expected ld+json to be accepted');
    expect(result.contentType).toBe('application/ld+json');
  });

  it('rejects a disallowed content type', async () => {
    const response = new FakeResponse();
    response.headers = { 'content-type': 'text/html; charset=utf-8' };
    primeTransport(response);

    expect(await fetchAnchorBytes(ANCHOR_URL)).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.ContentType,
    });
    expect(response.destroy).toHaveBeenCalled();
  });

  it('rejects a missing content type', async () => {
    const response = new FakeResponse();
    response.headers = {};
    primeTransport(response);

    expect(await fetchAnchorBytes(ANCHOR_URL)).toEqual({
      ok: false,
      reason: AnchorFetchErrorType.ContentType,
    });
  });
});

describe('Anchor fetch service — result shape', () => {
  it('returns bounded raw bytes and transport metadata only', async () => {
    const body = '{"givenName":"Sample"}';
    primeTransport(new FakeResponse(), body);

    expect(await fetchAnchorBytes(ANCHOR_URL)).toEqual({
      ok: true,
      bytes: Buffer.from(body),
      host: 'anchor.example.org',
      contentType: 'application/json',
      byteLength: body.length,
    });
  });

  it('never parses the body', async () => {
    const body = '{"body":';
    primeTransport(new FakeResponse(), body);

    const result = await fetchAnchorBytes(ANCHOR_URL);
    if (!result.ok) throw new Error('expected the raw body to be returned');
    expect(result.bytes.toString()).toBe(body);
  });

  it('writes nothing to the filesystem on any path', async () => {
    const writeFileSyncSpy = jest
      .spyOn(fs, 'writeFileSync')
      .mockImplementation(() => undefined);
    const writeFileSpy = jest
      .spyOn(fs.promises, 'writeFile')
      .mockResolvedValue(undefined);
    try {
      primeTransport(new FakeResponse(), '{"givenName":"Sample"}');
      await fetchAnchorBytes(ANCHOR_URL);

      const failure = new FakeResponse();
      failure.statusCode = 500;
      primeTransport(failure);
      await fetchAnchorBytes(ANCHOR_URL);

      expect(writeFileSyncSpy).not.toHaveBeenCalled();
      expect(writeFileSpy).not.toHaveBeenCalled();
    } finally {
      writeFileSyncSpy.mockRestore();
      writeFileSpy.mockRestore();
    }
  });
});

describe('Anchor fetch service — DNS budget', () => {
  it('aborts when DNS resolution never settles', async () => {
    jest.useFakeTimers();
    try {
      mockLookup.mockReturnValue(new Promise(() => {}));
      const pending = fetchAnchorBytes(ANCHOR_URL);
      for (let tick = 0; tick < 10; tick += 1) {
        // eslint-disable-next-line no-await-in-loop
        await Promise.resolve();
      }
      jest.advanceTimersByTime(ANCHOR_TIMEOUT_MS);

      expect(await pending).toEqual({
        ok: false,
        reason: AnchorFetchErrorType.Timeout,
      });
      expect(mockRequest).not.toHaveBeenCalled();
    } finally {
      jest.useRealTimers();
    }
  });
});
