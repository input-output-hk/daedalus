import https from 'https';
import dns from 'dns';
import net from 'net';
import type { ClientRequest, IncomingMessage } from 'http';
import { AnchorFetchErrorType } from '../../common/types/governance.types';
import { logger } from '../utils/logging';

export const ANCHOR_MAX_BYTES = 1024 * 1024;
export const ANCHOR_TIMEOUT_MS = 10000;
export const ANCHOR_ALLOWED_CONTENT_TYPES = [
  'application/json',
  'application/ld+json',
];

export interface AnchorFetchOk {
  ok: true;
  bytes: Buffer;
  host: string;
  contentType: string;
  byteLength: number;
}

export interface AnchorFetchFail {
  ok: false;
  reason: AnchorFetchErrorType;
}

export type AnchorFetchResult = AnchorFetchOk | AnchorFetchFail;

export interface AnchorTransport {
  readonly scheme: string;
  fetch(url: string): Promise<AnchorFetchResult>;
}

const TLS_ERROR_CODES = [
  'CERT_HAS_EXPIRED',
  'DEPTH_ZERO_SELF_SIGNED_CERT',
  'SELF_SIGNED_CERT_IN_CHAIN',
  'UNABLE_TO_GET_ISSUER_CERT_LOCALLY',
  'UNABLE_TO_VERIFY_LEAF_SIGNATURE',
  'ERR_TLS_CERT_ALTNAME_INVALID',
  'ERR_TLS_HANDSHAKE_TIMEOUT',
  'ERR_SSL_WRONG_VERSION_NUMBER',
];

const DNS_ERROR_CODES = ['ENOTFOUND', 'EAI_AGAIN'];

const BLOCKED_IPV4_PREFIXES: Array<[string, number]> = [
  ['0.0.0.0', 8],
  ['10.0.0.0', 8],
  ['100.64.0.0', 10],
  ['127.0.0.0', 8],
  ['169.254.0.0', 16],
  ['172.16.0.0', 12],
  ['192.0.0.0', 24],
  ['192.168.0.0', 16],
  ['198.18.0.0', 15],
  ['224.0.0.0', 4],
  ['240.0.0.0', 4],
];

// 2002::/16 and 2001::/32 can encapsulate an arbitrary IPv4 destination, so
// they are blocked alongside the ranges that are reserved outright.
const BLOCKED_IPV6_PREFIXES: Array<[string, number]> = [
  ['::', 128],
  ['::1', 128],
  ['64:ff9b::', 96],
  ['100::', 64],
  ['2001::', 32],
  ['2001:db8::', 32],
  ['2002::', 16],
  ['fc00::', 7],
  ['fe80::', 10],
  ['ff00::', 8],
];

function ipv4ToBytes(input: string): Uint8Array | null {
  if (!net.isIPv4(input)) return null;
  return Uint8Array.from(input.split('.').map((part) => Number(part)));
}

function ipv6ToBytes(input: string): Uint8Array | null {
  const address = input.split('%')[0];
  if (!net.isIPv6(address)) return null;
  let text = address;
  if (text.lastIndexOf('.') !== -1) {
    const colon = text.lastIndexOf(':');
    const embedded = ipv4ToBytes(text.slice(colon + 1));
    if (!embedded) return null;
    text = `${text.slice(0, colon + 1)}${(
      (embedded[0] << 8) |
      embedded[1]
    ).toString(16)}:${((embedded[2] << 8) | embedded[3]).toString(16)}`;
  }
  const [left, right = ''] = text.split('::');
  const leftGroups = left === '' ? [] : left.split(':');
  const rightGroups = right === '' ? [] : right.split(':');
  const groups = text.includes('::')
    ? [
        ...leftGroups,
        ...new Array(8 - leftGroups.length - rightGroups.length).fill('0'),
        ...rightGroups,
      ]
    : leftGroups;
  if (groups.length !== 8) return null;
  const bytes = new Uint8Array(16);
  for (let index = 0; index < 8; index += 1) {
    const value = parseInt(groups[index], 16);
    if (Number.isNaN(value)) return null;
    bytes[index * 2] = value >> 8;
    bytes[index * 2 + 1] = value & 0xff;
  }
  return bytes;
}

function isInPrefix(
  bytes: Uint8Array,
  prefix: Uint8Array,
  prefixLength: number
): boolean {
  const fullBytes = Math.floor(prefixLength / 8);
  for (let index = 0; index < fullBytes; index += 1) {
    if (bytes[index] !== prefix[index]) return false;
  }
  const remainingBits = prefixLength % 8;
  if (remainingBits === 0) return true;
  const mask = (0xff << (8 - remainingBits)) & 0xff;
  return (bytes[fullBytes] & mask) === (prefix[fullBytes] & mask);
}

// Anything that is not a parseable public address is blocked, so a new address
// form can never default to allowed.
export function isBlockedAnchorAddress(address: string): boolean {
  if (net.isIPv4(address)) {
    const bytes = ipv4ToBytes(address);
    if (!bytes) return true;
    return BLOCKED_IPV4_PREFIXES.some(([prefix, length]) => {
      const prefixBytes = ipv4ToBytes(prefix);
      return prefixBytes != null && isInPrefix(bytes, prefixBytes, length);
    });
  }
  if (net.isIPv6(address)) {
    const bytes = ipv6ToBytes(address);
    if (!bytes) return true;
    const mapped = ipv6ToBytes('::ffff:0:0');
    if (mapped != null && isInPrefix(bytes, mapped, 96)) {
      return isBlockedAnchorAddress(
        `${bytes[12]}.${bytes[13]}.${bytes[14]}.${bytes[15]}`
      );
    }
    return BLOCKED_IPV6_PREFIXES.some(([prefix, length]) => {
      const prefixBytes = ipv6ToBytes(prefix);
      return prefixBytes != null && isInPrefix(bytes, prefixBytes, length);
    });
  }
  return true;
}

function fail(reason: AnchorFetchErrorType): AnchorFetchFail {
  logger.warn('Anchor fetch: request rejected', { errorType: reason });
  return { ok: false, reason };
}

function classifyTransportError(error: unknown): AnchorFetchErrorType {
  const code = (error as { code?: string })?.code ?? '';
  if (TLS_ERROR_CODES.includes(code)) return AnchorFetchErrorType.TlsFailed;
  if (DNS_ERROR_CODES.includes(code)) return AnchorFetchErrorType.DnsFailed;
  if (code === 'ETIMEDOUT') return AnchorFetchErrorType.Timeout;
  return AnchorFetchErrorType.Network;
}

function requestAnchorBytes(
  parsed: URL,
  host: string,
  pinned: dns.LookupAddress,
  budgetMs: number
): Promise<AnchorFetchResult> {
  return new Promise((resolve) => {
    const chunks: Buffer[] = [];
    let received = 0;
    let settled = false;
    let request: ClientRequest | null = null;
    let totalTimer: ReturnType<typeof setTimeout>;

    const rejectOnce = (reason: AnchorFetchErrorType) => {
      if (settled) return;
      settled = true;
      clearTimeout(totalTimer);
      if (request) request.destroy();
      resolve(fail(reason));
    };

    const resolveOnce = (result: AnchorFetchOk) => {
      if (settled) return;
      settled = true;
      clearTimeout(totalTimer);
      logger.info('Anchor fetch: anchor bytes retrieved', {
        byteLength: result.byteLength,
      });
      resolve(result);
    };

    // budgetMs is what is left of the one wall-clock budget after DNS, so the
    // ten seconds cover resolution and transfer together, not each in turn.
    if (budgetMs <= 0) {
      resolve(fail(AnchorFetchErrorType.Timeout));
      return;
    }

    totalTimer = setTimeout(
      () => rejectOnce(AnchorFetchErrorType.Timeout),
      budgetMs
    );

    // The socket is forced onto the address the guard validated, so a second
    // DNS answer cannot redirect the connection after the check.
    const lookup: net.LookupFunction = (_hostname, _options, callback) =>
      callback(null, pinned.address, pinned.family);

    const options: https.RequestOptions = {
      protocol: 'https:',
      hostname: host,
      port: parsed.port || 443,
      path: `${parsed.pathname}${parsed.search}`,
      method: 'GET',
      headers: { accept: ANCHOR_ALLOWED_CONTENT_TYPES.join(', ') },
      lookup,
      timeout: budgetMs,
      ...(net.isIP(host) === 0 ? { servername: host } : {}),
    };

    request = https.request(options, (response: IncomingMessage) => {
      const statusCode = response.statusCode ?? 0;
      if (statusCode >= 300 && statusCode < 400) {
        response.destroy();
        rejectOnce(AnchorFetchErrorType.Redirected);
        return;
      }
      if (statusCode < 200 || statusCode >= 300) {
        response.destroy();
        rejectOnce(AnchorFetchErrorType.HttpStatus);
        return;
      }

      const contentType = String(response.headers['content-type'] ?? '')
        .split(';')[0]
        .trim()
        .toLowerCase();
      if (!ANCHOR_ALLOWED_CONTENT_TYPES.includes(contentType)) {
        response.destroy();
        rejectOnce(AnchorFetchErrorType.ContentType);
        return;
      }

      const declaredLength = Number(response.headers['content-length']);
      if (
        Number.isFinite(declaredLength) &&
        declaredLength > ANCHOR_MAX_BYTES
      ) {
        response.destroy();
        rejectOnce(AnchorFetchErrorType.TooLarge);
        return;
      }

      response.on('data', (chunk: Buffer) => {
        received += chunk.length;
        if (received > ANCHOR_MAX_BYTES) {
          response.destroy();
          rejectOnce(AnchorFetchErrorType.TooLarge);
          return;
        }
        chunks.push(Buffer.from(chunk));
      });
      response.on('error', (error) =>
        rejectOnce(classifyTransportError(error))
      );
      response.on('end', () => {
        const bytes = Buffer.concat(chunks, received);
        resolveOnce({
          ok: true,
          bytes,
          host,
          contentType,
          byteLength: bytes.length,
        });
      });
    });

    request.on('timeout', () => rejectOnce(AnchorFetchErrorType.Timeout));
    request.on('error', (error) => rejectOnce(classifyTransportError(error)));
    request.end();
  });
}

const TIMEOUT_SENTINEL = Symbol('anchor-timeout');

// dns.promises.lookup has no timeout of its own, so the wall-clock budget is
// armed here and the resolver races against it; what is left funds the request.
function lookupWithinBudget(
  host: string,
  budgetMs: number
): Promise<dns.LookupAddress[] | typeof TIMEOUT_SENTINEL> {
  let timer: ReturnType<typeof setTimeout>;
  const expiry = new Promise<typeof TIMEOUT_SENTINEL>((resolve) => {
    timer = setTimeout(() => resolve(TIMEOUT_SENTINEL), budgetMs);
  });
  return Promise.race([
    dns.promises.lookup(host, { all: true }),
    expiry,
  ]).finally(() => clearTimeout(timer));
}

async function fetchOverHttps(url: string): Promise<AnchorFetchResult> {
  const deadline = Date.now() + ANCHOR_TIMEOUT_MS;

  let parsed: URL;
  try {
    parsed = new URL(url);
  } catch (error) {
    return fail(AnchorFetchErrorType.InvalidRequest);
  }
  if (parsed.protocol !== 'https:') {
    return fail(AnchorFetchErrorType.UnsupportedScheme);
  }
  const host = parsed.hostname.replace(/^\[|\]$/g, '');
  if (host === '') return fail(AnchorFetchErrorType.InvalidRequest);

  let addresses: dns.LookupAddress[];
  if (net.isIP(host) !== 0) {
    addresses = [{ address: host, family: net.isIP(host) }];
  } else {
    try {
      const resolved = await lookupWithinBudget(host, ANCHOR_TIMEOUT_MS);
      if (resolved === TIMEOUT_SENTINEL) {
        return fail(AnchorFetchErrorType.Timeout);
      }
      addresses = resolved;
    } catch (error) {
      return fail(AnchorFetchErrorType.DnsFailed);
    }
  }
  if (addresses.length === 0) return fail(AnchorFetchErrorType.DnsFailed);
  // One blocked answer fails the whole fetch: a split resolution is an attack,
  // not a fallback.
  if (addresses.some((entry) => isBlockedAnchorAddress(entry.address))) {
    return fail(AnchorFetchErrorType.BlockedAddress);
  }

  try {
    return await requestAnchorBytes(
      parsed,
      host,
      addresses[0],
      deadline - Date.now()
    );
  } catch (error) {
    return fail(classifyTransportError(error));
  }
}

export const httpsAnchorTransport: AnchorTransport = {
  scheme: 'https:',
  fetch: fetchOverHttps,
};

// The IPFS slot is this interface. No ipfs: entry is registered, so an ipfs URL
// resolves to UnsupportedScheme rather than a partial implementation.
const TRANSPORTS: Record<string, AnchorTransport> = {
  [httpsAnchorTransport.scheme]: httpsAnchorTransport,
};

export async function fetchAnchorBytes(
  url: string
): Promise<AnchorFetchResult> {
  let scheme: string;
  try {
    scheme = new URL(url).protocol;
  } catch (error) {
    return fail(AnchorFetchErrorType.InvalidRequest);
  }
  const transport = TRANSPORTS[scheme];
  if (!transport) return fail(AnchorFetchErrorType.UnsupportedScheme);
  return transport.fetch(url);
}
