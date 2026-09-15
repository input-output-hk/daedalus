import http from 'http';
import https from 'https';
import type { ClientRequest, IncomingMessage } from 'http';

/**
 * One POST, one wall-clock budget, one response cap.
 *
 * This was `assetRegistryClient`'s private `post`. It moved here when a second
 * caller appeared, because the two have different response sizes to allow: a
 * registry answer is JSON for up to a hundred subjects, and a batch of raw
 * transactions is an order larger. The cap is therefore a parameter rather than
 * a constant, and everything else is unchanged.
 *
 * It never throws. Every failure is a result, because both callers treat being
 * offline as a state rather than as an error.
 */
export type HttpTransportResult =
  | { ok: true; status: number; body: string }
  | { ok: false; reason: 'timeout' | 'network' | 'too-large' };

export interface HttpTransport {
  post(
    url: string,
    body: string,
    timeoutMs: number,
    maxResponseBytes?: number
  ): Promise<HttpTransportResult>;
}

/** The registry's cap, and the default for a caller that does not name one. */
export const DEFAULT_MAX_RESPONSE_BYTES = 1024 * 1024;

const readResponse = (
  response: IncomingMessage,
  maxResponseBytes: number
): Promise<HttpTransportResult> =>
  new Promise((resolve) => {
    const declared = Number(response.headers['content-length']);
    if (Number.isFinite(declared) && declared > maxResponseBytes) {
      response.destroy();
      resolve({ ok: false, reason: 'too-large' });
      return;
    }
    const chunks: Array<Buffer> = [];
    let received = 0;
    response.on('data', (chunk: Buffer) => {
      received += chunk.length;
      if (received > maxResponseBytes) {
        response.destroy();
        resolve({ ok: false, reason: 'too-large' });
        return;
      }
      chunks.push(Buffer.from(chunk));
    });
    response.on('error', () => resolve({ ok: false, reason: 'network' }));
    response.on('end', () =>
      resolve({
        ok: true,
        status: response.statusCode ?? 0,
        body: Buffer.concat(chunks, received).toString('utf8'),
      })
    );
  });

const post = (
  url: string,
  body: string,
  timeoutMs: number,
  maxResponseBytes: number = DEFAULT_MAX_RESPONSE_BYTES
): Promise<HttpTransportResult> =>
  new Promise((resolve) => {
    let settled = false;
    let request: ClientRequest | null = null;
    const settle = (result: HttpTransportResult) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      if (request) request.destroy();
      resolve(result);
    };
    const timer = setTimeout(
      () => settle({ ok: false, reason: 'timeout' }),
      timeoutMs
    );

    let parsed: URL;
    try {
      parsed = new URL(url);
    } catch {
      settle({ ok: false, reason: 'network' });
      return;
    }
    // The selfnode mock serves plain HTTP on the loopback address, so the
    // scheme comes from the configured endpoint rather than being assumed.
    const agent = parsed.protocol === 'http:' ? http : https;

    request = agent.request(
      {
        protocol: parsed.protocol,
        hostname: parsed.hostname,
        port: parsed.port || (parsed.protocol === 'http:' ? 80 : 443),
        path: `${parsed.pathname}${parsed.search}`,
        method: 'POST',
        headers: {
          'content-type': 'application/json',
          accept: 'application/json',
          'content-length': Buffer.byteLength(body),
        },
        timeout: timeoutMs,
      },
      (response: IncomingMessage) => {
        readResponse(response, maxResponseBytes).then(settle, () =>
          settle({ ok: false, reason: 'network' })
        );
      }
    );
    request.on('timeout', () => settle({ ok: false, reason: 'timeout' }));
    request.on('error', () => settle({ ok: false, reason: 'network' }));
    request.end(body);
  });

export const httpTransport: HttpTransport = { post };
