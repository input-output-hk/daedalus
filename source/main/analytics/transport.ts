import http from 'http';
import https from 'https';
import { setTimeout, clearTimeout } from 'timers';

export type TransportResult = { status: number; retryAfter?: number };
export type Transport = (
  endpoint: string,
  body: string,
  signal: AbortSignal
) => Promise<TransportResult>;

// Node HTTP has no redirect/cookie machinery. Discard provider bodies entirely.
export const postAnalytics: Transport = (endpoint, body, signal) =>
  new Promise((resolve) => {
    if (signal.aborted) {
      resolve({ status: 0 });
      return;
    }
    let finished = false;
    let timer: ReturnType<typeof setTimeout>;
    const finish = (result: TransportResult) => {
      if (finished) return;
      finished = true;
      clearTimeout(timer);
      signal.removeEventListener('abort', abort);
      resolve(result);
    };
    const request = (endpoint.startsWith('https:') ? https : http).request(
      endpoint,
      {
        method: 'POST',
        agent: false,
        maxHeaderSize: 8192,
        headers: {
          'content-type': 'application/json',
          'content-length': Buffer.byteLength(body),
        },
      },
      (response) => {
        const retry = response.headers['retry-after'];
        finish({
          status: response.statusCode || 0,
          retryAfter:
            typeof retry === 'string' && /^\d{1,5}$/.test(retry)
              ? Number(retry)
              : undefined,
        });
        response.destroy();
        request.destroy();
      }
    );
    const abort = () => {
      request.destroy();
      finish({ status: 0 });
    };
    request.on('error', () => finish({ status: 0 }));
    signal.addEventListener('abort', abort, { once: true });
    timer = setTimeout(abort, 5000);
    timer.unref();
    request.end(body);
  });
