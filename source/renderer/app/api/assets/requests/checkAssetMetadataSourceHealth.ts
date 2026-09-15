import { ASSET_METADATA_SOURCE_PROBE_TIMEOUT_MS } from '../../../config/assetsConfig';
import type { AssetMetadataSourceTip } from '../types';

/**
 * The largest `/tip` response worth reading. A Koios tip is a one-element array
 * of six short fields, well under 512 bytes; anything larger is not one, and a
 * probe must not read an unbounded body from a URL the user has just typed.
 */
const MAX_RESPONSE_BYTES = 64 * 1024;

/**
 * A candidate source's current tip.
 *
 * This does not go through `api/utils/request.ts`, and the reason is not style.
 * That module reads `isSelfnode` once at load and sends the whole request over
 * plain HTTP when it is true, and it merges in an agent carrying cardano-wallet's
 * client certificate. Neither is acceptable against a third-party host the user
 * has just named. `ASSET_METADATA_URL_VALIDATOR` has already required `https://`,
 * so the scheme here is not a choice.
 *
 * It also does not accept a bare `200`. A URL that answers everything with a
 * page is not an instance, and the shape of the body is the only thing that
 * distinguishes the two.
 */
export const checkAssetMetadataSourceHealth = (
  url: string,
  timeoutMs: number = ASSET_METADATA_SOURCE_PROBE_TIMEOUT_MS
): Promise<AssetMetadataSourceTip> =>
  new Promise((resolve, reject) => {
    let parsed: URL;
    try {
      parsed = new URL(`${url.replace(/\/+$/, '')}/tip`);
    } catch {
      reject(new Error('asset metadata source URL could not be parsed'));
      return;
    }

    let settled = false;
    let request = null;
    const settle = (error: Error | null, tip?: AssetMetadataSourceTip) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      if (request) request.destroy();
      if (error) reject(error);
      else resolve(tip);
    };
    const timer = setTimeout(
      () => settle(new Error('asset metadata source did not answer in time')),
      timeoutMs
    );

    request = global.https.request(
      {
        hostname: parsed.hostname,
        port: parsed.port || 443,
        path: parsed.pathname,
        method: 'GET',
        headers: {
          accept: 'application/json',
        },
      },
      (response) => {
        let body = '';
        let received = 0;
        response.on('data', (chunk) => {
          received += chunk.length;
          if (received > MAX_RESPONSE_BYTES) {
            response.destroy();
            settle(new Error('asset metadata source answered with too much'));
            return;
          }
          body += chunk;
        });
        response.on('error', () =>
          settle(new Error('asset metadata source connection failed'))
        );
        response.on('end', () => {
          const status = response.statusCode;
          if (status < 200 || status > 299) {
            settle(
              new Error(`asset metadata source answered with ${status ?? 0}`)
            );
            return;
          }
          const tip = parseTip(body);
          if (!tip) {
            settle(new Error('asset metadata source did not answer a tip'));
            return;
          }
          settle(null, tip);
        });
      }
    );
    request.on('error', () =>
      settle(new Error('asset metadata source connection failed'))
    );
    request.end();
  });

/**
 * Koios answers `/tip` with a one-element array. Only `abs_slot` is read, and
 * it is read as a finite number rather than coerced, so a string in that
 * position is a body that is not a tip rather than a slot of `NaN`.
 */
const parseTip = (body: string): AssetMetadataSourceTip | null => {
  let payload: unknown;
  try {
    payload = JSON.parse(body);
  } catch {
    return null;
  }
  if (!Array.isArray(payload) || payload.length === 0) return null;
  const first = payload[0];
  if (!first || typeof first !== 'object') return null;
  const absoluteSlot = (first as { abs_slot?: unknown }).abs_slot;
  if (typeof absoluteSlot !== 'number' || !Number.isFinite(absoluteSlot)) {
    return null;
  }
  return { absoluteSlot };
};
