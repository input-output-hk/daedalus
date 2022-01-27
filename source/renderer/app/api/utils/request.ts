import { includes, omit, size, values, flatten } from 'lodash';
import JSONBigInt from 'json-bigint';
import querystring from 'querystring';
import { getContentLength } from '.';

export type RequestOptions = {
  hostname: string;
  method: string;
  path: string;
  port: number;
  ca: Uint8Array;
  cert: Uint8Array;
  key: Uint8Array;
  headers?: {
    'Content-Type': string;
    'Content-Length': number;
  };
};
const ALLOWED_ERROR_EXCEPTION_PATHS = [];
const { isSelfnode } = global.environment;
const agent = new global.https.Agent({
  maxCachedSessions: 256,
  // Default: 100 | 0 - Disable TLS session caching
  maxFreeSockets: 256,
  // Default: 256
  maxSockets: 256,
  // Default: Infinity
  maxTotalSockets: 256,
  // Default: Infinity
  keepAlive: true,
  // Default: false
  keepAliveMsecs: 1000,
  // Default: 1000 | unit: milliseconds
  scheduling: 'lifo', // Default: 'lifo'
  // timeout: 5 * 1000, // 5 seconds | unit: milliseconds
});
// Passing ciphers, minVersion, and maxVersion speeds up TLS handshake
const httpsOptions = {
  ciphers: [
    'TLS_AES_256_GCM_SHA384', // 'TLS_AES_128_GCM_SHA256',
    // 'TLS_AES_128_CCM_SHA256',
    // 'TLS_AES_128_CCM_8_SHA256',
    // 'TLS_CHACHA20_POLY1305_SHA256'
  ].join(' '),
  minVersion: 'TLSv1.3',
  maxVersion: 'TLSv1.3',
  agent,
};

const logSocketStats = (state: string, { sockets, freeSockets }) => {
  // @ts-ignore ts-migrate(2339) FIXME: Property 'logSocketStats' does not exist on type '... Remove this comment to see the full error message
  if (!window.logSocketStats) {
    return;
  }

  const used = flatten(values(sockets)).length;
  const free = flatten(values(freeSockets)).length;
  const total = used + free;
  // eslint-disable-next-line no-console
  console.debug(`[connection:${state}]:socket-stats`, {
    used,
    free,
    total,
  });
};

function typedRequest<Response>(
  httpOptions: RequestOptions,
  queryParams?: {},
  rawBodyParams?: any,
  requestOptions?: {
    returnMeta?: boolean;
    isOctetStreamRequest?: boolean;
    isOctetStreamResponse?: boolean;
  }
): Promise<Response> {
  return new Promise((resolve, reject) => {
    const options: RequestOptions = Object.assign(
      {},
      httpsOptions,
      httpOptions
    );
    const { isOctetStreamRequest, isOctetStreamResponse } = Object.assign(
      {},
      requestOptions
    );
    let hasRequestBody = false;
    let requestBody = '';

    if (queryParams && size(queryParams) > 0) {
      options.path += `?${querystring.stringify(queryParams)}`;
    }

    // Handle raw body params
    if (rawBodyParams) {
      hasRequestBody = true;

      if (isOctetStreamRequest) {
        requestBody = rawBodyParams;
        options.headers = {
          'Content-Length': requestBody.length / 2,
          'Content-Type': 'application/octet-stream',
        };
      } else {
        requestBody = JSONBigInt.stringify(rawBodyParams);
        options.headers = {
          'Content-Length': getContentLength(requestBody),
          'Content-Type': 'application/json; charset=utf-8',
        };
      }

      options.headers = {
        ...options.headers,
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ Accept: string; 'Content-Type': string; 'C... Remove this comment to see the full error message
        Accept: isOctetStreamResponse
          ? 'application/octet-stream'
          : 'application/json; charset=utf-8',
      };
    }

    const httpOnlyOptions = omit(options, [
      'agent',
      'ca',
      'cert',
      'key',
      'ciphers',
      'minVersion',
      'maxVersion',
    ]);
    const httpsRequest = isSelfnode
      ? global.http.request(httpOnlyOptions)
      : global.https.request(options);

    if (hasRequestBody) {
      if (isOctetStreamRequest) {
        httpsRequest.write(requestBody, 'hex');
      } else {
        httpsRequest.write(requestBody);
      }
    }

    httpsRequest.on('socket', () => {
      logSocketStats('socket', httpsRequest.agent);
    });
    httpsRequest.on('finish', () => {
      logSocketStats('finish', httpsRequest.agent);
    });
    httpsRequest.on('response', (response) => {
      logSocketStats('response', httpsRequest.agent);
      let body = '';
      let stream;
      // cardano-wallet returns chunked requests, so we need to concat them
      response.on('data', (chunk) => {
        if (isOctetStreamResponse) {
          stream = chunk;
        } else {
          body += chunk;
        }
      });
      // Reject errors
      response.on('error', (error) => reject(error));
      // Resolve JSON results and handle backend errors
      response.on('end', () => {
        try {
          const { statusCode, statusMessage } = response;
          const isSuccessResponse =
            (statusCode >= 200 && statusCode <= 206) ||
            (statusCode === 404 &&
              includes(ALLOWED_ERROR_EXCEPTION_PATHS, options.path));

          if (isSuccessResponse) {
            if (isOctetStreamResponse) {
              resolve(stream);
            } else {
              const data =
                statusCode === 404
                  ? 'null'
                  : `"statusCode: ${statusCode} -- statusMessage: ${statusMessage}"`;

              // When deleting a wallet, the API does not return any data in body
              // even if it was successful
              if (!body) {
                body = `{
                  "status": ${statusCode},
                  "data": ${data}
                }`;
              }

              resolve(JSONBigInt.parse(body));
            }
          } else if (stream) {
            // Error response with a stream
            const parsedStream = JSONBigInt.parse(stream.toString());
            reject(parsedStream);
          } else if (body) {
            // Error response with a body
            const parsedBody = JSONBigInt.parse(body);

            if (parsedBody.code && parsedBody.message) {
              reject(parsedBody);
            } else {
              reject(new Error('Unknown API response'));
            }
          } else {
            // Error response without a stream or body
            reject(new Error('Unknown API response'));
          }
        } catch (error) {
          // Handle internal server errors (e.g. HTTP 500 - 'Something went wrong')
          reject(new Error(error));
        }
      });
    });
    httpsRequest.on('timeout', () => {
      logSocketStats('timeout', httpsRequest.agent);
    });
    httpsRequest.on('close', () => {
      logSocketStats('close', httpsRequest.agent);
    });
    httpsRequest.on('error', (error) => reject(error));
    httpsRequest.end();
    logSocketStats('init', httpsRequest.agent);
  });
}

export const request = typedRequest;
