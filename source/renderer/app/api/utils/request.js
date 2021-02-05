// @flow
import { includes, omit, size } from 'lodash';
import JSONBigInt from 'json-bigint';
import querystring from 'querystring';
import { getContentLength } from '.';

export type RequestOptions = {
  hostname: string,
  method: string,
  path: string,
  port: number,
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
  headers?: {
    'Content-Type': string,
    'Content-Length': number,
  },
};

const ALLOWED_ERROR_EXCEPTION_PATHS = [];

const { isIncentivizedTestnet, isSelfnode } = global.environment;

function typedRequest<Response>(
  httpOptions: RequestOptions,
  queryParams?: {},
  rawBodyParams?: any,
  requestOptions?: {
    returnMeta?: boolean,
    isOctetStreamRequest?: boolean,
    isOctetStreamResponse?: boolean,
  }
): Promise<Response> {
  return new Promise((resolve, reject) => {
    const options: RequestOptions = Object.assign({}, httpOptions);
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
        requestBody = JSON.stringify(rawBodyParams);
        options.headers = {
          'Content-Length': getContentLength(requestBody),
          'Content-Type': 'application/json; charset=utf-8',
        };
      }

      options.headers = {
        ...options.headers,
        Accept: isOctetStreamResponse
          ? 'application/octet-stream'
          : 'application/json; charset=utf-8',
      };
    }

    const httpOnlyOptions = omit(options, ['ca', 'cert', 'key']);
    const httpsRequest =
      isIncentivizedTestnet || isSelfnode
        ? global.http.request(httpOnlyOptions)
        : global.https.request(options);

    if (hasRequestBody) {
      if (isOctetStreamRequest) {
        httpsRequest.write(requestBody, 'hex');
      } else {
        httpsRequest.write(requestBody);
      }
    }

    httpsRequest.on('response', (response) => {
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
    httpsRequest.on('error', (error) => reject(error));
    httpsRequest.end();
  });
}

export const request = typedRequest;
