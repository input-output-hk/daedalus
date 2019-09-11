// @flow
import { size, has, get, omit, includes } from 'lodash';
import querystring from 'querystring';
import { encryptPassphrase, getContentLength } from '.';

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

const ALLOWED_ERROR_EXCEPTION_PATHS = [
  '/api/internal/next-update', // when nextAdaUpdate receives a 404, it isn't an error
];

function typedRequest<Response>(
  httpOptions: RequestOptions,
  queryParams?: {},
  rawBodyParams?: any
  // requestOptions?: { returnMeta: boolean }
): Promise<Response> {
  return new Promise((resolve, reject) => {
    const allowedErrorExceptionPaths = [
      '/api/internal/next-update', // when nextAdaUpdate receives a 404, it isn't an error
    ];
    const options: RequestOptions = Object.assign({}, httpOptions);
    // const { returnMeta } = Object.assign({}, requestOptions);
    let hasRequestBody = false;
    let requestBody = '';

    let queryString = '';
    if (queryParams && size(queryParams) > 0) {
      // Handle passphrase
      if (has(queryParams, 'passphrase')) {
        const passphrase = get(queryParams, 'passphrase');

        // If passphrase is present it must be encrypted and included in options.path
        if (passphrase) {
          const encryptedPassphrase = encryptPassphrase(passphrase);
          queryString = `?passphrase=${encryptedPassphrase}`;
        }

        // Passphrase must be ommited from rest query params
        queryParams = omit(queryParams, 'passphrase');

        if (size(queryParams > 1) && passphrase) {
          queryString += `&${querystring.stringify(queryParams)}`;
        }
      } else {
        queryString = `?${querystring.stringify(queryParams)}`;
      }

      if (queryString) options.path += queryString;
    }

    // Handle raw body params
    if (rawBodyParams) {
      hasRequestBody = true;
      requestBody = JSON.stringify(rawBodyParams);
      options.headers = {
        'Content-Length': getContentLength(requestBody),
        'Content-Type': 'application/json; charset=utf-8',
        Accept: 'application/json; charset=utf-8',
      };
    }

    // @API TODO:  Delete once HTTPS is supported by the new API
    const httpOnlyOptions = omit(options, ['ca', 'cert', 'key']);

    // @API TODO: Uncomment / switch once HTTPS is supported by the new API
    // const httpsRequest = global.https.request(options);
    const httpsRequest = global.http.request(httpOnlyOptions);

    if (hasRequestBody) {
      httpsRequest.write(requestBody);
    }
    httpsRequest.on('response', response => {
      let body = '';
      // Cardano-sl returns chunked requests, so we need to concat them
      response.on('data', chunk => {
        body += chunk;
      });
      // Reject errors
      response.on('error', error => reject(error));
      // Resolve JSON results and handle backend errors
      response.on('end', () => {
        try {
          const { statusCode, statusMessage } = response;
          const isSuccessResponse =
            (statusCode >= 200 && statusCode <= 206) ||
            (statusCode === 404 &&
              includes(ALLOWED_ERROR_EXCEPTION_PATHS, options.path));

          if (isSuccessResponse) {
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
            resolve(JSON.parse(body));
          } else if (body) {
            // Error response with a body
            const parsedBody = JSON.parse(body);
            if (parsedBody.code && parsedBody.message) {
              reject(parsedBody);
            } else {
              reject(new Error('Unknown API response'));
            }
          } else {
            // Error response without a body
            reject(new Error('Unknown API response'));
          }
        } catch (error) {
          // Handle internal server errors (e.g. HTTP 500 - 'Something went wrong')
          reject(new Error(error));
        }
      });
    });
    httpsRequest.on('error', error => reject(error));
    httpsRequest.end();
  });
}

export const request = typedRequest;
