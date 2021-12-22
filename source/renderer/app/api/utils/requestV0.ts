import https from 'https';
import { size, has, get, omit } from 'lodash';
import querystring from 'querystring';
import { encryptPassphrase, getContentLength } from '.';

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

function typedRequest<Response>(
  httpOptions: RequestOptions,
  queryParams?: {},
  rawBodyParams?: any
): Promise<Response> {
  return new Promise((resolve, reject) => {
    const options: RequestOptions = Object.assign({}, httpOptions);
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

        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'boolean' is not assignable to pa... Remove this comment to see the full error message
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
        'Content-Type': 'application/json',
      };
    }

    // @ts-ignore
    const httpsRequest = https.request(options);

    if (hasRequestBody) {
      httpsRequest.write(requestBody);
    }

    httpsRequest.on('response', (response) => {
      let body = '';
      // Cardano-sl returns chunked requests, so we need to concat them
      response.on('data', (chunk) => {
        body += chunk;
      });
      // Reject errors
      response.on('error', (error) => reject(error));
      // Resolve JSON results and handle weird backend behavior
      // of "Left" (for errors) and "Right" (for success) properties
      response.on('end', () => {
        try {
          const parsedBody = JSON.parse(body);

          if (has(parsedBody, 'Right')) {
            // "Right" means 200 ok (success) -> also handle if Right: false (boolean response)
            resolve(parsedBody.Right);
          } else if (has(parsedBody, 'Left')) {
            // "Left" means error case -> return error with contents (exception on nextUpdate)
            if (parsedBody.Left.contents) {
              reject(new Error(parsedBody.Left.contents));
            } else {
              reject(new Error('Unknown response from backend.'));
            }
          } else {
            // TODO: investigate if that can happen! (no Right or Left in a response)
            reject(new Error('Unknown response from backend.'));
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
