// @flow
import https from 'https';
import { size, has, get, omit } from 'lodash';
import querystring from 'querystring';
import { encryptPassphrase } from '../encryptPassphrase';
import { getContentLength } from '../../../lib/utils';

export type RequestOptions = {
  hostname: string,
  method: string,
  path: string,
  port: number,
  ca: string,
  cert: string,
  key: string,
  headers?: {
    'Content-Type': string,
    'Content-Length': number,
  },
};

function typedRequest<Response>(
  httpOptions: RequestOptions, queryParams?: {}, rawBodyParams?: any
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

    const httpsRequest = https.request(options);
    if (hasRequestBody) {
      httpsRequest.write(requestBody);
    }
    httpsRequest.on('response', (response) => {
      let body = '';
      // Cardano-sl returns chunked requests, so we need to concat them
      response.on('data', (chunk) => (body += chunk));
      // Reject errors
      response.on('error', (error) => reject(error));
      // Resolve JSON results and handle backend errors
      response.on('end', () => {
        try {
          const parsedBody = JSON.parse(body);
          const status = get(parsedBody, 'status', false);
          if (status) {
            if (status === 'success') {
              resolve(parsedBody.data);
            } else if (status === 'error' || status === 'fail') {
              reject(new Error(parsedBody.message));
            } else {
              // TODO: find a way to record this case and report to the backend team
              reject(new Error('Unknown response from backend.'));
            }
          } else {
            // TODO: find a way to record this case and report to the backend team
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
