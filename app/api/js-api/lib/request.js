// @flow
import _ from 'lodash';
import blakejs from 'blakejs';
import https from 'https';
import querystring from 'querystring';

export type RequestOptions = {
  hostname: string,
  method: string,
  path: string,
  port: number,
  ca: string,
  headers?: {
    'Content-Type': string,
    'Content-Length': number,
  },
};

const bytesToB16 = (bytes) => Buffer.from(bytes).toString('hex');
const blake2b = (data) => blakejs.blake2b(data, null, 32);
const encryptPasspharse = (passphrase) => bytesToB16(blake2b(passphrase));

export const request = (httpOptions: RequestOptions, queryParams?: {}) => {
  return new Promise((resolve, reject) => {
    // Prepare request with http options and (optional) query params
    const options: RequestOptions = Object.assign({}, httpOptions);
    let hasRequestBody = false;
    let requestBody = '';
    if (queryParams) {
      // Handle passphrase
      if (_.has(queryParams, 'passphrase')) {
        const passpharse = _.get(queryParams, 'passphrase');

        // If passphrase is present it must be encrypted and included in options.path
        if (passpharse !== null) {
          const encryptedPassphrase = encryptPasspharse(passpharse);
          options.path += `?passphrase=${encryptedPassphrase}`;
        }

        // Passphrase is never sent as a part of requestBody so we need to omit it
        queryParams = _.omit(queryParams, 'passphrase');
      }

      // Handle other query params
      if (_.size(queryParams) > 0) {
        hasRequestBody = true;
        requestBody = querystring.stringify(queryParams);
        options.headers = {
          'Content-Type': 'application/x-www-form-urlencoded',
          'Content-Length': requestBody.length,
        };
      }
    }
    const request = https.request(options);
    if (hasRequestBody) { request.write(requestBody); }
    request.on('response', (response) => {
      let body = '';
      // Cardano-sl returns chunked requests, so we need to concat them
      response.on('data', (chunk) => body += chunk);
      // Reject errors
      response.on('error', (error) => reject(error));
      // Resolve JSON results and handle weird backend behavior
      // of "Left" (for errors) and "Right" (for success) properties
      response.on('end', () => {
        const parsedBody = JSON.parse(body);
        if (parsedBody.Right) {
          // "Right" means 200 ok (success)
          resolve(parsedBody.Right);
        } else if (parsedBody.Left) {
          // "Left" means error case -> return error with contents
          reject(new Error(parsedBody.Left.contents));
        } else {
          // TODO: investigate if that can happen! (no Right or Left in a response)
          reject(new Error('Unknown response from backend.'));
        }
      });
    });
    request.on('error', (error) => reject(error));
    request.end();
  });
};
