// @flow
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

export const request = (httpOptions: RequestOptions, queryParams?: {}) => {
  return new Promise((resolve, reject) => {
    // Prepare request with http options and (optional) query params
    const options: RequestOptions = Object.assign({}, httpOptions);
    let requestBody = '';
    if (queryParams) {
      requestBody = querystring.stringify(queryParams);
      options.headers = {
        'Content-Type': 'application/x-www-form-urlencoded',
        'Content-Length': requestBody.length,
      };
    }
    const request = https.request(options);
    if (queryParams) { request.write(requestBody); }
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
