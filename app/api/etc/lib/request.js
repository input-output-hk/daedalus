// @flow
import http from 'http';
import querystring from 'querystring';

export type RequestOptions = {
  hostname: string,
  method: string,
  path: string,
  port: number,
  headers?: {
    'Content-Type': string,
    'Content-Length': number,
  },
};

export const request = (httpOptions: RequestOptions, queryParams?: {}) => (
  new Promise((resolve, reject) => {
    // Prepare request with http options and (optional) query params
    const options: RequestOptions = Object.assign({}, httpOptions);
    let requestBody = '';
    if (queryParams) requestBody = JSON.stringify(queryParams);
    options.headers = Object.assign(options.headers || {}, {
      'Content-Type': 'application/json',
      'Content-Length': requestBody.length,
    });
    const httpsRequest = http.request(options, (response) => {
      let body = '';
      // Cardano-sl returns chunked requests, so we need to concat them
      response.on('data', (chunk) => (body += chunk));
      // Reject errors
      response.on('error', (error) => reject(error));
      // Resolve JSON results and handle weird backend behavior
      // of "Left" (for errors) and "Right" (for success) properties
      response.on('end', () => { resolve(JSON.parse(body)) });
    });
    httpsRequest.on('error', (error) => reject(error));
    if (queryParams) { httpsRequest.write(requestBody); }
    httpsRequest.end();
  })
);
