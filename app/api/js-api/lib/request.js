//@flow
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
    let options: RequestOptions = Object.assign({}, httpOptions);
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
      request.on('error', (error) => reject(error));
      // Resolve JSON results and only return the "Right" property
      // which is always the root that is returned by the backend (no idea why)
      response.on('end', () => {
        const parsedBody = JSON.parse(body);
        if (parsedBody.Right) {
          resolve(parsedBody.Right);
        } else if(parsedBody.Left) {
          reject(new Error(parsedBody.Left.contents));
        } else {
          reject(new Error("Unknown response from backend."));
        }
      });
    });
    request.on('error', (error) => reject(error));
    request.end();
  });
};
