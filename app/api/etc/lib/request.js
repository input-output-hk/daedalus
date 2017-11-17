// @flow
import https from 'https';

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

function typedRequest<Response>(
  httpOptions: RequestOptions, queryParams?: {}
): Promise<Response> {
  return new Promise((resolve, reject) => {
    // Prepare request with http options and (optional) query params
    const options: RequestOptions = Object.assign({}, httpOptions);
    let requestBody = '';
    if (queryParams) requestBody = JSON.stringify(queryParams);
    options.headers = Object.assign(options.headers || {}, {
      'Content-Type': 'application/json',
      'Content-Length': requestBody.length,
    });
    const httpsRequest = https.request(options, (response) => {
      let body = '';
      // Cardano-sl returns chunked requests, so we need to concat them
      response.on('data', (chunk) => (body += chunk));
      // Reject errors
      response.on('error', (error) => reject(error));
      // Resolve JSON results and handle weird backend behavior
      response.on('end', () => {
        const parsedBody = JSON.parse(body);
        if (parsedBody.result != null) {
          resolve(parsedBody.result);
        } else if (parsedBody.error) {
          reject(new Error(parsedBody.error.message));
        } else {
          // TODO: investigate if that can happen! (no Right or Left in a response)
          reject(new Error('Unknown response from backend.'));
        }
      });
    });
    httpsRequest.on('error', (error) => reject(error));
    if (queryParams) {
      httpsRequest.write(requestBody);
    }
    httpsRequest.end();
  });
}

export const request = typedRequest;
