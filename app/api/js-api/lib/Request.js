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

export class Request<Params: Object | void, Response> {

  _options: RequestOptions;

  constructor(options: RequestOptions) {
    this._options = options;
  }

  send(params?: Params): Promise<Response> {
    return new Promise((resolve, reject) => {
      // Prepare request with http options and (optional) query params
      let options: RequestOptions = Object.assign({}, this._options);
      let requestBody = '';
      if (params) {
        requestBody = querystring.stringify(params);
        options.headers = {
          'Content-Type': 'application/x-www-form-urlencoded',
          'Content-Length': requestBody.length,
        };
      }
      const request = https.request(options);
      if (params) { request.write(requestBody); }
      request.on('response', (response) => {
        let body = '';
        // Cardano-sl returns chunked requests, so we need to concat them
        response.on('data', (chunk) => body += chunk);
        // Reject errors
        request.on('error', (e) => reject(e));
        // Resolve JSON results
        response.on('end', () => resolve(JSON.parse(body)));
      });
      request.end();
    });
  }
}
