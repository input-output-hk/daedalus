// @flow
import { ALLOWED_EXTERNAL_HOSTNAMES } from '../../config/urlsConfig';

export type HttpOptions = {
  hostname: string,
  method: string,
  path?: string,
  port?: number,
  headers?: {
    'Content-Type': string,
    'Content-Length': number,
  },
};

export const externalRequest = (httpOptions: HttpOptions): Promise<any> => {
  return new Promise((resolve, reject) => {
    if (!ALLOWED_EXTERNAL_HOSTNAMES.includes(httpOptions.hostname)) {
      return reject(new Error('Hostname not allowed'));
    }

    const httpsRequest = global.https.request(httpOptions);

    httpsRequest.on('response', response => {
      let body = '';
      response.on('data', chunk => {
        body += chunk;
      });
      response.on('error', error => reject(error));
      response.on('end', () => {
        const parsedBody = JSON.parse(body);
        return resolve(parsedBody);
      });
    });
    httpsRequest.on('error', error => reject(error));
    return httpsRequest.end();
  });
};
