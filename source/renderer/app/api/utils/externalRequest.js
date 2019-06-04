// @flow
import { omit, map } from 'lodash';
import { ALLOWED_EXTERNAL_SOURCES } from '../../config/urlsConfig';

export type HttpOptions = {
  hostname: string,
  method: string,
  path?: string,
  port?: number,
  protocol?: string,
  headers?: {
    'Content-Type': string,
    'Content-Length': number,
  },
};

export const externalRequest = (httpOptions: HttpOptions): Promise<any> => (
  new Promise((resolve, reject) => {
    let isAllowed = false;
    map(ALLOWED_EXTERNAL_SOURCES, (allowedSource) => {
      const allowedHostname = allowedSource.split('/')[2];
      if (allowedHostname === httpOptions.hostname) {
        isAllowed = true;
      }
    });

    if (!isAllowed) {
      return reject(new Error('Hostname not allowed'));
    }

    const { protocol = 'https' } = httpOptions;
    const options = omit(httpOptions, 'protocol');
    const requestMethod = global[protocol].request;
    const request = requestMethod(options);

    request.on('response', response => {
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
    request.on('error', error => reject(error));
    return request.end();
  })
);
