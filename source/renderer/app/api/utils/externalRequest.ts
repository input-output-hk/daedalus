import { omit } from 'lodash';
import { ALLOWED_EXTERNAL_HOSTNAMES } from '../../config/urlsConfig';

export type HttpOptions = {
  hostname: string;
  method: string;
  path?: string;
  port?: number;
  protocol?: string;
  headers?: {
    'Content-Type': string;
    'Content-Length': number;
  };
};
export const externalRequest = (
  httpOptions: HttpOptions,
  raw = false
): Promise<any> =>
  new Promise((resolve, reject) => {
    if (!ALLOWED_EXTERNAL_HOSTNAMES.includes(httpOptions.hostname)) {
      return reject(new Error('Hostname not allowed'));
    }

    const { protocol = 'https' } = httpOptions;
    const options = omit(httpOptions, 'protocol');
    const requestMethod = global[protocol].request;
    const request = requestMethod(options);
    request.on('response', (response) => {
      response.setEncoding('utf8');
      let body = '';
      response.on('data', (chunk) => {
        body += chunk;
      });
      response.on('error', (error) => reject(error));
      response.on('end', () => {
        try {
          resolve(raw ? body : JSON.parse(body));
        } catch (error) {
          // Handle internal server errors (e.g. HTTP 500 - 'Something went wrong')
          reject(new Error(error));
        }
      });
    });
    request.on('error', (error) => reject(error));
    return request.end();
  });
