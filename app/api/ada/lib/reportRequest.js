import http from 'http';
import querystring from 'querystring';

export type RequestOptions = {
  hostname: string,
  method: string,
  path: string,
  port: number,
  headers?: {
    'Content-Type': string,
  },
};

export type RequestPayload = {
  application: string,
  version: string,
  build: string,
  os: string,
  logs: Array<string>,
  date: string,
  magic: number,
  type: {
    type : string,
    email: string,
    subject: string,
    problem: string,
  }
};

function typedHttpRequest<Response>(
  httpOptions: RequestOptions, RequestPayload?: RequestPayload
): Promise<Response> {
  return new Promise((resolve, reject) => {
    const requestOptions: RequestOptions = Object.assign({}, httpOptions);
    const requestPayload: RequestPayload = Object.assign({}, RequestPayload);

    const stringifiedPayload = JSON.stringify(requestPayload);
    const payload = 'payload='+stringifiedPayload;

    const httpRequest = http.request(requestOptions);

    httpRequest.write(payload);

    httpRequest.on('response', (response) => {
      console.debug('response', response);
      response.on('response.data', (chunk) => {
        console.debug('data', chunk);
      });
      response.on('response.error', (error) => {
        console.debug('error', error);
        reject(error);
      });
      response.on('response.end', () => {
        console.debug('end');
      });
    });
    httpRequest.on('error', (error) => reject(error));
    httpRequest.end();
  });
}

export const request = typedHttpRequest;