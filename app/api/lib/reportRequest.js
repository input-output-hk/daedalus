import http from 'http';
import FormData from 'form-data/lib/form_data';
import fs from 'fs';

export type RequestOptions = {
  hostname: string,
  method: string,
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
  httpOptions: RequestOptions, requestPayload?: RequestPayload
): Promise<Response> {
  return new Promise((resolve, reject) => {
    const options: RequestOptions = Object.assign({}, httpOptions);
    const payload: RequestPayload = Object.assign({}, requestPayload);
    // Prepare multipart/form-data
    const formData = new FormData();
    formData.append('payload', JSON.stringify(payload));

    // prepare file stream (attachment)
    if (payload.compressedLog) {
      const stream = fs.createReadStream(payload.compressedLog);
      formData.append('logs.zip', stream);
    }

    options.headers = formData.getHeaders();
    const httpRequest = http.request(options);

    // Attach form-data to the request
    formData.pipe(httpRequest);

    httpRequest.on('response', (response) => {
      if (response.statusCode !== 200) {
        return reject();
      }
      response.on('data', (chunk) => {
        console.debug('response.data', chunk);
      });
      response.on('error', (error) => {
        console.debug('response.error', error);
        reject(error);
      });
      response.on('end', () => {
        console.debug('response.end');
        resolve();
      });
    });
    httpRequest.on('error', (error) => reject(error));
  });
}

export const request = typedHttpRequest;
