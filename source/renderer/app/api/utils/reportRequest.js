// @flow
import http from 'http';
import FormData from 'form-data/lib/form_data';
import fs from 'fs';
import { extractFileNameFromPath } from '../../../../common/fileName';

export type RequestOptions = {
  hostname: ?string,
  method: string,
  path: string,
  port: ?string,
  headers?: {
    'Content-Type': string,
  },
};

export type RequestPayload = {
  product: string,
  frontendVersion: string,
  backendVersion: string,
  network: string,
  build: string,
  installerVersion: string,
  os: string,
  compressedLogsFile: string,
  date: string,
  magic: number,
  type: {
    type : string,
    email: string,
    subject: string,
    problem: string,
  }
};

function typedHttpRequest(
  httpOptions: RequestOptions, requestPayload?: RequestPayload
): Promise<void> {
  return new Promise((resolve, reject) => {
    const options: RequestOptions = Object.assign({}, httpOptions);
    const payload: RequestPayload = Object.assign({}, requestPayload);
    // Prepare multipart/form-data
    const formData = new FormData();
    formData.append('payload', JSON.stringify(payload));

    // prepare file stream (attachment)
    if (payload.compressedLogsFile) {
      const stream = fs.createReadStream(payload.compressedLogsFile);
      const fileName = extractFileNameFromPath(payload.compressedLogsFile);
      formData.append(fileName, stream);
    }

    options.headers = formData.getHeaders();

    const httpRequest = http.request(options);
    httpRequest.on('response', (response) => {
      if (response.statusCode !== 200) {
        return reject();
      }
      response.on('data', () => {});
      response.on('error', (error) => {
        reject(error);
      });
      response.on('end', () => {
        resolve();
      });
    });
    httpRequest.on('error', (error) => reject(error));

    // Attach form-data and trigger the request
    formData.pipe(httpRequest);
  });
}

export const request = typedHttpRequest;
