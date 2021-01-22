// @flow
import { omit, size, assign, cloneDeep, has, keys } from 'lodash';
import querystring from 'querystring';
import { getContentLength } from '.';

export type RequestOptions = {
  hostname: string,
  method: string,
  path: string,
  port: number,
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
  headers?: {
    'Content-Type': string,
    'Content-Length': number,
  },
};

const { isIncentivizedTestnet } = global.environment;

function typedRequest<Response>(
  httpOptions: RequestOptions,
  queryParams?: {},
  rawBodyParams?: any
): Promise<Response> {
  return new Promise((resolve, reject) => {
    const options: RequestOptions = cloneDeep(httpOptions);
    // const { returnMeta } = Object.assign({}, requestOptions);
    let hasRequestBody = false;
    let requestBody = '';

    if (queryParams && size(queryParams) > 0) {
      options.path += `?${querystring.stringify(queryParams)}`;
    }

    // Handle raw body params
    if (rawBodyParams) {
      hasRequestBody = true;
      requestBody = JSON.stringify(rawBodyParams);
      options.headers = assign(
        options.headers,
        omit(
          {
            'Content-Length': getContentLength(requestBody),
            'Content-Type': 'application/json; charset=utf-8',
            Accept: 'application/octet-stream',
          },
          has(httpOptions, 'headers') ? keys(httpOptions.headers) : []
        )
      );
    }

    const httpOnlyOptions = omit(options, ['ca', 'cert', 'key']);
    const httpsRequest = isIncentivizedTestnet
      ? global.http.request(httpOnlyOptions)
      : global.https.request(options);

    if (hasRequestBody) {
      httpsRequest.write(requestBody);
    }
    httpsRequest.on('response', (response) => {
      let body;
      // Cardano-sl returns chunked requests, so we need to concat them
      response.on('data', (chunk) => {
        body = chunk;
      });
      // Reject errors
      response.on('error', (error) => reject(error));
      // Resolve JSON results and handle backend errors
      response.on('end', () => {
        try {
          const { statusCode } = response;

          if (statusCode >= 200 && statusCode <= 206) {
            resolve(body);
          } else if (body) {
            // Error response with a body
            const parsedBody = JSON.parse(body);
            if (parsedBody.code && parsedBody.message) {
              reject(parsedBody);
            } else {
              reject(new Error('Unknown API response'));
            }
          } else {
            // Error response without a body
            reject(new Error('Unknown API response'));
          }
        } catch (error) {
          // Handle internal server errors (e.g. HTTP 500 - 'Something went wrong')
          reject(new Error(error));
        }
      });
    });
    httpsRequest.on('error', (error) => reject(error));
    httpsRequest.end();
  });
}

export const request = typedRequest;
