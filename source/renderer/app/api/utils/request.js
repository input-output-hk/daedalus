// @flow
import axios from 'axios';
import { size, has, get, omit, isEmpty } from 'lodash';
import querystring from 'querystring';
import { encryptPassphrase, getContentLength } from '.';

export type HTTPSConfig = {
  hostname: string,
  method: string,
  path: string,
  port: number,
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
  headers?: RequestHeaders,
};

export type RequestHeaders = {
  'Content-Type': string,
  'Content-Length': number,
};

export type RequestCredentials = {
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
};

export type RequestBaseURL = {
  hostname: string,
  port: number,
};

const getBodyData = (rawBodyParams): string => {
  if (!rawBodyParams || isEmpty(rawBodyParams)) {
    return '';
  }
  return JSON.stringify(rawBodyParams);
};

const getQueryParams = (queryParams: ?Object): ?Object => {
  if (queryParams && !isEmpty(queryParams) && has(queryParams, 'passphrase')) {
    return { ...queryParams, passphrase: getEncryptedPassphrase(queryParams) };
  }
  return queryParams;
};

const getEncryptedPassphrase = (queryParams: ?Object): string => {
  let queryString = '';
  if (!queryParams || isEmpty(queryParams) || size(queryParams) <= 0) {
    return queryString;
  }
  const unencryptedPassphrase = has(queryParams, 'passphrase')
    ? get(queryParams, 'passphrase')
    : queryString;
  if (unencryptedPassphrase) {
    // If passphrase is present it must be encrypted
    const encryptedPassphrase = encryptPassphrase(unencryptedPassphrase);
    queryString = `?passphrase=${encryptedPassphrase}`;
  }
  return queryString;
};

const getHeaders = (
  headers?: RequestHeaders,
  requestBodyLength: ?number
): Object => {
  const baseHeaders = {
    'Content-Length': requestBodyLength || '',
    'Content-Type': 'application/json; charset=utf-8',
    Accept: 'application/json; charset=utf-8',
  };
  if (!headers || isEmpty(headers)) {
    return baseHeaders;
  }
  return { ...baseHeaders, ...headers };
};

const getHttpsAgent = (credentials: RequestCredentials) =>
  global.https.Agent({ ...credentials });

const getBaseURL = (base: RequestBaseURL): string =>
  `https://${base.hostname}:${base.port}`;

export const axiosRequest = async (
  httpsConfig: HTTPSConfig,
  queryParams?: {},
  rawBodyParams?: any,
  requestOptions?: { returnMeta: boolean }
): Promise<*> => {
  console.log(
    '**** --- ARGUEMENTS START -------------------------------------->'
  );
  console.log(
    `httpsConfig: ${JSON.stringify(
      omit(httpsConfig, 'ca', 'cert', 'key'),
      null,
      2
    )}`
  );
  console.log(`queryParams: ${JSON.stringify(queryParams, null, 2)}`);
  console.log(`rawBodyParams: ${JSON.stringify(rawBodyParams, null, 2)}`);
  console.log(`requestOptions: ${JSON.stringify(requestOptions, null, 2)}`);
  console.log(
    '**** --- ARGUEMENTS END -------------------------------------->'
  );

  const { headers, hostname, method, path, port, ca, cert, key } = httpsConfig;

  const requestBaseURL = getBaseURL({ hostname, port });
  const requestBody = getBodyData(rawBodyParams);
  const requestBodyLength = getContentLength(requestBody);
  const requestHeaders = getHeaders(headers, requestBodyLength);
  const requestParams = getQueryParams(queryParams);
  const requestAgent = getHttpsAgent({ ca, cert, key });

  const sendRequest = axios.create({
    responseType: 'json',
    responseEncoding: 'utf8',
    method,
    url: path,
    baseURL: requestBaseURL,
    headers: requestHeaders,
    params: requestParams,
    data: requestBody,
    httpsAgent: requestAgent,
  });
  debugger;
  // begin response handling
  return new Promise(async (resolve, reject) => {
    try {
      const response = await sendRequest();
      debugger;
      if (response.data) {
        const { data } = response;
        debugger;
        resolve(data);
      }
    } catch (error) {
      debugger;
      reject(error);
    }
  });
};

function typedRequest<Response>(
  httpOptions: HTTPSConfig,
  queryParams?: {},
  rawBodyParams?: any,
  requestOptions?: { returnMeta: boolean }
): Promise<Response> {
  return new Promise((resolve, reject) => {
    const httpsOptions = JSON.stringify(
      omit(httpOptions, 'ca', 'cert', 'key'),
      null,
      2
    );
    console.log(
      '**** --- HERE ARE THE ARGUEMENTS -------------------------------------->'
    );
    console.log(`httpOptions: ${httpsOptions}`);
    console.log(`queryParams: ${JSON.stringify(queryParams, null, 2)}`);
    console.log(`rawBodyParams: ${rawBodyParams}`);
    console.log(`requestOptions: ${requestOptions}`);
    console.log(
      '**** --- ARGUEMENTS END -------------------------------------->'
    );
    const options: HTTPSConfig = Object.assign({}, httpOptions);
    const { returnMeta } = Object.assign({}, requestOptions);
    let hasRequestBody = false;
    let requestBody = '';

    let queryString = '';
    if (queryParams && size(queryParams) > 0) {
      // Handle passphrase
      if (has(queryParams, 'passphrase')) {
        const passphrase = get(queryParams, 'passphrase');

        // If passphrase is present it must be encrypted and included in options.path
        if (passphrase) {
          const encryptedPassphrase = encryptPassphrase(passphrase);
          queryString = `?passphrase=${encryptedPassphrase}`;
        }

        // Passphrase must be ommited from rest query params
        queryParams = omit(queryParams, 'passphrase');

        if (size(queryParams > 1) && passphrase) {
          queryString += `&${querystring.stringify(queryParams)}`;
        }
      } else {
        queryString = `?${querystring.stringify(queryParams)}`;
      }

      if (queryString) options.path += queryString;
    }

    // Handle raw body params
    if (rawBodyParams) {
      hasRequestBody = true;
      requestBody = JSON.stringify(rawBodyParams);
      options.headers = {
        'Content-Length': getContentLength(requestBody),
        'Content-Type': 'application/json; charset=utf-8',
        Accept: 'application/json; charset=utf-8',
      };
    }

    const httpsRequest = global.https.request(options);
    debugger;
    if (hasRequestBody) {
      httpsRequest.write(requestBody);
    }
    httpsRequest.on('response', response => {
      let body = '';
      // Cardano-sl returns chunked requests, so we need to concat them
      response.on('data', chunk => (body += chunk));
      // Reject errors
      response.on('error', error => reject(error));
      // Resolve JSON results and handle backend errors
      response.on('end', () => {
        try {
          // When deleting a wallet, the API does not return any data in body
          // even if it was successful
          const { statusCode, statusMessage } = response;

          if (!body && statusCode >= 200 && statusCode <= 206) {
            // adds status and data properties so JSON.parse doesn't throw an error
            body = `{
              "status": "success",
              "data": "statusCode: ${statusCode} -- statusMessage: ${statusMessage}"
            }`;
          } else if (
            options.path === '/api/internal/next-update' &&
            statusCode === 404
          ) {
            // when nextAdaUpdate receives a 404, it isn't an error
            // it means no updates are available
            body = `{
              "status": "success",
              "data": null
            }`;
          }

          const parsedBody = JSON.parse(body);
          const status = get(parsedBody, 'status', false);
          if (status) {
            if (status === 'success') {
              resolve(returnMeta ? parsedBody : parsedBody.data);
            } else if (status === 'error' || status === 'fail') {
              reject(parsedBody);
            } else {
              // TODO: find a way to record this case and report to the backend team
              reject(new Error('Unknown response from backend.'));
            }
          } else {
            // TODO: find a way to record this case and report to the backend team
            reject(new Error('Unknown response from backend.'));
          }
        } catch (error) {
          // Handle internal server errors (e.g. HTTP 500 - 'Something went wrong')
          reject(new Error(error));
        }
      });
    });
    httpsRequest.on('error', error => reject(error));
    httpsRequest.end();
  });
}

export const request = typedRequest;
