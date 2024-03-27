'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.externalRequest = void 0;
const lodash_1 = require('lodash');
const urlsConfig_1 = require('../../config/urlsConfig');
const externalRequest = (httpOptions, raw = false) =>
  new Promise((resolve, reject) => {
    if (
      !urlsConfig_1.ALLOWED_EXTERNAL_HOSTNAMES.includes(httpOptions.hostname)
    ) {
      return reject(new Error('Hostname not allowed'));
    }
    const { protocol = 'https' } = httpOptions;
    const options = (0, lodash_1.omit)(httpOptions, 'protocol');
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
exports.externalRequest = externalRequest;
//# sourceMappingURL=externalRequest.js.map
