'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.request = void 0;
const https_1 = __importDefault(require('https'));
const lodash_1 = require('lodash');
const querystring_1 = __importDefault(require('querystring'));
const _1 = require('.');
function typedRequest(httpOptions, queryParams, rawBodyParams) {
  return new Promise((resolve, reject) => {
    const options = Object.assign({}, httpOptions);
    let hasRequestBody = false;
    let requestBody = '';
    let queryString = '';
    if (queryParams && (0, lodash_1.size)(queryParams) > 0) {
      // Handle passphrase
      if ((0, lodash_1.has)(queryParams, 'passphrase')) {
        const passphrase = (0, lodash_1.get)(queryParams, 'passphrase');
        // If passphrase is present it must be encrypted and included in options.path
        if (passphrase) {
          const encryptedPassphrase = (0, _1.encryptPassphrase)(passphrase);
          queryString = `?passphrase=${encryptedPassphrase}`;
        }
        // Passphrase must be omitted from rest query params
        queryParams = (0, lodash_1.omit)(queryParams, 'passphrase');
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'boolean' is not assignable to pa... Remove this comment to see the full error message
        if ((0, lodash_1.size)(queryParams > 1) && passphrase) {
          queryString += `&${querystring_1.default.stringify(queryParams)}`;
        }
      } else {
        queryString = `?${querystring_1.default.stringify(queryParams)}`;
      }
      if (queryString) options.path += queryString;
    }
    // Handle raw body params
    if (rawBodyParams) {
      hasRequestBody = true;
      requestBody = JSON.stringify(rawBodyParams);
      options.headers = {
        'Content-Length': (0, _1.getContentLength)(requestBody),
        'Content-Type': 'application/json',
      };
    }
    // @ts-ignore
    const httpsRequest = https_1.default.request(options);
    if (hasRequestBody) {
      httpsRequest.write(requestBody);
    }
    httpsRequest.on('response', (response) => {
      let body = '';
      // Cardano-sl returns chunked requests, so we need to concat them
      response.on('data', (chunk) => {
        body += chunk;
      });
      // Reject errors
      response.on('error', (error) => reject(error));
      // Resolve JSON results and handle weird backend behavior
      // of "Left" (for errors) and "Right" (for success) properties
      response.on('end', () => {
        try {
          const parsedBody = JSON.parse(body);
          if ((0, lodash_1.has)(parsedBody, 'Right')) {
            // "Right" means 200 ok (success) -> also handle if Right: false (boolean response)
            resolve(parsedBody.Right);
          } else if ((0, lodash_1.has)(parsedBody, 'Left')) {
            // "Left" means error case -> return error with contents (exception on nextUpdate)
            if (parsedBody.Left.contents) {
              reject(new Error(parsedBody.Left.contents));
            } else {
              reject(new Error('Unknown response from backend.'));
            }
          } else {
            // TODO: investigate if that can happen! (no Right or Left in a response)
            reject(new Error('Unknown response from backend.'));
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
exports.request = typedRequest;
//# sourceMappingURL=requestV0.js.map
