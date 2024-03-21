'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.setupApi = void 0;
const api_1 = __importDefault(require('./api'));
const localStorage_1 = __importDefault(require('./utils/localStorage'));
const setupApi = (isTest) => ({
  ada: new api_1.default(isTest, {
    hostname: '127.0.0.1',
    port: 8090,
    ca: Uint8Array.from([]),
    key: Uint8Array.from([]),
    cert: Uint8Array.from([]),
  }),
  localStorage: new localStorage_1.default(),
});
exports.setupApi = setupApi;
//# sourceMappingURL=index.js.map
