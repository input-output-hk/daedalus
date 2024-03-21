'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const moment_1 = __importDefault(require('moment'));
const blockSyncProgressHelpers_1 = require('./blockSyncProgressHelpers');
describe('blockSyncProgressHelpers', () => {
  it('should return true for newer logs', () => {
    const time = '2022-02-18 13:18:47.66';
    const applicationStartDate = moment_1.default.utc(time).add(-1, 'minute');
    const line = `[34m[XX-M:cardano.node.ChainDB:Info:30][0m [${time} UTC] Validating chunk no. 2256 out of 2256. Progress: 99.96%`;
    expect(
      (0, blockSyncProgressHelpers_1.isItFreshLog)(applicationStartDate, line)
    ).toBe(true);
  });
  it('should return false for logs of same time', () => {
    const time = '2022-02-18 13:18:47.66';
    const applicationStartDate = moment_1.default.utc(time);
    const line = `[34m[XX-M:cardano.node.ChainDB:Info:30][0m [${time} UTC] Validating chunk no. 2256 out of 2256. Progress: 99.96%`;
    expect(
      (0, blockSyncProgressHelpers_1.isItFreshLog)(applicationStartDate, line)
    ).toBe(false);
  });
  it('should return false for older logs', () => {
    const time = '2022-02-18 13:18:47.66';
    const applicationStartDate = moment_1.default.utc(time).add(1, 'minute');
    const line = `[34m[XX-M:cardano.node.ChainDB:Info:30][0m [${time} UTC] Validating chunk no. 2256 out of 2256. Progress: 99.96%`;
    expect(
      (0, blockSyncProgressHelpers_1.isItFreshLog)(applicationStartDate, line)
    ).toBe(false);
  });
});
//# sourceMappingURL=blockSyncProgressHelpers.spec.js.map
