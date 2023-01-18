import moment from 'moment';
import { isItFreshLog } from './blockSyncProgressHelpers';

describe('blockSyncProgressHelpers', () => {
  it('should return true for newer logs', () => {
    const time = '2022-02-18 13:18:47.66';
    const applicationStartDate = moment.utc(time).add(-1, 'minute');
    const line = `[34m[XX-M:cardano.node.ChainDB:Info:30][0m [${time} UTC] Validating chunk no. 2256 out of 2256. Progress: 99.96%`;

    expect(isItFreshLog(applicationStartDate, line)).toBe(true);
  });

  it('should return false for logs of same time', () => {
    const time = '2022-02-18 13:18:47.66';
    const applicationStartDate = moment.utc(time);
    const line = `[34m[XX-M:cardano.node.ChainDB:Info:30][0m [${time} UTC] Validating chunk no. 2256 out of 2256. Progress: 99.96%`;

    expect(isItFreshLog(applicationStartDate, line)).toBe(false);
  });

  it('should return false for older logs', () => {
    const time = '2022-02-18 13:18:47.66';
    const applicationStartDate = moment.utc(time).add(1, 'minute');
    const line = `[34m[XX-M:cardano.node.ChainDB:Info:30][0m [${time} UTC] Validating chunk no. 2256 out of 2256. Progress: 99.96%`;

    expect(isItFreshLog(applicationStartDate, line)).toBe(false);
  });
});
