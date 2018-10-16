// @flow
import moment from 'moment';
import blakejs from 'blakejs';

// time utils
export const unixTimestampToDate = (timestamp: number) => new Date(timestamp * 1000);
export const utcStringToDate = (createDate: string) => moment.utc(createDate).toDate();

// passphrase utils
const bytesToB16 = (bytes) => Buffer.from(bytes).toString('hex');
const blake2b = (data) => blakejs.blake2b(data, null, 32);

export const encryptPassphrase = (passphrase: ?string) => (
  bytesToB16(blake2b(passphrase))
);

// string utils
export const getContentLength = (content: string) => (
  // 'TextEncoder' is used to measure correct length of UTF-8 strings
  (new TextEncoder()).encode(content).length
);
