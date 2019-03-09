// @flow
import moment from 'moment';

// time utils
export const unixTimestampToDate = (timestamp: number) => new Date(timestamp * 1000);
export const utcStringToDate = (createDate: string) => moment.utc(createDate).toDate();

// string utils
export const getContentLength = (content: string) => (
  // 'TextEncoder' is used to measure correct length of UTF-8 strings
  (new TextEncoder()).encode(content).length
);
