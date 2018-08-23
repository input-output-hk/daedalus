// @flow
import moment from 'moment';

export const unixTimestampToDate = (timestamp: number) => new Date(timestamp * 1000);
export const utcStringToDate = (createDate: string) => moment.utc(createDate).toDate();
