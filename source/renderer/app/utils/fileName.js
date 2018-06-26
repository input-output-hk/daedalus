// @flow
import moment from 'moment';

export const logWithTimestamp = (prefix: string = 'logs', filetype: string = 'zip') =>
  `${prefix}-${moment().format('YYYY-MM-DDTHHMMSS')}.${filetype}`;
