import moment from 'moment';

export const filenameWithTimestamp = (prefix = 'logs', filetype = 'zip') =>
  `${prefix}-${moment().format('YYYY-MM-DDThhmmss')}.${filetype}`;
