import moment from 'moment';

export const filenameWithTimestamp = (prefix = 'logs', filetype = 'zip') =>
  `${prefix}-${moment().format('YYYY-MM-DDThhmmss')}.${filetype}`;

export const getFilenameWithTimestamp = (prefix = 'logs', filetype = 'zip') => fileName =>
  fileName.match(RegExp(`(${prefix}-)([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6})(.${filetype})`));
