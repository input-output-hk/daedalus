// @flow
import moment from 'moment';

export const generateFileNameWithTimestamp = (prefix: string = 'logs', fileType: string = 'zip') =>
  `${prefix}-${moment.utc().format('YYYY-MM-DDTHHmmss.0SSS')}Z.${fileType}`;

export const getFileNameWithTimestamp = (prefix: string = 'logs', fileType: string = 'zip') => (fileName: string) =>
  fileName.match(RegExp(`(${prefix}-)([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6}.0[0-9]{3}Z)(.${fileType})`));
