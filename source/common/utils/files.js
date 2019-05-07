// @flow
import moment from 'moment';

const defaultProps = {
  prefix: 'logs',
  fileType: 'zip',
  timestamp: `${moment.utc().format('YYYY-MM-DDTHHmmss.0SSS')}Z`,
};

type Props = {
  prefix?: string,
  fileType?: string,
  timestamp?: string,
};

export const generateFileNameWithTimestamp = (props: Props) => {
  const { prefix, fileType, timestamp } = {
    ...defaultProps,
    ...props,
  };
  return `${prefix}-${timestamp}${fileType ? '.' : ''}${fileType}`;
};

export const isFileNameWithTimestamp = (
  prefix: string = 'logs',
  fileType: string = 'zip'
) => (fileName: string) =>
  fileName.match(
    RegExp(
      `(${prefix}-)([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6}.0[0-9]{3}Z)(.${fileType})`
    )
  );

export const getPathSlash = (path: string) =>
  path.indexOf('/') > -1 ? '/' : '\\';

export const extractFileNameFromPath = (path: string) =>
  path.substr(path.lastIndexOf(getPathSlash(path)) + 1);
