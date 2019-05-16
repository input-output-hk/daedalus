// @flow
import moment from 'moment';

export const defaultProps = {
  prefix: 'logs',
  extention: 'zip',
  date: moment(),
  isUTC: true,
};

type Props = {
  prefix?: string,
  extention?: string,
  date?: moment,
  isUTC?: boolean,
};

export const generateFileNameWithTimestamp = (props?: Props) => {
  const { prefix, extention, isUTC } = {
    ...defaultProps,
    ...props,
  };
  let { date } = {
    ...defaultProps,
    ...props,
  };
  let z = '';
  if (isUTC === true) {
    if (!props || !Object.prototype.hasOwnProperty.call(props, 'date'))
      date = date.utc();
    z = 'Z';
  }
  return `${prefix}-${`${date.format('YYYY-MM-DDTHHmmss.0SSS')}${z}`}${
    extention ? '.' : ''
  }${extention}`;
};

export const isFileNameWithTimestamp = (
  prefix: string = 'logs',
  extention: string = 'zip'
) => (fileName: string) =>
  fileName.match(
    RegExp(
      `(${prefix}-)([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6}.0[0-9]{3}Z)(.${extention})`
    )
  );

export const getPathSlash = (path: string) =>
  path.indexOf('/') > -1 ? '/' : '\\';

export const extractFileNameFromPath = (path: string) =>
  path.substr(path.lastIndexOf(getPathSlash(path)) + 1);
