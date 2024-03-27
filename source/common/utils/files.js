'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.extractFileNameFromPath = exports.getPathSlash = exports.isFileNameWithTimestamp = exports.generateFileNameWithTimestamp = exports.defaultProps = void 0;
const moment_1 = __importDefault(require('moment'));
const sanitize_filename_1 = __importDefault(require('sanitize-filename'));
exports.defaultProps = {
  prefix: 'logs',
  extension: 'zip',
  isUTC: true,
  sanitize: true,
};
const generateFileNameWithTimestamp = (props = {}) => {
  const { prefix, extension, isUTC, sanitize } = {
    ...exports.defaultProps,
    ...props,
  };
  let date = props.date || (0, moment_1.default)();
  let z = '';
  if (isUTC === true) {
    if (!props || !Object.prototype.hasOwnProperty.call(props, 'date'))
      date = date.utc();
    z = 'Z';
  }
  let fileName = `${prefix}-${`${date.format('YYYY-MM-DDTHHmmss.0SSS')}${z}`}${
    extension ? '.' : ''
  }${extension}`;
  if (sanitize) fileName = (0, sanitize_filename_1.default)(fileName);
  return fileName;
};
exports.generateFileNameWithTimestamp = generateFileNameWithTimestamp;
const isFileNameWithTimestamp = (prefix = 'logs', extension = 'zip') => (
  fileName
) =>
  fileName.match(
    RegExp(
      `(${prefix}-)([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6}.0[0-9]{3}Z)(.${extension})`
    )
  );
exports.isFileNameWithTimestamp = isFileNameWithTimestamp;
const getPathSlash = (path) => (path.indexOf('/') > -1 ? '/' : '\\');
exports.getPathSlash = getPathSlash;
const extractFileNameFromPath = (path) =>
  path.substr(path.lastIndexOf((0, exports.getPathSlash)(path)) + 1);
exports.extractFileNameFromPath = extractFileNameFromPath;
//# sourceMappingURL=files.js.map
