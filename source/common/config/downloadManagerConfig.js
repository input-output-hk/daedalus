'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.ERROR_TIME_AFTER_NO_END_EVENT = exports.DOWNLOAD_DATA_DEFAULT = exports.TEMPORARY_FILENAME = exports.DEFAULT_DIRECTORY_NAME = exports.DOWNLOAD_EVENT_TYPES = exports.DOWNLOAD_STATES = exports.ALLOWED_DOWNLOAD_DIRECTORIES = void 0;
exports.ALLOWED_DOWNLOAD_DIRECTORIES = {
  DOWNLOADS: 'downloads',
  DESKTOP: 'desktop',
  STATE_DIRECTORY: 'stateDirectory',
};
// DH_STATES in 'node-downloader-helper/src';
exports.DOWNLOAD_STATES = {
  IDLE: 'IDLE',
  SKIPPED: 'SKIPPED',
  STARTED: 'STARTED',
  DOWNLOADING: 'DOWNLOADING',
  RETRY: 'RETRY',
  PAUSED: 'PAUSED',
  RESUMED: 'RESUMED',
  STOPPED: 'STOPPED',
  FINISHED: 'FINISHED',
  FAILED: 'FAILED',
};
exports.DOWNLOAD_EVENT_TYPES = {
  START: 'start',
  DOWNLOAD: 'download',
  PROGRESS: 'progress',
  END: 'end',
  TIMEOUT: 'timeout',
  STOP: 'stop',
  PAUSE: 'pause',
  ERROR: 'error',
};
exports.DEFAULT_DIRECTORY_NAME =
  exports.ALLOWED_DOWNLOAD_DIRECTORIES.STATE_DIRECTORY;
exports.TEMPORARY_FILENAME = {
  prefix: 'Unconfirmed',
  extension: 'crdownload',
};
exports.DOWNLOAD_DATA_DEFAULT = {
  state: exports.DOWNLOAD_STATES.IDLE,
  remainingSize: 0,
  serverFileSize: 0,
  diskFileSize: 0,
  downloadSize: 0,
  progress: 0,
  speed: 0,
  incomplete: false,
  isResumed: false,
};
exports.ERROR_TIME_AFTER_NO_END_EVENT = 10 * 1000; // 10 seconds | unit: milliseconds
//# sourceMappingURL=downloadManagerConfig.js.map
