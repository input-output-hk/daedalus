// https://www.npmjs.com/package/node-downloader-helper
import type {
  AllowedDownloadDirectories,
  DownloadState,
  DownloadEventType,
} from '../types/downloadManager.types';

export const ALLOWED_DOWNLOAD_DIRECTORIES: Record<
  string,
  AllowedDownloadDirectories
> = {
  DOWNLOADS: 'downloads',
  DESKTOP: 'desktop',
  STATE_DIRECTORY: 'stateDirectory',
};
// DH_STATES in 'node-downloader-helper/src';
export const DOWNLOAD_STATES: Record<string, DownloadState> = {
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
export const DOWNLOAD_EVENT_TYPES: Record<string, DownloadEventType> = {
  START: 'start',
  DOWNLOAD: 'download',
  PROGRESS: 'progress',
  END: 'end',
  TIMEOUT: 'timeout',
  STOP: 'stop',
  PAUSE: 'pause',
  ERROR: 'error',
};
export const DEFAULT_DIRECTORY_NAME =
  ALLOWED_DOWNLOAD_DIRECTORIES.STATE_DIRECTORY;
export const TEMPORARY_FILENAME = {
  prefix: 'Unconfirmed',
  extension: 'crdownload',
};
export const DOWNLOAD_DATA_DEFAULT = {
  state: DOWNLOAD_STATES.IDLE,
  remainingSize: 0,
  serverFileSize: 0,
  diskFileSize: 0,
  downloadSize: 0,
  progress: 0,
  speed: 0,
  incomplete: false,
  isResumed: false,
};
export const ERROR_TIME_AFTER_NO_END_EVENT = 10 * 1000; // 10 seconds | unit: milliseconds
