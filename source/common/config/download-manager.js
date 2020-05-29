// @flow
// https://www.npmjs.com/package/node-downloader-helper
import type {
  AllowedDownloadDirectories,
  DownloadState,
  DownloadEventType,
} from '../types/download-manager.types';

export const ALLOWED_DOWNLOAD_DIRECTORIES: {
  [key: string]: AllowedDownloadDirectories,
} = {
  DOWNLOADS: 'downloads',
  DESKTOP: 'desktop',
};

// DH_STATES in 'node-downloader-helper/src';
export const DOWNLOAD_STATES: {
  [key: string]: DownloadState,
} = {
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

export const DOWNLOAD_EVENT_TYPES: {
  [key: string]: DownloadEventType,
} = {
  START: 'start',
  DOWNLOAD: 'download',
  PROGRESS: 'progress',
  END: 'end',
  TIMEOUT: 'timeout',
  ERROR: 'error',
};

// export const DEFAULT_DIRECTORY_NAME = ALLOWED_DOWNLOAD_DIRECTORIES.DOWNLOADS;
export const DEFAULT_DIRECTORY_NAME = ALLOWED_DOWNLOAD_DIRECTORIES.DESKTOP;

export const TEMPORARY_FILENAME = {
  prefix: 'Unconfirmed',
  extension: 'crdownload',
};

export const DOWNLOAD_PROGRESS_DEFAULT = {
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
