// @flow
// https://www.npmjs.com/package/node-downloader-helper
import type {
  AllowedDownloadDirectories,
  DownloadProgressStatuses,
} from '../types/download-manager.types';

export const ALLOWED_DOWNLOAD_DIRECTORIES: {
  [key: string]: AllowedDownloadDirectories,
} = {
  DOWNLOADS: 'downloads',
  DESKTOP: 'desktop',
};

// DH_STATES in 'node-downloader-helper/src';
export const DOWNLOAD_PROGRESS_STATUSES: {
  [key: string]: DownloadProgressStatuses,
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

// export const DEFAULT_DIRECTORY_NAME = ALLOWED_DOWNLOAD_DIRECTORIES.DOWNLOADS;
export const DEFAULT_DIRECTORY_NAME = ALLOWED_DOWNLOAD_DIRECTORIES.DESKTOP;

export const TEMPORARY_FILENAME = {
  prefix: 'Unconfirmed',
  extension: 'crdownload',
};

export const DOWNLOAD_INFO_DEFAULT = {
  downloaded: 0,
  downloadedSize: 0,
  fileName: '',
  incomplete: false,
  isResumed: false,
  onDiskSize: 0,
  progress: 0,
  remainingSize: 0,
  speed: 0,
  totalSize: 0,
};
