// @flow

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

export const DOWNLOAD_PROGRESS_STATUSES: {
  [key: string]: DownloadProgressStatuses,
} = {
  START: 'start',
  DOWNLOAD: 'download',
  END: 'end',
  ERROR: 'error',
  STATECHANGED: 'stateChanged',
  TIMEOUT: 'timeout',
  PROGRESS: 'progress',
};
