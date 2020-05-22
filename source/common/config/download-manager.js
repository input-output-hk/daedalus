// @flow

import type { AllowedDownloadDirectories } from '../types/download-manager.types';

export const ALLOWED_DOWNLOAD_DIRECTORIES: {
  [key: string]: AllowedDownloadDirectories,
} = {
  DOWNLOADS: 'downloads',
  DESKTOP: 'desktop',
};
