// @flow
import type { UpdateInstallationStatus } from '../types/app-update.types';

export const APP_UPDATE_DOWNLOAD_ID = 'appUpdate';

export const UPDATE_INSTALLATION_STATUSES: {
  [key: string]: UpdateInstallationStatus,
} = {
  PROGRESS: 'progress',
  ERROR: 'error',
  SUCCESS: 'success',
};
