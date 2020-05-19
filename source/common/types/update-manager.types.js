// @flow

export type UpdateManagerStatusResponse = {
  hasUpdateAvailable?: boolean,
  isDownloadingUpdate?: boolean,
  updateDownloadProgress?: number,
};
