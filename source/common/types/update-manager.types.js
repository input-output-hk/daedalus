// @flow

export type UpdateManagerStatusResponse = {
  isUpdateAvailable?: boolean,
  isDownloadingUpdate?: boolean,
  updateDownloadProgress?: number,
  haspendingUpdateDownload?: boolean,
};
