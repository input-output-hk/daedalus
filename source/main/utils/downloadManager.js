// @flow

import { app } from 'electron';
import {
  ALLOWED_DOWNLOAD_DIRECTORIES,
  DOWNLOAD_INFO_DEFAULT,
  DOWNLOAD_PROGRESS_STATUSES as statuses,
} from '../../common/config/download-manager';
import { extractFileNameFromPath } from '../../common/utils/files';
import type { DownloadRendererRequest } from '../../common/ipc/api';
import type {
  AllowedDownloadDirectories,
  DownloadInfo,
  DownloadInfoFromEvent,
  DownloadEventType,
} from '../../common/types/download-manager.types';

export const getPathFromDirectoryName = (
  directoryName: AllowedDownloadDirectories
) => {
  switch (directoryName) {
    case ALLOWED_DOWNLOAD_DIRECTORIES.DESKTOP:
      return app.getPath('desktop');
    default:
      return app.getPath('downloads');
  }
};

export const getOriginalFilename = ({
  fileUrl,
  options,
}: DownloadRendererRequest) =>
  options && typeof options.fileName === 'string'
    ? options.fileName
    : extractFileNameFromPath(fileUrl);

export const formatUpdate = (status: DownloadEventType) => {
  switch (status) {
    case statuses.STARTED:
      return (update: DownloadInfoFromEvent): DownloadInfo =>
        Object.assign({}, DOWNLOAD_INFO_DEFAULT, update);
    case statuses.FINISHED:
      return (update: DownloadInfoFromEvent): DownloadInfo =>
        Object.assign({}, DOWNLOAD_INFO_DEFAULT, update);
    case statuses.ERROR:
      return (update: DownloadInfoFromEvent): DownloadInfo =>
        Object.assign({}, DOWNLOAD_INFO_DEFAULT, update);
    default:
      return (update: DownloadInfoFromEvent): DownloadInfo =>
        Object.assign({}, DOWNLOAD_INFO_DEFAULT, update);
  }
};
