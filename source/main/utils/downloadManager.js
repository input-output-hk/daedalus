// @flow
import { app } from 'electron';
import { ALLOWED_DOWNLOAD_DIRECTORIES } from '../../common/config/download-manager';
import type { DownloadRendererRequest } from '../../common/ipc/api';
import type { AllowedDownloadDirectories } from '../../common/types/download-manager.types';
import { extractFileNameFromPath } from '../../common/utils/files';

// /Users/danilo/iohk/daedalus/source/common/utils/files.js
// /Users/danilo/iohk/daedalus/source/main/utils/downloadManager.js

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
