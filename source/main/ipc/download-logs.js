// @flow
import fs from 'fs';
import { MainIpcChannel } from './lib/MainIpcChannel';
import type {
  DownloadLogsRequest,
  DownloadLogsResponse,
} from '../../common/ipc/api';
import { DOWNLOAD_LOGS_CHANNEL } from '../../common/ipc/api';

export const downloadLogsChannel: (
  MainIpcChannel<DownloadLogsRequest, DownloadLogsResponse>
) = new MainIpcChannel(DOWNLOAD_LOGS_CHANNEL);

export default () => {
  downloadLogsChannel.onRequest((request) => {
    const { compressedLogsFilePath, destinationPath } = request;

    if (!fs.existsSync(compressedLogsFilePath)) {
      return Promise.reject('File does not exist');
    }

    const file = fs.readFileSync(compressedLogsFilePath);
    fs.writeFileSync(destinationPath, file);

    return Promise.resolve();
  });
};
