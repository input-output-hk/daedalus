import fs from 'fs';
import { MainIpcChannel } from './lib/MainIpcChannel';
import type {
  DownloadLogsRendererRequest,
  DownloadLogsMainResponse,
} from '../../common/ipc/api';
import { DOWNLOAD_LOGS_CHANNEL } from '../../common/ipc/api';

export const downloadLogsChannel: MainIpcChannel<
  DownloadLogsRendererRequest,
  DownloadLogsMainResponse
> = new MainIpcChannel(DOWNLOAD_LOGS_CHANNEL);
export default () => {
  downloadLogsChannel.onRequest((request) => {
    const { compressedLogsFilePath, destinationPath } = request;

    if (!fs.existsSync(compressedLogsFilePath)) {
      return Promise.reject(new Error('File does not exist'));
    }

    const file = fs.readFileSync(compressedLogsFilePath);
    fs.writeFileSync(destinationPath, file);
    return Promise.resolve();
  });
};
