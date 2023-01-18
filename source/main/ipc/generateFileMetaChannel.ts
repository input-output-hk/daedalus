import mime from 'mime-types';
import path from 'path';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GENERATE_FILE_META_CHANNEL } from '../../common/ipc/api';
import type {
  GenerateFileMetaMainResponse,
  GenerateFileMetaRendererRequest,
} from '../../common/ipc/api';

export const generateFileMetaChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  GenerateFileMetaRendererRequest,
  GenerateFileMetaMainResponse
> = new MainIpcChannel(GENERATE_FILE_META_CHANNEL);
export const handleFileMetaRequests = () => {
  generateFileMetaChannel.onReceive(
    (request: GenerateFileMetaRendererRequest) =>
      new Promise((resolve, reject) => {
        const { filePath } = request;

        try {
          resolve({
            fileName: path.basename(filePath),
            fileType: mime.lookup(filePath),
          });
        } catch (err) {
          reject(err);
        }
      })
  );
};
