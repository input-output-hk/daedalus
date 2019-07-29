// @flow
import fs from 'fs';
import mime from 'mime-types';
import path from 'path';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GENERATE_FILE_BLOB_CHANNEL } from '../../common/ipc/api';
import type {
  GenerateFileContentMainResponse,
  GenerateFileContentRendererRequest,
} from '../../common/ipc/api';

export const generateFileContentChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  GenerateFileContentRendererRequest,
  GenerateFileContentMainResponse
> = new MainIpcChannel(GENERATE_FILE_BLOB_CHANNEL);

export const handleFileContentRequests = () => {
  generateFileContentChannel.onReceive(
    (request: GenerateFileContentRendererRequest) =>
      new Promise((resolve, reject) => {
        const { filePath } = request;
        try {
          const fileBuffer = fs.readFileSync(filePath);
          const fileLastModified = fs.statSync(filePath).mtime.getTime();
          resolve({
            fileBuffer,
            fileLastModified,
            fileName: path.basename(filePath),
            fileType: mime.lookup(filePath),
          });
        } catch (err) {
          reject(err);
        }
      })
  );
};
