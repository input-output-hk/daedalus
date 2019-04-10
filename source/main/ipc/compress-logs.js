// @flow
import fs from 'fs';
import archiver from 'archiver';
import path from 'path';
import { get } from 'lodash';
import { appLogsFolderPath, pubLogsFolderPath } from '../config';
import { Logger } from '../utils/logging';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { COMPRESS_LOGS_CHANNEL } from '../../common/ipc/api';
import type {
  CompressLogsRendererRequest,
  CompressLogsMainResponse,
} from '../../common/ipc/api';

export const compressLogsChannel: MainIpcChannel<
  CompressLogsRendererRequest,
  CompressLogsMainResponse
> = new MainIpcChannel(COMPRESS_LOGS_CHANNEL);

export default () => {
  compressLogsChannel.onRequest(
    ({ logs, compressedFileName }) =>
      new Promise((resolve, reject) => {
        const outputPath = path.join(appLogsFolderPath, compressedFileName);
        const output = fs.createWriteStream(outputPath);
        const archive = archiver('zip', {
          zlib: { level: 9 }, // Sets the compression level
        });

        output.on('close', () => {
          Logger.debug('COMPRESS_LOGS.SUCCESS', { outputPath });
          resolve(outputPath);
        });

        archive.on('error', error => {
          Logger.error('COMPRESS_LOGS.ERROR', { error });
          reject(error);
        });

        Logger.debug('COMPRESS_LOGS.START');

        // compress files
        const logFiles = get(logs, ['files'], []);
        for (let i = 0; i < logFiles.length; i++) {
          const stream = fs.readFileSync(
            path.join(pubLogsFolderPath, logFiles[i])
          );
          const name = logFiles[i].replace('.log', '');
          archive.append(stream, { name });
        }

        archive.finalize(error => {
          if (error) {
            Logger.error('COMPRESS_LOGS.ERROR', { error });
            reject(error);
          }
        });

        archive.pipe(output);
      })
  );
};
