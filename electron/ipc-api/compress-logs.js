import { ipcMain } from 'electron';
import fs from 'fs';
import archiver from 'archiver';
import path from 'path';
import splitFile from 'split-file';
import { map, get } from 'lodash';
import { appLogsFolderPath, pubLogsFolderPath } from '../config';
import { Logger, stringifyError } from '../../app/utils/logging';

const CHANNEL_NAME = 'compress-logs';

export const COMPRESS_LOGS = {
  REQUEST: CHANNEL_NAME,
  SUCCESS: `${CHANNEL_NAME}-success`,
  MAX_SIZE: 7 * 1024 * 1024, // 7mb in bytes
};

export default () => {
  ipcMain.on(COMPRESS_LOGS.REQUEST, (event, logs) => {
    const sender = event.sender;
    const compressedFileName = 'logs.zip';

    const outputPath = path.join(appLogsFolderPath, compressedFileName);
    const output = fs.createWriteStream(outputPath);
    const archive = archiver('zip', {
      zlib: { level: 9 } // Sets the compression level
    });

    const handleSuccessResponse = (files) => {
      const response = {
        files,
        path: appLogsFolderPath,
        originalFile: outputPath,
      };
      // response on compression success
      Logger.info('COMPRESS_LOGS.SUCCESS');
      return sender.send(COMPRESS_LOGS.SUCCESS, response);
    };

    output.on('close', () => {
      const archiveSize = archive.pointer();
      const fileNames = [];

      if (archiveSize > COMPRESS_LOGS.MAX_SIZE) {
        // split archive - one part can have max 7mb
        splitFile.splitFileBySize(outputPath, COMPRESS_LOGS.MAX_SIZE)
          .then((files) => {
            map(files, (file) => {
              fileNames.push(file.replace(appLogsFolderPath + '/', ''));
            });
            return handleSuccessResponse(fileNames);
          })
          .catch((err) => {
            Logger.error('COMPRESS_LOGS.ERROR: ' + stringifyError(err));
            return sender.send(COMPRESS_LOGS.ERROR, err);
          });
      } else {
        handleSuccessResponse([compressedFileName]);
      }
    });

    archive.on('error', (err) => {
      Logger.error('COMPRESS_LOGS.ERROR: ' + stringifyError(err));
      return sender.send(COMPRESS_LOGS.ERROR, err);
    });

    Logger.info('COMPRESS_LOGS started');

    // compress files
    const logFiles = get(logs, ['files'], []);
    for (let i = 0; i < logFiles.length; i++) {
      const stream = fs.readFileSync(path.join(pubLogsFolderPath, logFiles[i]));
      archive.append(stream, { name: logFiles[i] });
    }

    archive.finalize((err) => {
      if (err) {
        Logger.error('COMPRESS_LOGS.ERROR: ' + stringifyError(err));
        return sender.send(COMPRESS_LOGS.ERROR, err);
      }
    });

    archive.pipe(output);
  });
};
