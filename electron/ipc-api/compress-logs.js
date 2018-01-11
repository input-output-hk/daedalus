import { ipcMain } from 'electron';
import fs from 'fs';
import archiver from 'archiver';
import path from 'path';
import splitFile from 'split-file';
import { get } from 'lodash';
import { appLogsFolderPath } from '../config';
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

    output.on('close', () => {
      const archiveSize = archive.pointer();

      if (archiveSize > COMPRESS_LOGS.MAX_SIZE) {
        // split archive - one part can have max 7mb
        splitFile.splitFileBySize(outputPath, COMPRESS_LOGS.MAX_SIZE)
          .then((files) => {
            // response on compression success
            Logger.info('COMPRESS_LOGS.SUCCESS');
            return sender.send(COMPRESS_LOGS.SUCCESS, {
              files, // array of zip parts
              originalFile: outputPath,
              archive,
              message: 'Logs compressed successfully',
              success: true,
              warnings: 0,
            });
          })
          .catch((err) => {
            Logger.error('COMPRESS_LOGS.ERROR: ' + stringifyError(err));
            return sender.send(COMPRESS_LOGS.ERROR, err);
          });
      } else {
        Logger.info('COMPRESS_LOGS.SUCCESS');
        // response on compression success
        return sender.send(COMPRESS_LOGS.SUCCESS, {
          files: [outputPath],
          originalFile: outputPath,
          archive,
          message: 'Logs compressed successfully',
          success: true,
          warnings: 0,
        });
      }
    });

    archive.on('error', (err) => {
      Logger.error('COMPRESS_LOGS.ERROR: ' + stringifyError(err));
      return sender.send(COMPRESS_LOGS.ERROR, err);
    });

    // if there are no logs in logs folder (both root and pub) then
    // return success response but with warning and corresponding message
    // validation must be performed before compress (e.g. in get-logs)
    const noRootLogs = get(logs, ['rootLogs', 'files'], []).length === 0;
    const noPubLogs = get(logs, ['pubLogs', 'files'], []).length === 0;

    if (noRootLogs && noPubLogs) {
      Logger.warn('COMPRESS_LOGS.SUCCESS: No files to compress');
      return sender.send(COMPRESS_LOGS.SUCCESS, {
        files: [],
        archive: null,
        message: 'No files to compress',
        success: true,
        warnings: 1,
      });
    }

    Logger.info('COMPRESS_LOGS started');

    // compress root files
    const rootLogsFiles = get(logs, ['rootLogs', 'files'], []);
    for (let i = 0; i < rootLogsFiles.length; i++) {
      const stream = fs.readFileSync(path.join(appLogsFolderPath, rootLogsFiles[i]));
      archive.append(stream, { name: rootLogsFiles[i] });
    }

    // compress pub folder files
    archive.directory(`${logs.pubLogs.path}/`, 'pub');
    archive.finalize((err) => {
      if (err) {
        Logger.error('COMPRESS_LOGS.ERROR: ' + stringifyError(err));
        return sender.send(COMPRESS_LOGS.ERROR, err);
      }
    });

    archive.pipe(output);
  });
};
