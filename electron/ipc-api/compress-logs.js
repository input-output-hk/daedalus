import { ipcMain } from 'electron';
import fs from 'fs';
import archiver from 'archiver';
import Log from 'electron-log';
import path from 'path';
import splitFile from 'split-file';
import getRuntimeFolderPath from '../lib/getRuntimeFolderPath';

const APP_NAME = 'Daedalus';
const CHANNEL_NAME = 'compress-logs';

export const COMPRESS_LOGS = {
  REQUEST: CHANNEL_NAME,
  SUCCESS: `${CHANNEL_NAME}-success`,
  MAX_SIZE: 7340032, // 7mb in bytes
};

export default () => {
  ipcMain.on(COMPRESS_LOGS.REQUEST, (event, logs) => {
    const sender = event.sender;
    const runtimeFolderPath = getRuntimeFolderPath(process.platform, process.env, APP_NAME);
    const destination = path.join(runtimeFolderPath, 'Logs');
    const compressedFileName = 'logs.zip';

    const outputPath = path.join(destination, compressedFileName);
    const output = fs.createWriteStream(outputPath);
    const archive = archiver('zip', {
      zlib: { level: 9 } // Sets the compression level.
    });

    output.on('close', () => {
      const archiveSize = archive.pointer();

      if (archiveSize > COMPRESS_LOGS.MAX_SIZE) {
        // split archive - one part can have max 7mb
        splitFile.splitFileBySize(outputPath, COMPRESS_LOGS.MAX_SIZE)
          .then((files) => {
            Log.info('Split files');
            // response on success compression
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
            Log.error('error: ', err);
            return sender.send(COMPRESS_LOGS.ERROR);
          });
      } else {
        Log.info('archiver has been finalized and the output file descriptor has closed.');
        // response on success compression
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
      Log.error('error: ', err);
      return sender.send(COMPRESS_LOGS.ERROR);
    });

    // if there are no logs in logs folder (both root and pub) then
    // return success response but with warning and corresponding message
    // validation must be performed before compress ( e.g. in get-logs)
    const noRootLogs = !logs.rootLogs || !logs.rootLogs.files || !logs.rootLogs.files.length;
    const noPubLogs = !logs.pubLogs || !logs.pubLogs.files || !logs.pubLogs.files.length;
    if (noRootLogs && noPubLogs) {
      Log.warn('No files to compress, proceed with empty path');
      return sender.send(COMPRESS_LOGS.SUCCESS, {
        files: [],
        archive: null,
        message: 'No files to compress',
        success: true,
        warnings: 1,
      });
    }

    Log.info('Compressing...');

    // compress root files
    for (let i = 0; i < logs.rootLogs.files.length; i++) {
      const stream = fs.readFileSync(path.join(destination, logs.rootLogs.files[i]));
      archive.append(stream, { name: logs.rootLogs.files[i] });
    }

    // compress folder with files (e.g. pub folder)
    archive.directory(`${logs.pubLogs.path}/`, 'pub');
    archive.finalize((err) => {
      if (err) {
        Log.error('error: ', err);
        return sender.send(COMPRESS_LOGS.ERROR);
      }
    });

    archive.pipe(output);
  });
};
