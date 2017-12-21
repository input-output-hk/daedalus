import { ipcMain } from 'electron';
import fs from 'fs';
import archiver from 'archiver';
import Log from 'electron-log';
import path from 'path';
import getRuntimeFolderPath from '../lib/getRuntimeFolderPath';

const APP_NAME = 'Daedalus';
const CHANNEL_NAME = 'compress-logs';

export const COMPRESS_LOGS = {
  REQUEST: CHANNEL_NAME,
  SUCCESS: `${CHANNEL_NAME}-success`,
};

export default () => {
  ipcMain.on(COMPRESS_LOGS.REQUEST, (event, logs) => {
    const sender = event.sender;
    const runtimeFolderPath = getRuntimeFolderPath(process.platform, process.env, APP_NAME);
    const destination = path.join(runtimeFolderPath, 'Logs');
    const compressedFileName = 'logs.zip';

    const output = fs.createWriteStream(path.join(destination, compressedFileName));
    const archive = archiver('zip', {
      zlib: { level: 9 } // Sets the compression level.
    });

    output.on('close', () => {
      Log.info(archive.pointer() + ' total bytes');
      Log.info('archiver has been finalized and the output file descriptor has closed.');
    });

    output.on('end', () => {
      Log.info('Data has been drained');
    });

    archive.on('error', (err) => {
      Log.error('error: ', err);
      throw err;
    });

    archive.pipe(output);

    // if there are no logs in logs folder (both root and pub) then
    // return success response but with warning and corresponding message
    // validation must be performed before compress ( e.g. in get-logs)
    if (!logs.rootLogs.files.length && !logs.pubLogs.files.length) {
      Log.warn('No files to compress, proceed with empty path');

      return sender.send(COMPRESS_LOGS.SUCCESS, {
        path: null,
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
    archive.finalize((err, bytes) => {
      if (err) {
        Log.error('error: ', err);
        throw err;
      }
      Log.info(bytes + ' total bytes');
    });

    // response on success compression
    return sender.send(COMPRESS_LOGS.SUCCESS, {
      path: `${destination}/${compressedFileName}`,
      archive,
      message: 'Logs compressed successfully',
      success: true,
      warnings: 0,
    });
  });
};
