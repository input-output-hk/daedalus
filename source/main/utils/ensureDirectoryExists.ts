import mkdirp from 'mkdirp';
import fs from 'fs';
import { logger } from './logging';

export default (filepath: string) => {
  let stats;

  // statSync, not lstatSync: a state or logs directory is allowed to be a
  // symlink to a directory elsewhere (e.g. another disk), and lstat would
  // report the link itself, failing the isDirectory() check below.
  try {
    stats = fs.statSync(filepath);
  } catch (e) {
    try {
      mkdirp.sync(filepath);
      stats = fs.statSync(filepath);
    } catch (error) {
      logger.error('ensureDirectoryExists: could not create directory', {
        filepath,
        error,
      });
      process.exit(1);
    }
  }

  if (!stats || !stats.isDirectory()) {
    logger.error('ensureDirectoryExists: path is not a directory', {
      filepath,
    });
    process.exit(1);
  }
};
