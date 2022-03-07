import mkdirp from 'mkdirp';
import fs from 'fs';

export default (filepath: string) => {
  let stats;

  try {
    stats = fs.lstatSync(filepath);
  } catch (e) {
    try {
      mkdirp.sync(filepath);
      stats = fs.lstatSync(filepath);
    } catch (error) {
      process.exit(1);
    }
  }

  if (!stats || !stats.isDirectory()) {
    process.exit(1);
  }
};
