// @flow
import { app } from 'electron';
import log from 'electron-log-daedalus';
import { isBlankScreenFixActive } from '../environment';

export const safeExitWithCode = (exitCode: number = 0) => {
  const { file } = log.transports;
  // Prevent electron-log from writing to stream
  file.level = false;
  // Flush the stream to the log file and exit afterwards.
  // https://nodejs.org/api/stream.html#stream_writable_end_chunk_encoding_callback
  file.stream.end('', 'utf8', () => {
    app.releaseSingleInstanceLock();
    if (exitCode === 21 && !isBlankScreenFixActive()) {
      app.relaunch({
        args: process.argv.slice(1).concat(['--blankScreenFixActive']),
      });
      app.exit(0);
    } else if (exitCode === 22) {
      app.relaunch({
        args: process.argv.slice(1).concat(['--normal']),
      });
      app.exit(0);
    } else {
      app.exit(exitCode);
    }
  });
};
