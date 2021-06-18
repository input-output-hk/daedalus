// @flow
import { app } from 'electron';
import log from 'electron-log-daedalus';

export const safeExitWithCode = (exitCode: number) => {
  const { file } = log.transports;
  // Prevent electron-log from writing to stream
  file.level = false;
  // Flush the stream to the log file and exit afterwards.
  // https://nodejs.org/api/stream.html#stream_writable_end_chunk_encoding_callback
  file.stream.end('', 'utf8', () => {
    app.releaseSingleInstanceLock();
    // Acording the official doc, relaunch should be called before exit
    // https://www.electronjs.org/docs/api/app#apprelaunchoptions
    app.relaunch();
    app.exit(exitCode);
  });
};
