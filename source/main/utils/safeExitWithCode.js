// @flow
import { app } from 'electron';
import log from 'electron-log';
import { releaseDaedalusInstanceLock } from './app-instance-lock';

export const safeExitWithCode = (exitCode: number) => {
  const { file } = log.transports;
  // Prevent electron-log from writing to stream
  file.level = false;
  // Flush the stream to the log file and exit afterwards.
  // https://nodejs.org/api/stream.html#stream_writable_end_chunk_encoding_callback
  file.stream.end(
    `========== Flushing logs and exiting with code ${exitCode} ===========`,
    'utf8',
    () => {
      releaseDaedalusInstanceLock();
      app.exit(exitCode);
    }
  );
};
