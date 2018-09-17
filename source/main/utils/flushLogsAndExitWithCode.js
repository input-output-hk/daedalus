// @flow
import { app } from 'electron';
import log from 'electron-log';

export const flushLogsAndExitWithCode = (exitCode: number) => {
  const { stream } = log.transports.file;
  // https://nodejs.org/api/stream.html#stream_writable_end_chunk_encoding_callback
  stream.end(`========== Flushing logs and exiting with code ${exitCode} ===========`, 'utf8', () => {
    app.exit(exitCode);
  });
};
