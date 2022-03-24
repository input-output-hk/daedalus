// @flow
import { app } from 'electron';
import log from 'electron-log-daedalus';

export const safeExitWithCode = (exitCode: number = 0) => {
  const { file } = log.transports;
  // Prevent electron-log from writing to stream
  file.level = false;

  // XXX: <michalrus> I’m really not sure if this is right, pls verify:

  if (typeof file.stream !== 'undefined') {
    // If we’re in the primary (single) instance:

    // Flush the stream to the log file and exit afterwards.
    // https://nodejs.org/api/stream.html#stream_writable_end_chunk_encoding_callback
    file.stream.end('', 'utf8', () => {
      app.releaseSingleInstanceLock();
      app.exit(exitCode);
    });
  } else {
    // If this is a `second-instance`, which doesn’t get logging:
    app.exit(exitCode);
  }
};
