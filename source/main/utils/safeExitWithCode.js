// @flow
import { app } from 'electron';
import log from 'electron-log-daedalus';

export const safeExitWithCode = (exitCode: number = 0) => {
  const { file } = log.transports;
  // Prevent electron-log from writing to stream
  file.level = false;
  // Flush the stream to the log file and exit afterwards.
  // https://nodejs.org/api/stream.html#stream_writable_end_chunk_encoding_callback
  file.stream.end('', 'utf8', () => {
    app.releaseSingleInstanceLock();
    const args =
      exitCode === 21
        ? { args: process.argv.slice(1).concat(['--safe-mode']) }
        : {};
    if (exitCode !== 0) app.relaunch(args);
    app.exit(exitCode);
  });
};
