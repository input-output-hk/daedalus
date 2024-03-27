'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.relaunch = exports.safeExitWithCode = void 0;
const electron_1 = require('electron');
const electron_log_daedalus_1 = __importDefault(
  require('electron-log-daedalus')
);
const safeExitWithCode = (exitCode = 0) => {
  const { file } = electron_log_daedalus_1.default.transports;
  // Prevent electron-log from writing to stream
  file.level = false;
  // Flush the stream to the log file and exit afterwards.
  // https://nodejs.org/api/stream.html#stream_writable_end_chunk_encoding_callback
  file.stream.end('', 'utf8', () => {
    electron_1.app.releaseSingleInstanceLock();
    electron_1.app.exit(exitCode);
  });
};
exports.safeExitWithCode = safeExitWithCode;
const relaunch = () => {
  const { file } = electron_log_daedalus_1.default.transports;
  // Prevent electron-log from writing to stream
  file.level = false;
  // Flush the stream to the log file and exit afterwards.
  // https://nodejs.org/api/stream.html#stream_writable_end_chunk_encoding_callback
  file.stream.end('', 'utf8', () => {
    electron_1.app.releaseSingleInstanceLock();
    electron_1.app.relaunch({
      args: process.argv.slice(1).concat(['--relaunch']),
    });
    electron_1.app.exit(0);
  });
};
exports.relaunch = relaunch;
//# sourceMappingURL=safeExitWithCode.js.map
