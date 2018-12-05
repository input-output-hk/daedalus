// @flow
import os from 'os';
import _https from 'https';
import {
  ipcRenderer as _ipcRenderer,
  remote as _remote
} from 'electron';
import _electronLog from 'electron-log';
import ElectronStore from 'electron-store';
import { environment } from './environment';

const _process = process;
const _electronStore = new ElectronStore();

process.once('loaded', () => {
  Object.assign(global, {
    Buffer,
    dialog: {
      showSaveDialog: (...args) => _remote.dialog.showSaveDialog(...args),
    },
    electronLog: {
      debug: (...args) => _electronLog.debug(...args),
      info: (...args) => _electronLog.info(...args),
      error: (...args) => _electronLog.error(...args),
      warn: (...args) => _electronLog.warn(...args),
    },
    electronStore: {
      get: (...args) => _electronStore.get(...args),
      set: (...args) => _electronStore.set(...args),
      delete: (...args) => _electronStore.delete(...args),
    },
    environment,
    https: {
      request: (...args) => _https.request(...args),
    },
    ipcRenderer: {
      on: (...args) => _ipcRenderer.on(...args),
      once: (...args) => _ipcRenderer.once(...args),
      send: (...args) => _ipcRenderer.send(...args),
      removeListener: (...args) => _ipcRenderer.removeListener(...args),
      removeAllListeners: (...args) => _ipcRenderer.removeAllListeners(...args),
    },
    os: {
      platform: os.platform(),
    },
  });
  // Expose require for Spectron!
  if (_process.env.NODE_ENV === 'test') {
    // $FlowFixMe
    global.spectronRequre = __non_webpack_require__; // eslint-disable-line
  }
  // ESLint will warn about any use of eval(), even this one
  // eslint-disable-next-line
  global.eval = () => {
    throw new Error('This app does not support window.eval().');
  };
});
