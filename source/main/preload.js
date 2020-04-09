// @flow
import os from 'os';
import _https from 'https';
import _http from 'http';
import { ipcRenderer } from 'electron';
import electronLog from 'electron-log-daedalus';
import { environment } from './environment';
import { buildLabel, nodeImplementation, isFlight } from './config';

const _process = process;
const _isIncentivizedTestnet = nodeImplementation === 'jormungandr';

process.once('loaded', () => {
  Object.assign(global, {
    environment,
    buildLabel,
    https: {
      request: (...args) => _https.request(...args),
    },
    http: {
      request: (...args) => _http.request(...args),
    },
    os: {
      platform: os.platform(),
    },
    ipcRenderer: {
      on: (...args) => ipcRenderer.on(...args),
      once: (...args) => ipcRenderer.once(...args),
      send: (...args) => ipcRenderer.send(...args),
      removeListener: (...args) => ipcRenderer.removeListener(...args),
      removeAllListeners: (...args) => ipcRenderer.removeAllListeners(...args),
    },
    electronLog: {
      debug: (...args) => electronLog.debug(...args),
      info: (...args) => electronLog.info(...args),
      error: (...args) => electronLog.error(...args),
      warn: (...args) => electronLog.warn(...args),
    },
    isIncentivizedTestnet: _isIncentivizedTestnet,
    isFlight,
  });
  // Expose require for Spectron!
  if (_process.env.NODE_ENV === 'test') {
    // $FlowFixMe
    global.spectronRequire = __non_webpack_require__; // eslint-disable-line
  }
  // ESLint will warn about any use of eval(), even this one
  // eslint-disable-next-line no-eval
  global.eval = () => {
    throw new Error('This app does not support window.eval().');
  };

  // Prevent context-menu for elements which don't support copy&paste options
  if (_process.env.NODE_ENV === 'production') {
    // elements that can be copied using the context menu (right click),
    // must have a css property of user-select: 'text' or be an input element
    global.document.addEventListener(
      'contextmenu',
      event => {
        const targetIsSelectable =
          getComputedStyle(event.target).userSelect === 'text';
        const targetIsInput = event.target.nodeName === 'INPUT';

        if (targetIsSelectable || targetIsInput) {
          return true;
        }

        return event.preventDefault();
      },
      false
    );
  }
});
