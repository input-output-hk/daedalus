// @flow
import os from 'os';
import _https from 'https';
import _http from 'http';
import { ipcRenderer as _ipcRenderer } from 'electron';
import type { LoggingType } from '../common/types/logging.types';
import { electronLogChannel } from '../renderer/app/ipc/logger-channel';
import { environment } from './environment';
import { buildLabel, nodeImplementation, isFlight } from './config';

const _process = process;
const _isIncentivizedTestnet = nodeImplementation === 'jormungandr';

const logFunc = async (type: LoggingType, ...args) => {
  if (!args.length) return;
  const message = args[0];
  const options = args.length > 1 ? args[1] : null;

  await electronLogChannel.send({ type, message, options });
};

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
      on: (...args) => _ipcRenderer.on(...args),
      once: (...args) => _ipcRenderer.once(...args),
      send: (...args) => _ipcRenderer.send(...args),
      removeListener: (...args) => _ipcRenderer.removeListener(...args),
      removeAllListeners: (...args) => _ipcRenderer.removeAllListeners(...args),
    },
    electronLog: {
      debug: (...args) => logFunc('debug', ...args),
      info: (...args) => logFunc('info', ...args),
      error: (...args) => logFunc('error', ...args),
      warn: (...args) => logFunc('warn', ...args),
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
