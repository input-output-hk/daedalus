import os from 'os';
import _https from 'https';
import _http from 'http';
import { ipcRenderer } from 'electron';
import electronLog from 'electron-log-daedalus';
import EventEmitter from 'events';
import { environment } from './environment';
import { buildLabel, legacyStateDir, isFlight, smashUrl } from './config';

const _process = process;
// Increase maximum event listeners to avoid IPC channel stalling
// (2/2) this line increases the limit for the renderer process
EventEmitter.defaultMaxListeners = 100; // Default: 10

process.once('loaded', () => {
  Object.assign(global, {
    environment,
    buildLabel,
    https: {
      Agent: _https.Agent,
      // @ts-ignore ts-migrate(2556) FIXME: Expected 1-3 arguments, but got 0 or more.
      request: (...args) => _https.request(...args),
    },
    http: {
      // @ts-ignore ts-migrate(2556) FIXME: Expected 1-3 arguments, but got 0 or more.
      request: (...args) => _http.request(...args),
    },
    os: {
      platform: os.platform(),
    },
    ipcRenderer: {
      // @ts-ignore ts-migrate(2556) FIXME: Expected 2 arguments, but got 0 or more.
      on: (...args) => ipcRenderer.on(...args),
      // @ts-ignore ts-migrate(2556) FIXME: Expected 2 arguments, but got 0 or more.
      once: (...args) => ipcRenderer.once(...args),
      // @ts-ignore ts-migrate(2557) FIXME: Expected at least 1 arguments, but got 0 or more.
      send: (...args) => ipcRenderer.send(...args),
      // @ts-ignore ts-migrate(2556) FIXME: Expected 2 arguments, but got 0 or more.
      removeListener: (...args) => ipcRenderer.removeListener(...args),
      // @ts-ignore ts-migrate(2556) FIXME: Expected 1 arguments, but got 0 or more.
      removeAllListeners: (...args) => ipcRenderer.removeAllListeners(...args),
    },
    electronLog: {
      debug: (...args) => electronLog.debug(...args),
      info: (...args) => electronLog.info(...args),
      error: (...args) => electronLog.error(...args),
      warn: (...args) => electronLog.warn(...args),
    },
    isFlight,
    legacyStateDir,
    smashUrl,
  });

  // Expose require for Spectron!
  if (_process.env.NODE_ENV === 'test') {
    // @ts-ignore
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
      (event) => {
        const targetIsSelectable =
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'EventTarget' is not assignable t... Remove this comment to see the full error message
          getComputedStyle(event.target).userSelect === 'text';
        // @ts-ignore ts-migrate(2339) FIXME: Property 'nodeName' does not exist on type 'EventT... Remove this comment to see the full error message
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
