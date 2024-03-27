'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const os_1 = __importDefault(require('os'));
const https_1 = __importDefault(require('https'));
const http_1 = __importDefault(require('http'));
const electron_1 = require('electron');
const electron_log_daedalus_1 = __importDefault(
  require('electron-log-daedalus')
);
const events_1 = __importDefault(require('events'));
const environment_1 = require('./environment');
const config_1 = require('./config');
const _process = process;
// Increase maximum event listeners to avoid IPC channel stalling
// (2/2) this line increases the limit for the renderer process
events_1.default.defaultMaxListeners = 100; // Default: 10
process.once('loaded', () => {
  Object.assign(global, {
    environment: environment_1.environment,
    buildLabel: config_1.buildLabel,
    https: {
      Agent: https_1.default.Agent,
      // @ts-ignore ts-migrate(2556) FIXME: Expected 1-3 arguments, but got 0 or more.
      request: (...args) => https_1.default.request(...args),
    },
    http: {
      // @ts-ignore ts-migrate(2556) FIXME: Expected 1-3 arguments, but got 0 or more.
      request: (...args) => http_1.default.request(...args),
    },
    os: {
      platform: os_1.default.platform(),
    },
    ipcRenderer: electron_1.ipcRenderer,
    electronLog: {
      debug: (...args) => electron_log_daedalus_1.default.debug(...args),
      info: (...args) => electron_log_daedalus_1.default.info(...args),
      error: (...args) => electron_log_daedalus_1.default.error(...args),
      warn: (...args) => electron_log_daedalus_1.default.warn(...args),
    },
    isFlight: config_1.isFlight,
    legacyStateDir: config_1.legacyStateDir,
    smashUrl: config_1.smashUrl,
  });
  // ESLint will warn about any use of eval(), even this one
  if (environment_1.environment.isProduction) {
    // eslint-disable-next-line no-eval
    global.eval = () => {
      throw new Error('This app does not support window.eval().');
    };
  }
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
//# sourceMappingURL=preload.js.map
