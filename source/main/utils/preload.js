const os = require('os');
const https = require('https');
const ipcRenderer = require('electron').ipcRenderer;
const remote = require('electron').remote;
const electronLog = require('electron-log');
const ElectronStore = require('electron-store');

const _process = process;
const _require = require;

process.once('loaded', () => {
  Object.assign(global, {
    process: {
      env: Object.assign({}, _process.env)
    },
    os: {
      platform: os.platform(),
    },
    ipcRenderer,
    https,
    electronLog,
    ElectronStore,
    Buffer,
    dialog: remote.dialog,
  });
  // Expose require for Spectron!
  if (_process.env.NODE_ENV === 'test') {
    global.require = _require;
  }
  // ESLint will warn about any use of eval(), even this one
  // eslint-disable-next-line
  global.eval = () => {
    throw new Error('This app does not support window.eval().');
  };
});
