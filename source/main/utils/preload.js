const _os = require('os');
const _ipcRenderer = require('electron').ipcRenderer;
const _electronLog = require('electron-log');
const _electronStore = require('electron-store');

const _process = process;
const _require = require;

process.once('loaded', () => {
  global.process = {
    env: Object.assign({}, _process.env)
  };
  global.os = {
    platform: _os.platform(),
  };
  global.ipcRenderer = _ipcRenderer;
  global.electronLog = _electronLog;
  global.ElectronStore = _electronStore;
  global.Buffer = Buffer;
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
