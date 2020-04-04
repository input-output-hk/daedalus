import { dialog as _dialog, BrowserWindow as _browserWindow } from 'electron';
import _electronLog from 'electron-log-daedalus';
import ElectronStore from 'electron-store';

const _electronStore = new ElectronStore({ name: 'config' });

process.once('loaded', () => {
  Object.assign(global, {
    dialog: {
      showOpenDialog: (...args) =>
        _dialog.showOpenDialog(_browserWindow.getFocusedWindow(), ...args),
      showSaveDialog: (...args) =>
        _dialog.showSaveDialog(_browserWindow.getFocusedWindow(), ...args),
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
  });
});
