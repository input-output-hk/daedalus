import { ipcRenderer as _ipcRenderer } from 'electron';
import './app/index';

Object.assign(global, {
  ipcRenderer: {
    on: (...args) => _ipcRenderer.on(...args),
    once: (...args) => _ipcRenderer.once(...args),
    send: (...args) => _ipcRenderer.send(...args),
    removeListener: (...args) => _ipcRenderer.removeListener(...args),
    removeAllListeners: (...args) => _ipcRenderer.removeAllListeners(...args),
  },
});
