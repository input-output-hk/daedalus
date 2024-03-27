'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const Store_1 = __importDefault(require('./lib/Store'));
// TODO: refactor all parts that rely on this to ipc channels!
// @ts-ignore ts-migrate(2339) FIXME: Property 'ipcRenderer' does not exist on type 'typ... Remove this comment to see the full error message
const { ipcRenderer } = global;
class WindowStore extends Store_1.default {
  _isTest = false;
  setup() {
    this.actions.window.resizeWindow.listen(this._resizeWindow);
    this.actions.window.closeWindow.listen(this.closeWindow);
    this.actions.app.initAppEnvironment.listen(() => {});
  }
  closeWindow = () => {
    // TODO: refactor to ipc channel
    ipcRenderer.send('close-window');
  };
  // PRIVATE
  _onGetAppEnvironmentSuccess = (0, mobx_1.action)((event, { isTest }) => {
    this._isTest = isTest;
  });
  _resizeWindow = ({ width, height }) => {
    // TODO: refactor to ipc channel
    ipcRenderer.send('resize-window', {
      width,
      height,
      animate: !this._isTest,
    });
  };
}
exports.default = WindowStore;
//# sourceMappingURL=WindowStore.js.map
