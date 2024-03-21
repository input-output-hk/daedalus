'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.PersistentTimeMachine = void 0;
const timemachine_1 = __importDefault(require('timemachine'));
// https://github.com/schickling/timemachine/issues/8
timemachine_1.default.reset();
const TIME_MACHINE_SESSION_STORAGE_KEY = 'time_machine_date';
class PersistentTimeMachine {
  _isInitialized = false;
  init() {
    const dateString = sessionStorage.getItem(TIME_MACHINE_SESSION_STORAGE_KEY);
    if (dateString !== null) {
      timemachine_1.default.config({
        dateString,
      });
    }
    this._isInitialized = true;
  }
  enable(dateString) {
    this._ensureIsInitialized();
    timemachine_1.default.config({
      dateString,
    });
    sessionStorage.setItem(TIME_MACHINE_SESSION_STORAGE_KEY, dateString);
    window.location.reload();
  }
  disable() {
    this._ensureIsInitialized();
    timemachine_1.default.reset();
    sessionStorage.removeItem(TIME_MACHINE_SESSION_STORAGE_KEY);
    window.location.reload();
  }
  _ensureIsInitialized() {
    if (!this._isInitialized) {
      throw new Error('Time machine must be initialized before use');
    }
  }
}
exports.PersistentTimeMachine = PersistentTimeMachine;
//# sourceMappingURL=PersistentTimeMachine.js.map
