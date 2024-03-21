'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.DiscreetModeApi = void 0;
const electron_store_config_1 = require('../../../../../common/config/electron-store.config');
class DiscreetModeApi {
  localStorage;
  constructor(localStorage) {
    this.localStorage = localStorage;
  }
  getDiscreetModeSettings = () =>
    this.localStorage.get(
      electron_store_config_1.STORAGE_KEYS.DISCREET_MODE_ENABLED,
      false
    );
  setDiscreetModeSettings = (enabled) =>
    this.localStorage.set(
      electron_store_config_1.STORAGE_KEYS.DISCREET_MODE_ENABLED,
      enabled
    );
  unsetDiscreetModeSettings = async () =>
    this.localStorage.unset(
      electron_store_config_1.STORAGE_KEYS.DISCREET_MODE_ENABLED
    );
}
exports.DiscreetModeApi = DiscreetModeApi;
//# sourceMappingURL=index.js.map
