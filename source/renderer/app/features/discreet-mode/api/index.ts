import { STORAGE_KEYS as storageKeys } from '../../../../../common/config/electron-store.config';
import type { LocalStorageApi } from '../../local-storage';

export class DiscreetModeApi {
  localStorage: LocalStorageApi;

  constructor(localStorage: LocalStorageApi) {
    this.localStorage = localStorage;
  }

  getDiscreetModeSettings = () =>
    this.localStorage.get(storageKeys.DISCREET_MODE_ENABLED, false);
  setDiscreetModeSettings = (enabled: boolean) =>
    this.localStorage.set(storageKeys.DISCREET_MODE_ENABLED, enabled);
  unsetDiscreetModeSettings = async () =>
    this.localStorage.unset(storageKeys.DISCREET_MODE_ENABLED);
}
