// @flow
import { localStorageBridge } from '../../../api/utils/localStorageBridge';
import { STORAGE_KEYS as storageKeys } from '../../../../../common/config/electron-store.config';

export class DiscreetModeApi {
  getDiscreetModeSettings = async (): Promise<boolean> => {
    const localStorage = await localStorageBridge();
    return localStorage.get(storageKeys.DISCREET_MODE_ENABLED, false);
  };

  setDiscreetModeSettings = async (enabled: boolean): Promise<void> => {
    const localStorage = await localStorageBridge();
    localStorage.set(storageKeys.DISCREET_MODE_ENABLED, enabled);
  };

  unsetDiscreetModeSettings = async (): Promise<void> => {
    const localStorage = await localStorageBridge();
    localStorage.unset(storageKeys.DISCREET_MODE_ENABLED);
  };
}
