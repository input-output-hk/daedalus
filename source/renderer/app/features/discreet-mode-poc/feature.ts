import { useReducer } from 'react';
import { useCommandEffect, useStateEffect } from 'react-states';
import { createContainer } from 'react-tracked';
import { STORAGE_KEYS as storageKeys } from '../../../../common/config/electron-store.config';
import { useEnvironment } from '../../environment';
import { discreetModeReducer } from './state';

export const DiscreetMode = createContainer(() => {
  const environment = useEnvironment();
  const [context, dispatch] = useReducer(discreetModeReducer, {
    state: 'LOADING',
  });
  // Fetch "open in discreet mode" setting from storage when in "LOADING" mode
  useStateEffect(context, 'LOADING', () => {
    environment.storage
      .get(storageKeys.DISCREET_MODE_ENABLED, false)
      .then((setting: string) => {
        // Enable or disable the toggle & setting states based on the saved setting
        dispatch({ type: 'LOADED', savedUserSetting: setting === 'true' });
      });
  });
  // Save "open in discreet mode" setting changes
  useCommandEffect(context, 'SAVE_OPEN_IN_DISCREET_MODE', (command) => {
    environment.storage.set(storageKeys.DISCREET_MODE_ENABLED, command.value);
  });
  return [context, dispatch];
});
